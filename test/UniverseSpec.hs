module UniverseSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

import Control.Lens
import qualified Data.Map.Strict as M
import Linear

import Abyme.Polyomino
import Abyme.Universe
import Abyme.Universe.Generate
import Abyme.Universe.Normalise
import Abyme.Addressing
import Abyme.Chunk

spec :: Spec
spec = do
  describe "addressing" $ do
    prop "square occupies its location" $ \u -> do
      s <- randomSquare u
      return $ Just s === inhabitant u (squareLocation u s)

  -- TODO: order is irrelevant
  describe "chunking" $ do
    describe "minimal example" $ do
      it "finds the chunk for r1" $
        let r1 = minimal ^?! universeRegions . ix (RegionId 1)
            p  = Piece r1 monomino
        in
          pieceChunk minimal p === Chunk r1 [monomino] M.empty

    describe "simple example" $ do
      let s11 = monomino
          s12 = monomino {_shapePosition = V2 0 1}
          s21 = monomino
          r1 = Region {_regionId = RegionId {getRegionId = 1}, _regionParentId = RegionId {getRegionId = 2}, _regionPosition = V2 0 0, _regionShapes = [s11, s12]}
          r2 = Region {_regionId = RegionId {getRegionId = 2}, _regionParentId = RegionId {getRegionId = 1}, _regionPosition = V2 0 0, _regionShapes = [s21]}
          u = Universe {_universeRegions = M.fromList [ (RegionId {getRegionId = 1}, r1),
                                                        (RegionId {getRegionId = 2}, r2) ]}
      it "finds the chunk for s11" $
        pieceChunk u (Piece r1 s11) === Chunk r1 [s11] (M.fromList [ (RegionId {getRegionId = 1}, [[s11], [s12]]) ])
      it "finds the chunk for s21" $
        pieceChunk u (Piece r2 s21) === Chunk r2 [s21] M.empty

    describe "harder example" $ do
      let s11 = Shape {_shapePosition = V2 0 0, _shapePolyomino = Polyomino {_polyominoSquares = [V2 0 0,V2 0 1,V2 0 2]}}
          s21 = monomino
          s22 = monomino {_shapePosition = V2 0 1}
          r1  = Region {_regionId = RegionId {getRegionId = 1}, _regionParentId = RegionId {getRegionId = 2}, _regionPosition = V2 0 0, _regionShapes = [s11]}
          r2  = Region {_regionId = RegionId {getRegionId = 2}, _regionParentId = RegionId {getRegionId = 1}, _regionPosition = V2 0 0, _regionShapes = [s21, s22]}
          u   = Universe {_universeRegions = M.fromList [(RegionId {getRegionId = 1}, r1), (RegionId {getRegionId = 2}, r2)]}
      it "finds the chunk for s11" $
        pieceChunk u (Piece r1 s11) === Chunk r1 [s11] M.empty

      it "explores s21 correctly" $
        explorePiece u (Piece r2 s21) === M.fromList [(RegionId 1, [(1, s11)]), (RegionId 2, [(0, s22), (0,s21)]) ]
      it "finds the chunk for s21" $
        pieceChunk u (Piece r2 s21) === Chunk r2 [s22, s21] M.empty

      it "finds the chunk for s22" $
        pieceChunk u (Piece r2 s22) === Chunk r2 [s21, s22] M.empty


  describe "splitting" $ do
    let checkSplitting u p = normaliseUniverse u === normaliseUniverse u''
          where c = pieceChunk u p
                (r, u') = isolateChunk u c
                u'' = fuseInhabitantRegions u' (regionParent u' r)

    it "works for tricky example 1" $
      let r1 = Region {_regionId = RegionId {getRegionId = 1}, _regionParentId = RegionId {getRegionId = 2}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino,
                          monomino {_shapePosition = V2 1 0},
                          monomino {_shapePosition = V2 2 0} ] }
          r2 = Region {_regionId = RegionId {getRegionId = 2}, _regionParentId = RegionId {getRegionId = 1}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino,
                          monomino {_shapePosition = V2 1 0} ] }

          u = Universe {_universeRegions = M.fromList [ (RegionId {getRegionId = 1}, r1), (RegionId {getRegionId = 2}, r2)]}
          p = Piece r2 monomino
      in checkSplitting u p

    it "works for tricky example 2" $
      let r1 = Region {_regionId = RegionId {getRegionId = 1}, _regionParentId = RegionId {getRegionId = 2}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino,
                          monomino { _shapePosition = V2 1 0 } ]}
          r2 = Region {_regionId = RegionId {getRegionId = 2}, _regionParentId = RegionId {getRegionId = 1}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino,
                          monomino { _shapePosition = V2 1 0 },
                          monomino { _shapePosition = V2 2 0 } ]}
          r3 = Region {_regionId = RegionId {getRegionId = 3}, _regionParentId = RegionId {getRegionId = 2}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino { _shapePosition = V2 3 0 },
                          monomino { _shapePosition = V2 4 0 } ]}
          u = Universe {_universeRegions = M.fromList [
                           (RegionId {getRegionId = 1}, r1),
                           (RegionId {getRegionId = 2}, r2),
                           (RegionId {getRegionId = 3}, r3) ]}
          p = Piece r1 monomino
      in checkSplitting u p

    -- This demonstrates that we need to keep track of which shapes we see at the top level
    it "works for tricky example 3" $
      let r1 = Region {_regionId = RegionId {getRegionId = 1}, _regionParentId = RegionId {getRegionId = 2}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino,
                          monomino { _shapePosition = V2 1 0 },
                          monomino { _shapePosition = V2 2 0 } ]}
          r2 = Region {_regionId = RegionId {getRegionId = 2}, _regionParentId = RegionId {getRegionId = 1}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino,
                          monomino { _shapePosition = V2 1 0 },
                          Shape {_shapePosition = V2 2 0, _shapePolyomino = Polyomino {_polyominoSquares = [V2 0 0, V2 1 0]}},
                          monomino { _shapePosition = V2 4 0 } ]}
          u = Universe {_universeRegions = M.fromList [
                           (RegionId {getRegionId = 1}, r1),
                           (RegionId {getRegionId = 2}, r2) ]}
          p = Piece r2 monomino
      in checkSplitting u p

    it "works for tricky example 4" $
      let r1 = Region {_regionId = RegionId {getRegionId = 1}, _regionParentId = RegionId {getRegionId = 2}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino,
                          Shape {_shapePosition = V2 0 0, _shapePolyomino = Polyomino {_polyominoSquares = [V2 1 0,V2 2 0,V2 3 0,V2 4 0]}}]}

          r2 = Region {_regionId = RegionId {getRegionId = 2}, _regionParentId = RegionId {getRegionId = 1}, _regionPosition = V2 0 0, _regionShapes = [
                          monomino,
                          monomino {_shapePosition = V2 1 0 },
                          monomino {_shapePosition = V2 2 0 } ]}

          u = Universe {_universeRegions = M.fromList [ (RegionId {getRegionId = 1}, r1), (RegionId {getRegionId = 2}, r2)]}

          p = Piece r1 monomino
      in checkSplitting u p


    prop "splitting then unsplitting is identity" $ \u -> do
      p <- randomPiece u
      return $ checkSplitting u p
