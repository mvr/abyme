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

  describe "chunks" $ do
    let s11 = monomino
        s12 = Shape {_shapePosition = V2 0 1, _shapePolyomino = Polyomino {_polyominoSquares = [V2 0 0]}}
        s21 = monomino
        r1 = Region {_regionId = RegionId {getRegionId = 1}, _regionParentId = RegionId {getRegionId = 2}, _regionPosition = V2 0 0, _regionShapes = [s11, s12]}
        r2 = Region {_regionId = RegionId {getRegionId = 2}, _regionParentId = RegionId {getRegionId = 1}, _regionPosition = V2 0 0, _regionShapes = [s21]}
        u = Universe {_universeRegions = M.fromList [ (RegionId {getRegionId = 1}, r1),
                                                      (RegionId {getRegionId = 2}, r2) ]}
    it "correctly identifies the chunks for r1" $
      regionChunks u r1 == [Chunk r1 [s11], Chunk r1 [s12]]
    it "correctly finds the chunk for s12" $
      findChunk u (Piece r1 s12) == Chunk r1 [s12]

  describe "splitting" $ do
    prop "splitting then unsplitting is identity" $ \u -> do
      s <- randomPiece u
      let c = findChunk u s
      let (r, u') = splitChunkIntoRegion u c
      return $ normaliseUniverse u === normaliseUniverse (fuseInhabitantRegions u' (regionParent u' r))
