{-# LANGUAGE RankNTypes #-}
module Abyme.Universe.Generator where

import qualified Data.Map.Strict as M
import Control.Lens
import Linear

import Abyme.Polyomino
import Abyme.Universe
import Abyme.Addressing
import Abyme.Chunk

monomino :: Shape
monomino = Shape (V2 0 0) (Polyomino [V2 0 0])

minimal :: Universe
minimal = Universe (M.fromList [(id1, region1), (id2, region2)])
  where id1 = RegionId 1
        id2 = RegionId 2
        region1 = Region id1 id2 (V2 0 0) [monomino]
        region2 = Region id2 id1 (V2 0 0) [monomino]

-- Needs a "fake square" off the edge of a Shape
growPiece :: Universe -> Square -> Universe
growPiece u (Square p l) = u & atPiece p . shapePolyomino . polyominoSquares %~ (l:)

locationPositionOnParent :: Location -> V2 Integer
locationPositionOnParent (Location (Square _ p) subp) = levelScale *^ p + subp

-- Assuming the square is uninhabited
growChild :: Universe -> Location -> Universe
growChild u l = fuseInhabitantRegions (u & universeRegions . at newId ?~ newRegion) locationRegion
  where newId = newRegionId u
        locationRegion = l ^. locationSquare . squarePiece . pieceRegion
        newRegion = Region newId (locationRegion ^. regionId) (locationPositionOnParent l) [monomino]
