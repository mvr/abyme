{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Regiony.Universe where

import Control.Lens hiding (contains)
import Data.List (delete, (\\))
import qualified Data.List.NonEmpty as NE (fromList)
import Data.Semigroup
import qualified Data.Map.Strict as M
import Linear

import Abyme.Util
import Abyme.Polyomino

-- Some nomenclature:
-- * There are Shapes that when next to each other form Regions.
-- * Some Shapes in a Region are actually locked together by shapes on a
--   lower level, these form Chunks
-- * A Piece is one Square in a Region
-- * Every Shape is made up of Squares, every Square lies on a Location
-- * Fringe is all the squares adjacent to something in a particular direction
--   Halo is all the squares adjacent to something in any direction

-- TODO: for efficiency:
-- * Keep track of which shapes are on the edge of their region?

newtype RegionId = RegionId { getRegionId :: Integer }
  deriving (Eq, Ord, Show, Enum)

data Shape = Shape
  {
    _shapePosition :: V2 Integer, -- on region
    _shapePolyomino :: Polyomino
  } deriving (Eq, Show, Ord)
makeLenses ''Shape

data Region = Region
  {
    _regionId :: RegionId,
    _regionParentId :: RegionId,
    _regionPosition :: V2 Integer, -- on parent
    _regionShapes :: [Shape] -- constituents -- TODO: NonEmpty
  } deriving (Eq, Show)
makeLenses ''Region

data Universe = Universe
  {
    _universeRegions :: M.Map RegionId Region
  } deriving (Eq, Show)
makeLenses ''Universe

-- Should be total
regionParent :: Universe -> Region -> Region
regionParent u c = fromJustOrDie "Region's parent doesn't exist" $ u ^. universeRegions . at (c ^. regionParentId)

regionEraseShape :: Shape -> Region -> Region
regionEraseShape s r = r & regionShapes %~ (delete s)

regionEraseShapes :: [Shape] -> Region -> Region
regionEraseShapes ss r = r & regionShapes %~ (\\ ss)

instance Semigroup Shape where
  (Shape p ss) <> (Shape p' ss') = Shape p $ ss <> polyOffset ss' p'

regionCompositePoly :: Region -> Polyomino
regionCompositePoly r = sconcat $ NE.fromList $ fmap shapePolyWithOffset $ r ^. regionShapes

-- It always should be
regionIsConnected :: Region -> Bool
regionIsConnected = polyIsConnected . regionCompositePoly

newRegionId :: Universe -> RegionId
newRegionId (Universe rs) = RegionId $ 1 + (getRegionId $ fst $ M.findMax rs)

-- Position relative to constituent region origin
shapeContains :: Shape -> V2 Integer -> Bool
shapeContains s p = polyContainsPoint (s^.shapePolyomino) (p - s^.shapePosition)

-- Position relative to parent region origin
shapeInhabits :: Region -> Shape -> V2 Integer -> Bool
shapeInhabits c s p = shapeContains s (p - c^.regionPosition)

shapePolyWithOffset :: Shape -> Polyomino
shapePolyWithOffset s = polyOffset (s ^. shapePolyomino) (s ^. shapePosition)

shapeIntersects :: Shape -> Shape -> Bool
shapeIntersects s s' = polyIntersects (shapePolyWithOffset s) (shapePolyWithOffset s')

remapIds :: [(RegionId, RegionId)] -> Universe -> Universe
remapIds assoc m = m & universeRegions . traverse %~ fixRegion
                     & universeRegions %~ M.mapKeys forceRemap
  where fixRegion r = r & regionId %~ forceRemap & regionParentId %~ forceRemap
        forceRemap = fromJustOrDie "Missing a remapping" . flip lookup assoc
