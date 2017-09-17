{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Shapey.Universe where

import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.List (nub)
import           Data.Maybe (isJust, catMaybes)
import           Linear

import           Abyme.Util
import           Abyme.Direction
import           Abyme.Polyomino

newtype ShapeId = ShapeId { getShapeId :: Integer }
  deriving (Eq, Ord, Show, Enum)

data Shape = Shape
  {
    _shapeId :: ShapeId,
    _shapeParentIds :: [(ShapeId, V2 Integer)],
    _shapePolyomino :: Polyomino
  } deriving (Eq, Show, Ord)
makeLenses ''Shape

data Universe = Universe
  {
    _universeShapes :: M.Map ShapeId Shape
  } deriving (Eq, Show)
makeLenses ''Universe

data Square = Square
  {
    _squareShape :: Shape,
    _squarePosition :: V2 Integer
  } deriving (Eq, Show, Ord)
makeLenses ''Square

data Location = Location
  {
    _locationSquare :: Square,
    _locationSubposition :: V2 Integer
  } deriving (Eq, Show, Ord)
makeLenses ''Location

lookupShape :: Universe -> ShapeId -> Shape
lookupShape u sid = fromJustOrDie "Couldn't find shape" $ M.lookup sid (u ^. universeShapes)

shapeHasParent :: Shape -> Shape -> Bool
shapeHasParent s p = any (\(sid, _) -> sid == p ^. shapeId ) (s ^. shapeParentIds)

shapePositionOn :: Shape -> Shape -> Maybe (V2 Integer)
shapePositionOn s p = lookup (p ^. shapeId) (s ^. shapeParentIds)

shapeChildren :: Shape -> Traversal' Universe Shape
shapeChildren s = universeShapes . traverse . filtered (flip shapeHasParent s)

shapeChildrenWithPosition :: Shape -> Fold Universe (Shape, V2 Integer)
shapeChildrenWithPosition s = shapeChildren s . to (\c -> (c,) <$> shapePositionOn c s ) . _Just

-- The square's position on a specified shape
squareLocationOn :: Universe -> Square -> (ShapeId, V2 Integer) -> Maybe Location
squareLocationOn u s (pid, shapePos) = do
  let (p', subp) = posDivMod (s^.squarePosition + shapePos)
      newShape = lookupShape u pid
  return $ Location (Square newShape p') subp

squareLocation :: Universe -> Square -> Location
squareLocation u s = case catMaybes $ fmap (squareLocationOn u s) (s ^. squareShape . shapeParentIds) of
                       []  -> error "A square is not sitting on anything"
                       [sh] -> sh
                       _  -> error "A square is sitting on multiple things"

locationToPositionOn :: Location -> V2 Integer -> V2 Integer
locationToPositionOn (Location (Square s p) subp) spos = levelScale *^ (p - spos) + subp

shapeHasPosition :: Shape -> V2 Integer -> Bool
shapeHasPosition s p = polyContainsPoint (s ^. shapePolyomino) p

inhabitant :: Universe -> Location -> Maybe Square
inhabitant u l = case catMaybes maybes of
                   []  -> Nothing
                   [s] -> Just s
                   _  -> error "Location has two inhabitants"
  where checkShape (s, spos) = let lpos = locationToPositionOn l spos in
                               if shapeHasPosition s lpos then Just (Square s lpos) else Nothing
        maybes = fmap checkShape $ u ^.. shapeChildrenWithPosition (l ^. locationSquare . squareShape)

footprintOn :: Shape -> Shape -> Polyomino
footprintOn s p = let pos = fromJustOrDie "Footprint not parent" $ shapePositionOn s p in
                    polyDiv $ polyOffset (s ^. shapePolyomino) pos

isInhabited :: Universe -> Location -> Bool
isInhabited u l = isJust $ inhabitant u l

class Eq a => HasSquares a where
  constituentSquares :: Universe -> a -> [Square]

  constituentShapes :: Universe -> a -> [Shape]
  constituentShapes u a = nub $ fmap (_squareShape) $ constituentSquares u a

  childShapes :: Universe -> a -> [Shape]
  childShapes u a = nub $ fmap (_squareShape) $ catMaybes $ fmap (inhabitant u) (constituentLocations u a)

constituentLocations :: HasSquares a => Universe -> a -> [Location]
constituentLocations u a = do
  s <- constituentSquares u a
  subp <- allSubpositions
  return $ Location s subp

allSubpositions :: [V2 Integer]
allSubpositions = do
  x <- [0 .. levelScale - 1]
  y <- [0 .. levelScale - 1]
  return $ V2 x y

-- --------------------------------------------------------------------------------
-- -- Nudging

wrapSubposition :: V2 Integer -> (Bool, V2 Integer)
wrapSubposition (V2 x y) = (x /= wx || y /= wy, V2 wx wy)
  where wx = x `mod` levelScale
        wy = y `mod` levelScale

nudgeSubposition :: Direction -> V2 Integer -> (Bool, V2 Integer)
nudgeSubposition d v = wrapSubposition (v + directionToVector d)

nudgeLocation' :: Universe -> [(Direction, Location)] -> Direction -> Location -> Maybe Location
nudgeLocation' _ seen d l | (d, l) `elem` seen = Nothing
nudgeLocation' u seen d l@(Location s p)
  = case nudgeSubposition d p of
      (False, p') -> Just (Location s p')
      (True, p') -> do
        s' <- nudgeSquare' u ((d, l) : seen) d s
        return $ Location s' p'

nudgeSquareOnPoly :: Direction -> Square -> Maybe Square
nudgeSquareOnPoly d (Square s p)
  = if polyContainsPoint (s ^. shapePolyomino) (p + directionToVector d) then
      Just $ Square s (p + directionToVector d)
    else
      Nothing

nudgeSquare' :: Universe -> [(Direction, Location)] -> Direction -> Square -> Maybe Square
nudgeSquare' _ _ d s | Just s' <- nudgeSquareOnPoly d s = Just s'
nudgeSquare' u seen d s = do
  l <- nudgeLocation' u seen d (squareLocation u s)
  inhabitant u l

nudgeLocation :: Universe -> Direction -> Location -> Maybe Location
nudgeLocation u d l = nudgeLocation' u [] d l

nudgeSquare :: Universe -> Direction -> Square -> Maybe Square
nudgeSquare u d s = nudgeSquare' u [] d s
