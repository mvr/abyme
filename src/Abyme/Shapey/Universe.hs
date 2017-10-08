{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Shapey.Universe where

import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.List (nub)
import           Data.Maybe (isJust, catMaybes, maybeToList)
import           Control.Monad (guard)
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

newShapeId :: Universe -> ShapeId
newShapeId (Universe rs) = ShapeId $ 1 + (getShapeId $ fst $ M.findMax rs)

atShape :: Shape -> Lens' Universe Shape
atShape s = singular $ universeShapes . ix (s ^. shapeId)

locationShape :: Lens' Location Shape
locationShape = locationSquare . squareShape

lookupShape :: Universe -> ShapeId -> Shape
lookupShape u sid = fromJustOrDie "Couldn't find shape" $ M.lookup sid (u ^. universeShapes)

shapeHasParent :: Shape -> Shape -> Bool
shapeHasParent s p = any (\(sid, _) -> sid == p ^. shapeId ) (s ^. shapeParentIds)

shapeParents :: Universe -> Shape -> [Shape]
shapeParents u s = fmap (lookupShape u) (s ^.. shapeParentIds . traverse . to fst)

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
  guard $ shapeHasPosition newShape p'
  return $ Location (Square newShape p') subp

squareLocation :: Universe -> Square -> Location
squareLocation u s = case catMaybes $ fmap (squareLocationOn u s) (s ^. squareShape . shapeParentIds) of
                       []  -> error "A square is not sitting on anything"
                       [sh] -> sh
                       _  -> error "A square is sitting on multiple things"

locationToPosition :: Location -> V2 Integer
locationToPosition (Location (Square _ p) subp) = (levelScale *^ p) + subp

locationToPositionOn :: Location -> V2 Integer -> V2 Integer
locationToPositionOn l spos = locationToPosition l - (levelScale *^ spos)

shapeHasPosition :: Shape -> V2 Integer -> Bool
shapeHasPosition s p = polyContainsPoint (s ^. shapePolyomino) p

inhabitant :: Universe -> Location -> Maybe Square
inhabitant u l = case catMaybes maybes of
                   []  -> Nothing
                   [s] -> Just s
                   _  -> error "Location has two inhabitants"
  where checkShape (s, spos) = let lpos = locationToPosition l in
                               if shapeHasPosition s (lpos - spos) then Just (Square s (lpos - spos)) else Nothing
        maybes = fmap checkShape $ u ^.. shapeChildrenWithPosition (l ^. locationShape)

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

instance HasSquares Square where
  constituentSquares _ s = [s]
  constituentShapes _ s = [s ^. squareShape]

instance HasSquares Shape where
  constituentSquares _ s = fmap (Square s) (s ^. shapePolyomino . polyominoSquares)
  constituentShapes _ s = [s]

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

contains :: HasSquares a => Universe -> a -> Square -> Bool
contains u a s = s `elem` (constituentSquares u a)

locations :: HasSquares a => Universe -> a -> [Location]
locations u a = fmap (squareLocation u) (constituentSquares u a)

inhabits :: HasSquares a => Universe -> a -> Location -> Bool
inhabits u a l = l `elem` (locations u a)

habitat :: HasSquares a => Universe -> a -> [Shape]
habitat u a = nub $ fmap (\l -> l ^. locationShape) (locations u a)

-- Bool records if we hit OoB
fringe :: HasSquares a => Universe -> Direction -> a -> ([Location], Bool)
fringe u d a = (filter (not . inhabits u a) justs, length allMaybes /= length justs)
  where allMaybes = fmap (nudgeLocation u d) $ locations u a
        justs = catMaybes allMaybes

halo :: HasSquares a => Universe -> a -> [Location]
halo uni a = nub $ u ++ d ++ l ++ r
  where (u, _) = fringe uni Up a
        (d, _) = fringe uni Down a
        (l, _) = fringe uni LEft a
        (r, _) = fringe uni RIght a

fringeWithOriginal :: HasSquares a => Universe -> Direction -> a -> [(Square, Location)]
fringeWithOriginal u d a = do
  s <- constituentSquares u a
  l <- maybeToList $ nudgeLocation u d (squareLocation u s)
  guard $ not $ inhabits u a l
  return (s, l)

haloWithOriginal :: HasSquares a => Universe -> a -> [(Direction, Square, Location)]
haloWithOriginal uni a = u ++ d ++ l ++ r
  where u = fmap (mark Up)    $ fringeWithOriginal uni Up a
        d = fmap (mark Down)  $ fringeWithOriginal uni Down a
        l = fmap (mark LEft)  $ fringeWithOriginal uni LEft a
        r = fmap (mark RIght) $ fringeWithOriginal uni RIght a
        mark d (l, l') = (d, l, l')

neighbourhood :: HasSquares a => Universe -> a -> [Shape]
neighbourhood u a = nub $ constituentShapes u a ++ neighbouring
  where neighbouring = fmap (\s -> s ^. squareShape) $ catMaybes $ fmap (inhabitant u) (halo u a)

uninhabited :: HasSquares a => Universe -> a -> Bool
uninhabited u a = null (childShapes u a)

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
