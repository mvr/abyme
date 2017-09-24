{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Regiony.Addressing (
  Piece(..),
  pieceRegion,
  pieceShape,
  Square(..),
  squarePiece,
  squareCoordinates,
  Location(..),
  locationSquare,
  locationSubPosition,

  regionPieces,
  locationToPosition,
  squareLocation,
  inhabitant,
  isInhabited,

  atPiece,

  HasSquares(..),
  constituentLocations,

  contains,
  locations,
  inhabits,
  habitat,
  fringe,
  halo,
  neighbourhood,
  uninhabited,

  regionCollectAdjacentShapes,

  nudgeLocation,
  nudgeSquare
) where

import Control.Lens hiding (contains, children)
import Data.List (nub, intersect)
import Data.Maybe (catMaybes, isJust)
import Linear

import Abyme.Util (unionize, posDivMod, levelScale, fromJustOrDie)
import Abyme.Direction
import Abyme.Polyomino
import Abyme.Regiony.Universe

-- --------------------------------------------------------------------------------
-- -- Addressing

data Piece = Piece {
  _pieceRegion :: Region,
  _pieceShape :: Shape
} deriving (Eq, Show)
makeLenses ''Piece

data Square = Square {
  _squarePiece :: Piece,
  _squareCoordinates :: V2 Integer -- relative to shape, must actually be in the poly
} deriving (Eq, Show)
makeLenses ''Square

data Location = Location {
  _locationSquare :: Square,
  _locationSubPosition :: V2 Integer
} deriving (Eq, Show)
makeLenses ''Location

-- --------------------------------------------------------------------------------
-- Fundamentals

-- Could be indexed(?) traversals
regionPieces :: Region -> [Piece]
regionPieces r = fmap (Piece r) (r ^. regionShapes)

locationToPosition :: Location -> V2 Integer
locationToPosition (Location (Square (Piece _ s) p) subp) = levelScale *^ (p + s ^. shapePosition) + subp

findConstituentSquare :: Region -> V2 Integer -> Maybe Square
findConstituentSquare c p = fmap (\s -> Square (Piece c s) (p - s^.shapePosition)) maybeSquare
  where maybeSquare = c ^? regionShapes . traverse . filtered (flip shapeContains p)

findInhabitantSquare :: Universe -> Region -> V2 Integer -> Maybe Square
findInhabitantSquare u r p = case catMaybes $ fmap checkChild children of
                               []  -> Nothing
                               [s] -> Just s
                               _   -> error "Multiple child regions claim to inhabit the same location"
  where children = childRegions u r
        checkChild c = findConstituentSquare c (p - c^.regionPosition)

-- This is total unless the Universe is busted
squareLocation :: Universe -> Square -> Location
squareLocation u (Square (Piece r s) p) = Location newSquare subp
  where (p', subp) = posDivMod (p + s ^. shapePosition + r ^. regionPosition)
        newSquare = fromJustOrDie ("Square wasn't sitting on a square in the parent region") $
                    findConstituentSquare (regionParent u r) p'

inhabitant :: Universe -> Location -> Maybe Square
inhabitant u l@(Location (Square (Piece c _) _) _) = findInhabitantSquare u c (locationToPosition l)

isInhabited :: Universe -> Location -> Bool
isInhabited u l = isJust $ inhabitant u l

-- --------------------------------------------------------------------------------
-- Lenses

atPiece :: Piece -> Lens' Universe Shape
atPiece (Piece r s) = singular $ universeRegions . ix (r ^. regionId) . regionShapes . traverse . filtered (== s)

-- --------------------------------------------------------------------------------
-- HasSquares

-- TODO: A lot of these could be smarter / more efficient

-- Here constituentPieces is any piece that this `a` is a part of,
-- and childRegions and childPieces are any children that intersect
-- `a` at all, the children don't have to be completely contained

class Eq a => HasSquares a where
  constituentSquares :: Universe -> a -> [Square]

  constituentPieces :: Universe -> a -> [Piece]
  constituentPieces u a = nub $ fmap (_squarePiece) $ constituentSquares u a

  childRegions :: Universe -> a -> [Region]
  childRegions u a = nub $ fmap (_pieceRegion . _squarePiece) $ catMaybes $ fmap (inhabitant u) (constituentLocations u a)

  childPieces :: Universe -> a -> [Piece]
  childPieces u a = nub $ fmap (_squarePiece) $ catMaybes $ fmap (inhabitant u) (constituentLocations u a)

instance HasSquares Square where
  constituentSquares _ s = [s]
  constituentPieces _ s = [s ^. squarePiece]

instance HasSquares Piece where
  constituentSquares _ p = fmap (\s -> Square p s) (p ^. pieceShape . shapePolyomino . polyominoSquares)
  constituentPieces _ p = [p]

instance HasSquares Region where
  constituentSquares u r = concat $ fmap (constituentSquares u) pieces
    where pieces = fmap (Piece r) (r ^. regionShapes)

  constituentPieces _ r = regionPieces r

  childRegions u r = u ^.. universeRegions . traverse . filtered (\c -> r^.regionId == c^.regionParentId)

-- instance HasSquares a => HasSquares [a] where -- ??

constituentLocations :: HasSquares a => Universe -> a -> [Location]
constituentLocations u a = do
  s <- constituentSquares u a
  subp <- allSubpositions
  return $ Location s subp

contains :: HasSquares a => Universe -> a -> Square -> Bool
contains u a s = s `elem` (constituentSquares u a)

locations :: HasSquares a => Universe -> a -> [Location]
locations u a = fmap (squareLocation u) (constituentSquares u a)

inhabits :: HasSquares a => Universe -> a -> Location -> Bool
inhabits u a l = l `elem` (locations u a)

habitat :: HasSquares a => Universe -> a -> [Piece]
habitat u a = nub $ fmap (\l -> l ^. locationSquare . squarePiece) (locations u a)

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

neighbourhood :: HasSquares a => Universe -> a -> [Piece]
neighbourhood u a = nub $ constituentPieces u a ++ neighbouring
  where neighbouring = fmap (\s -> s ^. squarePiece) $ catMaybes $ fmap (inhabitant u) (halo u a)

uninhabited :: HasSquares a => Universe -> a -> Bool
uninhabited u a = null (childRegions u a)

regionCollectAdjacentShapes :: Universe -> Region -> [Shape] -> [[Shape]]
regionCollectAdjacentShapes u r ss = fmap (fmap _pieceShape) $ unionize $ fmap (\p -> neighbourhood u p `intersect` pieces) pieces
  where pieces = fmap (Piece r) ss

-- --------------------------------------------------------------------------------
-- -- Nudging

allSubpositions :: [V2 Integer]
allSubpositions = do
  x <- [0 .. levelScale - 1]
  y <- [0 .. levelScale - 1]
  return $ V2 x y

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
nudgeSquareOnPoly d (Square (Piece c s@(Shape _ poly)) p)
  = if polyContainsPoint poly (p + directionToVector d) then
      Just $ Square (Piece c s) (p + directionToVector d)
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
