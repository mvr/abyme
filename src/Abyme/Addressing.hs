{-# LANGUAGE TemplateHaskell #-}
module Abyme.Addressing where

import Control.Lens hiding (contains)
import Data.List (nub)
import Data.Maybe (catMaybes, isJust)
import Data.Monoid
import Linear

import Abyme.Direction
import Abyme.Polyomino
import Abyme.Universe

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

-- Could be indexed traversals
regionPieces :: Region -> [Piece]
regionPieces r = fmap (Piece r) (r ^. regionShapes)

findConstituentSquare :: Region -> V2 Integer -> Maybe Square
findConstituentSquare c p = fmap (\s -> Square (Piece c s) (p - s^.shapePosition)) maybeSquare
  where maybeSquare = c ^? regionShapes . traverse . filtered (flip shapeContains p)

findInhabitantSquare :: Universe -> Region -> V2 Integer -> Maybe Square
findInhabitantSquare u c p = findConstituentSquare (regionParent u c) (p - c^.regionPosition)

-- This is total unless the Universe is busted
squareLocation :: Universe -> Square -> Location
squareLocation u (Square (Piece c s) p) = Location newSquare subp
  where (p', subp) = posDivMod p
        newSquare = fromJustOrDie "Square wasn't sitting on a square in the parent region" $
                    findConstituentSquare (regionParent u c) p'

inhabitant :: Universe -> Location -> Maybe Square
inhabitant u (Location (Square (Piece c s) p) subp) = findInhabitantSquare u c (levelScale *^ p + subp)

isInhabited :: Universe -> Location -> Bool
isInhabited u l = isJust $ inhabitant u l

-- --------------------------------------------------------------------------------
-- HasSquares

class Eq a => HasSquares a where
  constituentSquares :: Universe -> a -> [Square]

  childRegions :: Universe -> a -> [Region]
  childRegions u a = nub $ fmap (_pieceRegion . _squarePiece) $ catMaybes $ fmap (inhabitant u) (constituentLocations u a)

instance HasSquares Square where
  constituentSquares _ s = [s]

instance HasSquares Piece where
  constituentSquares _ p = fmap (\s -> Square p s) (p ^. pieceShape . shapePolyomino . polyominoSquares)

instance HasSquares Region where
  constituentSquares u r = concat $ fmap (constituentSquares u) pieces
    where pieces = fmap (Piece r) (r ^. regionShapes)
  childRegions u r = u ^.. universeRegions . traverse . filtered (\c -> r^.regionId == c^.regionParentId)

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
fringe u d a = (filter (inhabits u a) justs, length allMaybes == length justs)
  where allMaybes = fmap (nudgeLocation u d) $ locations u a
        justs = catMaybes allMaybes

halo :: HasSquares a => Universe -> a -> [Location]
halo uni a = nub $ u ++ d ++ l ++ r
  where (u, _) = fringe uni Up a
        (d, _) = fringe uni Down a
        (l, _) = fringe uni LEft a
        (r, _) = fringe uni RIght a

childPieces :: HasSquares a => Universe -> a -> [Piece]
childPieces u a = regionPieces =<< (childRegions u a)
  where regionPieces r = fmap (Piece r) (r ^. regionShapes)

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

nudgeLocation' :: Universe -> First (Direction, Location) -> Direction -> Location -> Maybe Location
nudgeLocation' _ (First (Just (d', l'))) d l | d == d' && l == l' = Nothing
nudgeLocation' u f d l@(Location s p)
  = case nudgeSubposition d p of
      (False, p') -> Just (Location s p')
      (True, p') -> do
        s' <- nudgeSquare' u (f <> First (Just (d, l))) d s
        return $ Location s' p'

nudgeSquareOnPoly :: Direction -> Square -> Maybe Square
nudgeSquareOnPoly d (Square (Piece c s@(Shape _ poly)) p)
  = if polyContainsPoint poly (p + directionToVector d) then
      Just $ Square (Piece c s) (p + directionToVector d)
    else
      Nothing

nudgeSquare' :: Universe -> First (Direction, Location) -> Direction -> Square -> Maybe Square
nudgeSquare' _ _ d s | Just s' <- nudgeSquareOnPoly d s = Just s'
nudgeSquare' u f d s = do
  l <- nudgeLocation' u f d (squareLocation u s)
  inhabitant u l

nudgeLocation :: Universe -> Direction -> Location -> Maybe Location
nudgeLocation u d l = nudgeLocation' u (First Nothing) d l

nudgeSquare :: Universe -> Direction -> Square -> Maybe Square
nudgeSquare u d s = nudgeSquare' u (First Nothing) d s
