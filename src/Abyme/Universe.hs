{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Universe where

import Control.Lens
import Data.List (nub, intersect, union, find)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Monoid
import Linear

import Abyme.Direction
import Abyme.Polyomino

levelScale :: Integer
levelScale = 2

posDivMod :: V2 Integer -> (V2 Integer, V2 Integer)
posDivMod (V2 x y) = (V2 q q', V2 r r')
  where (q, r) = x `divMod` levelScale
        (q', r') = y `divMod` levelScale

fromJustOrDie :: String -> Maybe a -> a
fromJustOrDie _ (Just a) = a
fromJustOrDie m Nothing = error m

newtype RegionId = RegionId { getRegionId :: Integer }
  deriving (Eq, Ord, Show)

data Shape = Shape
  {
    _shapePosition :: V2 Integer, -- on region
    _shapePolyomino :: Polyomino
  } deriving (Eq, Show)

makeLenses ''Shape

data Region = Region
  {
    _regionId :: RegionId,
    _regionParentId :: RegionId,
    _regionPosition :: V2 Integer, -- on parent
    _regionShapes :: [Shape] -- constituents
  } deriving (Eq, Show)

makeLenses ''Region

data Universe = Universe
  {
    _universeRegions :: M.Map RegionId Region
  } deriving (Eq, Show)

makeLenses ''Universe

-- Should be total
regionParent :: Universe -> Region -> Region
regionParent u c = fromJust $ u ^. universeRegions . at (c ^. regionParentId)

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

-- Could be traversals
regionPieces :: Region -> [Piece]
regionPieces r = fmap (Piece r) (r ^. regionShapes)

regionChildRegions :: Universe -> Region -> [Region]
regionChildRegions u r = u ^.. universeRegions . traverse . filtered (\c -> r^.regionId == c^.regionParentId)

regionChildPieces :: Universe -> Region -> [Piece]
regionChildPieces u r = regionPieces =<< (regionChildRegions u r)

-- Position relative to constituent region origin
shapeContains :: Shape -> V2 Integer -> Bool
shapeContains s p = polyContainsPoint (s^.shapePolyomino) (p - s^.shapePosition)

findConstituentSquare :: Region -> V2 Integer -> Maybe Square
findConstituentSquare c p = fmap (\s -> Square (Piece c s) (p - s^.shapePosition)) maybeSquare
  where maybeSquare = c ^? regionShapes . traverse . filtered (flip shapeContains p)

-- Position relative to parent region origin
shapeInhabits :: Region -> Shape -> V2 Integer -> Bool
shapeInhabits c s p = shapeContains s (p - c^.regionPosition)

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

pieceSquares :: Piece -> [Square]
pieceSquares p = fmap (\s -> Square p s) (p ^. pieceShape . shapePolyomino . polyominoSquares)

pieceLocations :: Universe -> Piece -> [Location]
pieceLocations u p = fmap (squareLocation u) (pieceSquares p)

-- --------------------------------------------------------------------------------
-- -- Nudging

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

-- --------------------------------------------------------------------------------
-- -- Chunks
-- TODO: DANGER DANGER: this only works for exactly 2 levels

data Chunk = Chunk {
  _chunkPieces :: [Piece]
} deriving (Eq, Show)
makeLenses ''Chunk

chunkFringe :: Universe -> Direction -> Chunk -> [Location]
chunkFringe = undefined

pieceParents :: Universe -> Piece -> [Piece]
pieceParents u p = nub $ fmap (_squarePiece . _locationSquare . squareLocation u) $ pieceSquares p

unionize :: Eq a => [[a]] -> [[a]]
unionize [] = []
unionize (g:gs) = go [] g gs
  where go as g [] = g : unionize as
        go as g (b:bs) = if null $ intersect g b then
                           go (b:as) g bs
                         else
                           unionize $ [union g b] ++ as ++ bs

regionChunks :: Universe -> Region -> [Chunk]
regionChunks u r = fmap Chunk $ unionize $ fmap (pieceParents u) $ regionChildPieces u r

findChunk :: Universe -> Piece -> Chunk
findChunk u p = Chunk $ fromJustOrDie "Piece was not found in list of nearby chunks" $ find (p `elem`) (fmap _chunkPieces $ regionChunks u (p ^. pieceRegion))

-- --------------------------------------------------------------------------------
-- -- Example

-- minimal :: Universe
-- minimal = Universe (M.fromList [(cid1, [shape1]), (cid2, [shape2])])
--                    (M.fromList [(cid1, [shape2]), (cid2, [shape1])])
-- mono = Polyomino [V2 0 0]
-- cid1 = RegionId 1
-- cid2 = RegionId 2
-- shape1 = Shape cid1 (V2 0 0) cid2 (V2 0 0) mono
-- shape2 = Shape cid2 (V2 0 0) cid1 (V2 0 0) mono
