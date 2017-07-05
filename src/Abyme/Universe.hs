{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Universe where

import Control.Lens hiding (contains)
import Data.List (nub, intersect, union, find)
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import Data.Monoid
import Linear

import Abyme.Direction
import Abyme.Polyomino

-- Some nomenclature:
-- * There are Shapes that when next to each other form Regions.
-- * Some Shapes in a Region are actually locked together by shapes on a
--   lower level, these form Chunks
-- * A Piece is one Square in a Region
-- * Every Shape is made up of Squares, every Square lies on a Location
-- * Fringe is all the squares adjacent to something in a particular direction
--   Halo is all the squares adjacent to something in any direction

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

data Chunk = Chunk {
  _chunkPieces :: [Piece]
} deriving (Eq, Show)
makeLenses ''Chunk

-- --------------------------------------------------------------------------------
-- Fundamentals

-- Could be indexed traversals
regionPieces :: Region -> [Piece]
regionPieces r = fmap (Piece r) (r ^. regionShapes)

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

-- --------------------------------------------------------------------------------
-- -- More convenient accessors

class Eq a => HasSquares a where
  constituentSquares :: Universe -> a -> [Square]

  childRegions :: Universe -> a -> [Region]
  childRegions u a = nub $ fmap (_pieceRegion . _squarePiece) $ catMaybes $ fmap (inhabitant u) (constituentLocations u a)

instance HasSquares Square where
  constituentSquares _ s = [s]

instance HasSquares Piece where
  constituentSquares _ p = fmap (\s -> Square p s) (p ^. pieceShape . shapePolyomino . polyominoSquares)

instance HasSquares Chunk where
  constituentSquares u c = concat $ fmap (constituentSquares u)(_chunkPieces c)

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

-- --------------------------------------------------------------------------------
-- -- Chunks
-- TODO: DANGER DANGER: this only works for exactly 2 levels

unionize :: Eq a => [[a]] -> [[a]]
unionize [] = []
unionize (g:gs) = go [] g gs
  where go as g [] = g : unionize as
        go as g (b:bs) = if null $ intersect g b then
                           go (b:as) g bs
                         else
                           unionize $ [union g b] ++ as ++ bs

-- TODO maybe store this in Region
regionChunks :: Universe -> Region -> [Chunk]
regionChunks u r = fmap Chunk $ unionize $ fmap (habitat u) $ concatMap regionPieces $ childRegions u r

findChunk :: Universe -> Piece -> Chunk
findChunk u p = Chunk $ fromJustOrDie "Piece was not found in list of its own nearby chunks" $ find (p `elem`) (fmap _chunkPieces $ regionChunks u (p ^. pieceRegion))

fusePair :: Region -> Region -> Region
fusePair (Region lid lparent lpos lshapes) (Region rid rparent rpos rshapes)
  = if lparent == rparent then
      Region lid lparent lpos (lshapes ++ fmap fixShape rshapes)
    else
      error "Can't fuse Regions with different parents"
  where
    fixShape (Shape pos poly) = Shape (pos + lpos - rpos) poly

fuseRegions :: [Region] -> Region
fuseRegions [] = error "Cannot fuse empty list"
fuseRegions rs = foldl1 fusePair rs

adjacentRegions :: Universe -> Region -> [Region]
adjacentRegions u r = (r:) $ nub $ fmap (_pieceRegion . _squarePiece) $ catMaybes $ fmap (inhabitant u) (halo u r)

collectRegionChunks :: Universe -> [Region] -> [[Region]]
collectRegionChunks u rs = unionize $ fmap (adjacentRegions u) $ rs

findNewParent :: Eq a => [(a, [a])] -> a -> a
findNewParent [] a = a
findNewParent ((n, os):rest) a = if a `elem` os then n else findNewParent rest a

-- new parent, child
adjustChild :: Universe -> [Region] -> [(Region, [Region])] -> Region -> Region
adjustChild u needFixing adjs c@(Region cid _ cpos cshapes)
  = if o `elem` needFixing then
      let (Region pid _ ppos _) = findNewParent adjs c in
      Region cid pid (cpos + opos - ppos) cshapes
    else
      c
  where o@(Region _ _ opos _) = regionParent u c -- old parent

fuseInhabitantRegions :: Universe -> Region -> Universe
fuseInhabitantRegions u@(Universe regionMap) r = Universe adjusted
  where children = childRegions u r
        chunks = collectRegionChunks u children
        newRegions = fmap fuseRegions chunks
        adjustments = zip newRegions chunks
        deletedM = foldl (\m i -> M.delete i m) regionMap (fmap _regionId children)
        addedM = foldl (\m r -> M.insert (_regionId r) r m) deletedM newRegions
        adjusted = fmap (adjustChild u children adjustments) addedM

pushChunk :: Universe -> Direction -> Chunk -> Universe
pushChunk = undefined

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
