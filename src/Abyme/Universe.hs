{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Universe where

import Control.Lens hiding (contains)
import Data.List (nub, intersect, union, find, delete, (\\))
import Data.Maybe (fromJust, catMaybes, isJust)
import qualified Data.Map.Strict as M
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

-- TODO: for efficiency:
-- * Keep track of which shapes are on the edge of their region?

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

regionEraseShape :: Shape -> Region -> Region
regionEraseShape s r = r & regionShapes %~ (delete s)

regionEraseShapes :: [Shape] -> Region -> Region
regionEraseShapes ss r = r & regionShapes %~ (\\ ss)

newRegionId :: Universe -> RegionId
newRegionId (Universe rs) = RegionId $ 1 + (getRegionId $ fst $ M.findMax rs)

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

isInhabited :: Universe -> Location -> Bool
isInhabited u l = isJust $ inhabitant u l

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
-- A Chunk should be EXACTLY a collection of shapes that are glued together

data Chunk = Chunk {
  _chunkRegion :: Region,
  _chunkShapes :: [Shape]
} deriving (Eq, Show)
makeLenses ''Chunk

instance HasSquares Chunk where
  constituentSquares u c = concat $ fmap (constituentSquares u) (chunkPieces c)

chunkPieces :: Chunk -> [Piece]
chunkPieces (Chunk r ss) = fmap (Piece r) ss

chunkHasPiece :: Chunk -> Piece -> Bool
chunkHasPiece (Chunk r ss) (Piece r' s) = r == r' && s `elem` ss

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
regionChunks u r = fmap (Chunk r) $ fmap (fmap _pieceShape) $ unionize $ fmap (habitat u) $ concatMap regionPieces $ childRegions u r

-- TODO: DANGER DANGER: this only works for exactly 2 levels
findChunk :: Universe -> Piece -> Chunk
findChunk u p = fromJustOrDie "Piece was not found in list of its own nearby chunks" $ find (flip chunkHasPiece p) (regionChunks u (p ^. pieceRegion))

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

setNewParent :: Universe -> [(Region, [Region])] -> Region -> Region
setNewParent u adjs c@(Region cid _ cpos cshapes) = Region cid pid (cpos + opos - ppos) cshapes
  where oldp@(Region _ _ opos _) = regionParent u c
        newp@(Region pid _ ppos _) = findNewParent adjs c

fuseInhabitantRegions' :: Universe -> Region -> (Universe, [RegionId])
fuseInhabitantRegions' u@(Universe regionMap) r = (Universe adjusted, needRecursion)
  where children = childRegions u r
        childrenIds = fmap _regionId children
        chunks = collectRegionChunks u children
        newRegions = fmap fuseRegions chunks
        adjustments = zip newRegions chunks
        deletedM = foldl (\m i -> M.delete i m) regionMap (fmap _regionId children)
        addedM = foldl (\m r -> M.insert (_regionId r) r m) deletedM newRegions
        adjusted = addedM & itraversed . indices (`elem` childrenIds) %~ setNewParent u adjustments
        needRecursion = fmap _regionId $ concat $ filter (\g -> length g > 1) chunks

fuseInhabitantRegions :: Universe -> Region -> Universe
fuseInhabitantRegions u r = go u [r ^. regionId]
  where go u [] = u
        go u (i:is) = if M.member i (u ^. universeRegions) then
                        let (u', newis) = fuseInhabitantRegions' u (u ^?! universeRegions . ix i)
                        in go u' (is ++ newis)
                      else
                        go u is

-- TODO: DANGER DANGER: need to fuse recursively

canPushChunk :: Universe -> Direction -> Chunk -> Bool
canPushChunk u d c = not (oob || any (isInhabited u) fr)
  where (fr, oob) = fringe u d c

-- These are unsafe if used on their own:
erasePiece :: Universe -> Piece -> Universe
erasePiece (Universe rs) (Piece r s) = Universe $ M.adjust (regionEraseShape s) (r ^. regionId) rs

eraseChunk :: Universe -> Chunk -> Universe
eraseChunk (Universe rs) (Chunk r ss) = Universe $ M.adjust (regionEraseShapes ss) (r ^. regionId) rs

splitChunkIntoRegion :: Universe -> Chunk -> (Region, Universe)
splitChunkIntoRegion u@(Universe m) c = if isWholeRegion then
                                          (region, u)
                                        else
                                          (newRegion, Universe $ fmap adjustParent $ M.insert (region ^. regionId) remainingRegion $ M.insert newId newRegion $ m)
  where region = c ^. chunkRegion
        isWholeRegion = length (region ^. regionShapes \\ c ^. chunkShapes) > 0
        newId = newRegionId u
        newRegion = Region newId (region ^. regionParentId) (region ^. regionPosition) (c ^. chunkShapes)
        remainingRegion = (c ^. chunkRegion) {_regionShapes = region ^. regionShapes \\ c ^. chunkShapes }
        adjustParent (Region id pid pos sh)
          = if head sh `elem` c ^. chunkShapes then
              Region id newId pos sh
            else
              Region id pid pos sh

pushRegion :: Universe -> Direction -> Region -> Universe
pushRegion u d r = u & universeRegions . ix (r ^. regionId) . regionPosition +~ directionToVector d

pushChunk :: Universe -> Direction -> Chunk -> Universe
pushChunk u d c = let (r, u') = splitChunkIntoRegion u c
                  in fuseInhabitantRegions (pushRegion u' d r) (u ^?! universeRegions . ix (r ^. regionParentId))


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
