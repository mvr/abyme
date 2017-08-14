{-# LANGUAGE TemplateHaskell #-}
module Abyme.Chunk where

import Control.Lens hiding (contains)
import Data.List (nub, find, (\\))
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import Linear

import Abyme.Direction
import Abyme.Universe
import Abyme.Addressing
import Abyme.Util (levelScale, unionize, fromJustOrDie)

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

-- TODO maybe store this in Region
regionChunks :: Universe -> Region -> [Chunk]
regionChunks u r = fmap (Chunk r) $ fmap (fmap _pieceShape) $ unionize $ fmap (habitat u) $ concatMap regionPieces $ childRegions u r

-- TODO: DANGER DANGER: this only works for exactly 2 levels
findChunk :: Universe -> Piece -> Chunk
findChunk u p = fromJustOrDie "Piece was not found in list of its own nearby chunks" $ find (flip chunkHasPiece p) (regionChunks u (p ^. pieceRegion))

-- --------------------------------------------------------------------------------
-- -- Fusing and Splitting

fusePair :: Region -> Region -> Region
fusePair (Region lid lparent lpos lshapes) (Region rid rparent rpos rshapes)
  = if lparent == rparent then
      Region lid lparent lpos (lshapes ++ fmap fixShape rshapes)
    else
      error "Can't fuse Regions with different parents"
  where
    fixShape (Shape pos poly) = Shape (pos + rpos - lpos) poly

fuseRegions :: [Region] -> Region
fuseRegions [] = error "Cannot fuse empty list"
fuseRegions rs = foldl1 fusePair rs

adjacentRegions :: Universe -> Region -> [Region]
adjacentRegions u r = nub $ (r:) $ fmap (\s -> s ^. squarePiece . pieceRegion) $ catMaybes $ fmap (inhabitant u) (halo u r)

collectRegionChunks :: Universe -> [Region] -> [[Region]]
collectRegionChunks u rs = unionize $ fmap (adjacentRegions u) $ rs

findRepresentative :: Eq a => [(a, [a])] -> a -> Maybe a
findRepresentative [] _ = Nothing
findRepresentative ((n, os):rest) a = if a `elem` os then Just n else findRepresentative rest a

setNewParent :: Universe -> [(Region, [Region])] -> Region -> Region
setNewParent u adjs c@(Region cid _ cpos cshapes)
  = let o = regionParent u c in -- old parent
    case findRepresentative adjs o of
      Just (Region nid _ npos _) -> Region cid nid (cpos + parentOffset) cshapes
        where parentOffset = levelScale *^ ((o^.regionPosition) - npos)
      Nothing -> c

-- This is a little complex... We need to fuse together the regions,
-- then recursively fuse the child regions, as we may have caused a
-- cascade.

-- TODO: don't adjust regions that don't change
fuseInhabitantRegions' :: Universe -> Region -> (Universe, [RegionId])
fuseInhabitantRegions' u r = (Universe adjusted, needRecursion)
  where children = childRegions u r
--        childrenIds = fmap _regionId children
        chunks = collectRegionChunks u children
        newRegions = fmap fuseRegions chunks
        adjustments = zip newRegions chunks
        deletedM = foldl (\m i -> M.delete i m) (u ^. universeRegions) (fmap _regionId children)
        addedM = foldl (\m r -> M.insert (_regionId r) r m) deletedM newRegions
        adjusted = addedM & traversed %~ setNewParent u adjustments
        needRecursion = fmap _regionId $ concat $ filter (\g -> length g > 1) chunks

fuseInhabitantRegions :: Universe -> Region -> Universe
fuseInhabitantRegions u r = go u [r ^. regionId]
  where go u [] = u
        go u (i:is) = if M.member i (u ^. universeRegions) then
                        let (u', newis) = fuseInhabitantRegions' u (u ^?! universeRegions . ix i)
                        in go u' (is ++ newis)
                      else
                        go u is

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
        remainingShapes = region ^. regionShapes \\ c ^. chunkShapes
        isWholeRegion = length remainingShapes == 0
        newId = newRegionId u
        newRegion = region { _regionId = newId, _regionShapes = c ^. chunkShapes }
        remainingRegion = region { _regionShapes = remainingShapes }
        adjustParent r
          = if r ^. regionParentId == region ^. regionId &&
               head (habitat u r) `elem` chunkPieces c then
              r & regionParentId .~ newId
            else
              r

pushRegion :: Universe -> Direction -> Region -> Universe
pushRegion u d r = u & universeRegions . ix (r ^. regionId) . regionPosition +~ directionToVector d

pushChunk :: Universe -> Direction -> Chunk -> Universe
pushChunk u d c = let (r, u') = splitChunkIntoRegion u c
                  in fuseInhabitantRegions (pushRegion u' d r) (u ^?! universeRegions . ix (r ^. regionParentId))
