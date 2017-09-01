{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Chunk (
  Chunk(..),
  pieceChunk,
  fuseInhabitantRegions,
  canPushChunk,
  explorePiece,
  isolateChunk,
--  pushChunk,
) where

import Control.Lens hiding (contains)
import Control.Monad.State
import Data.List (nub, find, (\\), groupBy, delete)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import Linear

import Abyme.Direction
import Abyme.Universe
import Abyme.Addressing
import Abyme.Util (levelScale, unionize, fromJustOrDie)

-- --------------------------------------------------------------------------------
-- -- Chunks

data Chunk = Chunk {
  _chunkRegion :: Region,
  _chunkShapes :: [Shape],
  _chunkSubChunks :: M.Map RegionId [[Shape]]
} deriving (Eq, Show)
makeLenses ''Chunk

instance HasSquares Chunk where
  constituentSquares u c = concat $ fmap (constituentSquares u) (chunkTopPieces c)

chunkTopPieces :: Chunk -> [Piece]
chunkTopPieces (Chunk r ss _) = fmap (Piece r) ss

chunkHasTopPiece :: Chunk -> Piece -> Bool
chunkHasTopPiece (Chunk r ss _) (Piece r' s) = r == r' && s `elem` ss

mapHasPiece :: M.Map RegionId [(Int, Shape)] -> Piece -> Maybe Int
mapHasPiece m (Piece r s) = case M.lookup (r^.regionId) m of
    Just ss -> fmap fst $ find (\(d, s') -> s == s') ss
    Nothing -> Nothing

-- Remember a depth so we don't explore upwards
explorePiece' :: Universe -> M.Map RegionId [(Int, Shape)] -> [(Int, Piece)] -> (M.Map RegionId [(Int, Shape)], [(Int, Piece)])
explorePiece' _ m [] = (m, [])
explorePiece' u m ((d, p):ps)
  | d < 0      = explorePiece' u m ps -- We are back at the top level
  | Just d' <- mapHasPiece m p,
    d' <= d    = explorePiece' u m ps -- We have already seen this piece
  | otherwise  = explorePiece' u m' (parents ++ ps ++ children)
  where children = fmap (d+1,) $ childPieces u p
        parents  = fmap (d-1,) $ habitat u p
        s        = p ^. pieceShape
        m'       = m & at (p^.pieceRegion.regionId) . non [] %~ ((d, s):) . filter (\(d', s') -> s' /= s)

explorePiece :: Universe -> Piece -> (M.Map RegionId [(Int, Shape)])
explorePiece u p = fst $ explorePiece' u M.empty [(0,p)]

deleteComplete :: Universe -> M.Map RegionId [[Shape]] -> M.Map RegionId [[Shape]]
deleteComplete u m = M.filterWithKey isIncomplete m
  where isIncomplete rid [ss] = not $ null $ u ^?! universeRegions . ix rid . regionShapes \\ ss
        isIncomplete rid _    = True

-- TODO: maybe each region needs to store this kind of adjacency information
identifyIslands :: Universe -> RegionId -> [(Int, Shape)] -> [[Shape]]
identifyIslands u rid ss = concatMap doLevel $ fmap (fmap snd) $ groupBy ((==) `on` fst) ss
  where doLevel ss' = regionCollectAdjacentShapes u (u ^?! universeRegions . ix rid) ss'

pieceChunk :: Universe -> Piece -> Chunk
pieceChunk u p = Chunk r top (deleteComplete u $ M.mapWithKey (identifyIslands u) m)
  where r = p ^. pieceRegion
        m = explorePiece u p
        top = fmap snd $ filter (\(d, _) -> d == 0) $ (m ^?! ix (r ^. regionId))

-- --------------------------------------------------------------------------------
-- -- Fusing

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
adjacentRegions u r = filter (\r' -> r ^. regionParentId == r' ^. regionParentId ) $ nub $ fmap (_pieceRegion) $ neighbourhood u r

collectRegionAdjacents :: Universe -> [Region] -> [[Region]]
collectRegionAdjacents u rs = unionize $ fmap (adjacentRegions u) $ rs

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
        chunks = collectRegionAdjacents u children
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

-- --------------------------------------------------------------------------------
-- -- Splitting

canPushChunk :: Universe -> Direction -> Chunk -> Bool
canPushChunk u d c = not (oob || any (isInhabited u) fr)
  where (fr, oob) = fringe u d c

-- This just fills in the missing parts of the region from the Chunk
bitsToShapes :: Universe -> RegionId -> [[Shape]] -> [[Shape]]
bitsToShapes u rid ss = ss ++ regionCollectAdjacentShapes u region missing
  where region = u ^?! universeRegions . ix rid
        missing = region ^. regionShapes \\ concat ss

lookupRemap m p = M.findWithDefault (p ^. pieceRegion . regionId) (p ^. pieceRegion . regionId, p ^. pieceShape) m

constructUpdates :: Universe -> Chunk -> (M.Map RegionId Region, M.Map (RegionId, Shape) RegionId)
constructUpdates u (Chunk _ _ m) = (regionMap, idRemap)
  where shapes = concatMap (\(r, ss) -> fmap (r,) ss) $ M.toList $ M.mapWithKey (bitsToShapes u) m
        newIds = zip shapes [newRegionId u .. ]

        idRemap =  M.fromList $ concatMap (\((oldrid, ss), newrid) -> fmap (\s -> ((oldrid, s), newrid)) ss) newIds

        buildRegion ((oldrid, ss), newrid)
          = (newrid, oldr { _regionId = newrid,
                            _regionParentId = lookupRemap idRemap (head $ habitat u $ Piece oldr $ head ss),
                            _regionShapes = ss } )
          where oldr = u ^?! universeRegions . ix oldrid

        regionMap = M.fromList $ fmap buildRegion newIds

-- chunkChildShapes :: Chunk -> [(RegionId, [Shape])]
-- chunkChildShapes (Chunk r _ m) = concatMap (\(r, ss) -> fmap (r,) ss) $ (M.toList m)

isolateChunk :: Universe -> Chunk -> (Region, Universe)
isolateChunk u c = (newu ^?! universeRegions . ix newFocusId , newu)
  where (newRegions, idRemap) = constructUpdates u c

        oldIds = M.keys (c ^. chunkSubChunks)
        oldDeleted = M.filterWithKey (\k _ -> k `notElem` oldIds) (u ^. universeRegions)
        adjustParent r = r & regionParentId .~ lookupRemap idRemap (head $ habitat u r)

        newu = Universe $ M.union (fmap adjustParent oldDeleted) newRegions

        newFocusId = lookupRemap idRemap (head $ constituentPieces u c)

-- pushRegion :: Universe -> Direction -> Region -> Universe
-- pushRegion u d r = u & universeRegions . ix (r ^. regionId) . regionPosition +~ directionToVector d

-- pushChunk :: Universe -> Direction -> Chunk -> (Universe, Chunk)
-- pushChunk u d c = (u'', undefined)
--   where (r, u') = splitChunkIntoRegion u c
--         u'' = fuseInhabitantRegions (pushRegion u' d r) (u ^?! universeRegions . ix (r ^. regionParentId))
