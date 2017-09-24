{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Shapey.Chunk (
  Chunk(..),
  shapeChunk,
  canPushChunk,
  exploreShape,
--  pushChunk,
) where

import Control.Lens hiding (contains)
import Control.Monad.State
import Data.List (nub, find, deleteBy, partition)
import Data.Function (on)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Map.Strict as M
import Linear

import Abyme.Direction
import Abyme.Util (levelScale, unionize, fromJustOrDie)
import Abyme.Shapey.Universe

-- --------------------------------------------------------------------------------
-- -- Chunks

data Chunk = Chunk {
  _chunkTopShapes :: [Shape],
  _chunkSubChunks :: [Shape]
} deriving (Eq, Show)
makeLenses ''Chunk

instance HasSquares Chunk where
  constituentSquares u c = concat $ fmap (constituentSquares u) (c ^. chunkTopShapes)

chunkHasTopPiece :: Chunk -> Shape -> Bool
chunkHasTopPiece (Chunk ss _) s = s `elem` ss

-- Remember a depth so we don't explore upwards
exploreShape' :: Universe -> [(Shape, Int)] -> [(Shape, Int)] -> ([(Shape, Int)], [(Shape, Int)])
exploreShape' _ m [] = (m, [])
exploreShapen' u m ((s, d):rest)
  | d < 0      = exploreShape' u m rest -- We are back at the top level
  | Just d' <- lookup s m,
    d' <= d    = exploreShape' u m rest -- We have already seen this piece
  | otherwise  = exploreShape' u m' (parents ++ rest ++ children)
  where children = fmap (,d+1) $ childShapes u s
        parents  = fmap (,d-1) $ habitat u s
        m'       = ((s, d):) $ deleteBy ((==) `on` fst) (s, d) m

exploreShape :: Universe -> Shape -> [(Shape, Int)]
exploreShape u p = fst $ exploreShape' u [] [(p, 0)]

shapeChunk :: Universe -> Shape -> Chunk
shapeChunk u s = Chunk (fmap fst top) (fmap fst rest)
  where m = exploreShape u s
        (top, rest) = partition (\(_, d) -> d == 0) m

-- -- --------------------------------------------------------------------------------
-- -- -- Fusing

canPushChunk :: Universe -> Direction -> Chunk -> Bool
canPushChunk u d c = not (oob || any (isInhabited u) fr)
  where (fr, oob) = fringe u d c

pushSingleShape :: Universe -> Direction -> Shape -> Universe
pushSingleShape u d s = u & atShape s . shapeParentIds .~ (newParents & traverse . _2 +~ directionToVector d)
  where newParents = nub $ do
          sq <- constituentSquares u s
          l' <- maybeToList $ nudgeLocation u d $ squareLocation u sq
          let pos = directionToVector d + sq ^. squarePosition
              newParent = l' ^. locationShape
              parentPos = locationToPosition l' - pos
          return (newParent ^. shapeId, parentPos)

pushChunk :: Universe -> Direction -> Chunk -> Universe
pushChunk u d c = undefined
