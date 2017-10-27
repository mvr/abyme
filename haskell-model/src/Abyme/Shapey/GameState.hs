{-# LANGUAGE TemplateHaskell #-}
module Abyme.Shapey.GameState where

import Control.Lens

import Abyme.Direction
import Abyme.Shapey.Universe
import Abyme.Shapey.Chunk

data GameState = GameState
  {
    _gameUniverse :: Universe,
    _gameActiveChunk :: Chunk
  } deriving (Eq, Show, Ord)
makeLenses ''GameState

data GameAction = Move Direction | Zoom deriving (Eq, Show)

doMove :: Direction -> GameState -> GameState
doMove d gs@(GameState u c)
  | canPushChunk u d c = let (u', c') = pushChunkWithResult u d c in
                           GameState u' c'
  | otherwise = gs

doZoom :: GameState -> GameState
doZoom (GameState u c) = GameState u (chunkParent u c)

applyInput :: GameAction -> GameState -> GameState
applyInput (Move d) gs = doMove d gs
applyInput Zoom gs = doZoom gs
