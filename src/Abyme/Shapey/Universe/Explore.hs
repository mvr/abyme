module Abyme.Shapey.Universe.Explore where

import Data.Map.Strict as M

import Abyme.Direction
import Abyme.Shapey.Universe
import Abyme.Shapey.GameState

data Atlas = Atlas { atlas :: M.Map GameState [GameState] }

atlasSeen :: Atlas -> GameState -> Bool
atlasSeen (Atlas at) gs = M.member gs at

atlasSize :: Atlas -> Int
atlasSize (Atlas at) = M.size at

allMoves :: [GameAction]
allMoves = [Move Up, Move Down, Move LEft, Move RIght, Zoom]

allResults :: GameState -> [GameState]
allResults gs = fmap (flip applyInput gs) allMoves

explore' :: [GameState] -> Atlas -> Atlas
explore' [] at = at
explore' (gs:gss) at | atlasSeen at gs = explore' gss at
explore' (gs:gss) (Atlas m) | otherwise
  = explore' (gss ++ adjacent) newat
  where adjacent = allResults gs
        newat = Atlas $ M.insert gs adjacent m

explore :: GameState -> Atlas
explore gs = explore' [gs] (Atlas M.empty)
