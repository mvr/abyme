{-# LANGUAGE TemplateHaskell #-}
module Abyme.Polyomino where

import Control.Lens
import Data.List ((\\), delete, intersect)
import Linear

import Abyme.Util
import Abyme.Direction

newtype Polyomino = Polyomino
  {
    _polyominoSquares :: [V2 Integer]
  } deriving (Eq, Show)

makeLenses ''Polyomino

polyContainsPoint :: Polyomino -> V2 Integer -> Bool
polyContainsPoint (Polyomino ps) p = p `elem` ps

polyOffset :: Polyomino -> V2 Integer -> Polyomino
polyOffset (Polyomino ps) p = Polyomino $ fmap (+p) ps

-- Squares that need to be empty to move in that direction
polyFringe :: Direction -> Polyomino -> [V2 Integer]
polyFringe d (Polyomino s) = (fmap (+v) s) \\ s
  where v = directionToVector d

polySquareNeighbourhood :: Polyomino -> V2 Integer -> [V2 Integer]
polySquareNeighbourhood p pos = possible `intersect` (p ^. polyominoSquares)
  where possible = pos : fmap (\d -> pos + directionToVector d) [Up, Down, LEft, RIght]

polyIsConnected :: Polyomino -> Bool
polyIsConnected p = (1==) $ length $ unionize $ fmap (polySquareNeighbourhood p) (p ^. polyominoSquares)

polyRemoveSquare :: Polyomino -> V2 Integer -> Polyomino
polyRemoveSquare (Polyomino ss) pos = Polyomino $ delete pos ss

polyRemovableSquares :: Polyomino -> [V2 Integer]
polyRemovableSquares p = filter (polyIsConnected . polyRemoveSquare p) (p ^. polyominoSquares)
