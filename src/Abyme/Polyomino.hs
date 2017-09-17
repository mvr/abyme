{-# LANGUAGE TemplateHaskell #-}
module Abyme.Polyomino where

import Control.Lens
import Data.List ((\\), delete, intersect, nub)
import Data.Semigroup
import Linear

import Abyme.Util
import Abyme.Direction

-- TODO: Set
newtype Polyomino = Polyomino
  {
    _polyominoSquares :: [V2 Integer]
  } deriving (Eq, Show, Ord)
makeLenses ''Polyomino

instance Semigroup Polyomino where
  (Polyomino p) <> (Polyomino p') = Polyomino $ nub (p ++ p')

polyContainsPoint :: Polyomino -> V2 Integer -> Bool
polyContainsPoint (Polyomino ps) p = p `elem` ps

polyOffset :: Polyomino -> V2 Integer -> Polyomino
polyOffset (Polyomino ps) p = Polyomino $ fmap (+p) ps

-- Squares that need to be empty to move in that direction
polyFringe :: Direction -> Polyomino -> [V2 Integer]
polyFringe d (Polyomino s) = (fmap (+v) s) \\ s
  where v = directionToVector d

polyIntersects :: Polyomino -> Polyomino -> Bool
polyIntersects (Polyomino ps) (Polyomino ps') = ps `intersects` ps'
  where intersects as bs = any (`elem` bs) as

polySquareNeighbourhood :: Polyomino -> V2 Integer -> [V2 Integer]
polySquareNeighbourhood p pos = possible `intersect` (p ^. polyominoSquares)
  where possible = pos : fmap (\d -> pos + directionToVector d) [Up, Down, LEft, RIght]

polyIsConnected :: Polyomino -> Bool
polyIsConnected p = (1==) $ length $ unionize $ fmap (polySquareNeighbourhood p) (p ^. polyominoSquares)

polyRemoveSquare :: Polyomino -> V2 Integer -> Polyomino
polyRemoveSquare (Polyomino ss) pos = Polyomino $ delete pos ss

polyRemovableSquares :: Polyomino -> [V2 Integer]
polyRemovableSquares p = filter (polyIsConnected . polyRemoveSquare p) (p ^. polyominoSquares)

polyDiv :: Polyomino -> Polyomino
polyDiv (Polyomino ps) = Polyomino $ nub $ fmap (fst . posDivMod) ps
