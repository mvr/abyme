{-# LANGUAGE TemplateHaskell #-}
module Abyme.Polyomino where

import Control.Lens
import Data.List ((\\))
import Linear

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
