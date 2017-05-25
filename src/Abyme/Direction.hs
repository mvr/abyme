module Abyme.Direction where

import Linear

data Direction = Up | Down | LEft | RIght
  deriving (Eq, Ord, Show)

directionToVector :: Num a => Direction -> V2 a
directionToVector Up    = V2 0 (-1)
directionToVector Down  = V2 0 1
directionToVector LEft  = V2 (-1) 0
directionToVector RIght = V2 1 0
