{-# LANGUAGE TypeFamilies #-}
module Abyme.Location where

import Linear

import Abyme.Direction

import Abyme.Universe

-- Every Square sits on a Location,
-- Not every Location has a Square sitting on it

data Square a = Square {
  _squareUniverse :: Universe a,
  _squareChunk :: Chunk a,
  _squareShape :: Shape a,
  _squareCoordinates :: V2 Integer -- relative to Shape
}

data Location a = Location {
  _locationSquare :: Square a,
  _locationSubPosition :: V2 Integer
}

-- TODO: don't loop forever

wrapSubposition :: V2 Integer -> (Bool, V2 Integer)
wrapSubposition (V2 x y) = (x /= wx || y /= wy, V2 wx wy)
  where wx = x `mod` levelScale
        wy = y `mod` levelScale

nudgeSubposition :: Direction -> V2 Integer -> (Bool, V2 Integer)
nudgeSubposition d v = wrapSubposition (v + directionToVector d)

nudgeLocation :: Direction -> Location -> Maybe Location
nudgeLocation d (Location s p)
  = case nudgeSubposition d p of
      (False, p') -> Just (Location s p')
      (True, p') -> do
        s' <- nudgeSquare d s
        return $ Location s' p'

squareLocation :: Square -> Location
squareLocation = undefined

nudgeSquare :: Direction -> Square -> Maybe Square
nudgeSquare d s = do
  l <- nudgeLocation d (squareLocation s)
  inhabitant l

inhabitant :: Location -> Maybe Square
inhabitant = undefined

class HasPosition t where
  position :: Universe -> t -> Location

class HasTerritory t where
  territory :: Universe -> t -> [Location]
