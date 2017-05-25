{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Universe where

import Control.Lens
import qualified Data.Map as M
import Data.Monoid
import Linear

import Abyme.Direction
import Abyme.Polyomino
import Abyme.Chunk

levelScale :: Integer
levelScale = 2

posDivMod :: V2 Integer -> (V2 Integer, V2 Integer)
posDivMod (V2 x y) = (V2 q q', V2 r r')
  where (q, r) = x `divMod` levelScale
        (q', r') = y `divMod` levelScale

newtype ChunkId = ChunkId { getChunkId :: Integer }
  deriving (Eq, Ord, Show)

data Shape = Shape
  {
    _shapeConstituentChunk :: ChunkId,
    _shapePositionInChunk :: V2 Integer,
    _shapeParentChunk :: ChunkId,
    _shapePositionOnParent :: V2 Integer,
    _shapePolyomino :: Polyomino
  } deriving (Eq, Show)

makeLenses ''Shape

data Universe = Universe
  {
--    _universeShapeData :: M.Map a Shape,
    _universeChunkConstituents :: M.Map ChunkId [Shape],
    _universeChunkChildren :: M.Map ChunkId [Shape]
  } deriving (Eq, Show)

makeLenses ''Universe

data Square = Square {
  _squareUniverse :: Universe,
  _squareShape :: Shape,
  _squareCoordinates :: V2 Integer -- relative to shape, must actually be in the poly
} deriving (Eq, Show)
makeLenses ''Square

data Location = Location {
  _locationSquare :: Square,
  _locationSubPosition :: V2 Integer
} deriving (Eq, Show)
makeLenses ''Location

-- Position relative to constituent chunk
shapeContains :: Shape -> V2 Integer -> Bool
shapeContains s p = polyContainsPoint (s^.shapePolyomino) (p - s^.shapePositionInChunk)

findConstituentSquare :: Universe -> ChunkId -> V2 Integer -> Maybe Square
findConstituentSquare u c p = fmap (\s -> Square u s (p - s^.shapePositionInChunk)) maybeSquare
  where maybeSquare = u ^? universeChunkConstituents . at c . _Just . traverse . filtered (flip shapeContains p)

-- Position relative to parent chunk
shapeInhabits :: Shape -> V2 Integer -> Bool
shapeInhabits s p = polyContainsPoint (s^.shapePolyomino) (p - s^.shapePositionOnParent)

findInhabitantSquare :: Universe -> ChunkId -> V2 Integer -> Maybe Square
findInhabitantSquare u c p = fmap (\s -> Square u s (p - s^.shapePositionOnParent)) maybeSquare
  where maybeSquare = u ^? universeChunkChildren . at c . _Just . traverse . filtered (flip shapeInhabits p)

-- This is total unless the Universe is busted
squareLocation :: Square -> Location
squareLocation (Square u s c) = Location newSquare subp
  where (p, subp) = posDivMod c
        newSquare = case findConstituentSquare u (s ^. shapeParentChunk) p of
                      Just x -> x
                      Nothing -> error "Square wasn't sitting on a square in the parent chunk"

inhabitant :: Location -> Maybe Square
inhabitant (Location (Square u s p) subp) = findInhabitantSquare u (s ^. shapeConstituentChunk) (levelScale *^ p + subp)

--------------------------------------------------------------------------------
-- Nudging

wrapSubposition :: V2 Integer -> (Bool, V2 Integer)
wrapSubposition (V2 x y) = (x /= wx || y /= wy, V2 wx wy)
  where wx = x `mod` levelScale
        wy = y `mod` levelScale

nudgeSubposition :: Direction -> V2 Integer -> (Bool, V2 Integer)
nudgeSubposition d v = wrapSubposition (v + directionToVector d)

nudgeLocation' :: First (Direction, Location) -> Direction -> Location -> Maybe Location
nudgeLocation' (First (Just (d', l'))) d l | d == d' && l == l' = Nothing
nudgeLocation' f d l@(Location s p)
  = case nudgeSubposition d p of
      (False, p') -> Just (Location s p')
      (True, p') -> do
        s' <- nudgeSquare' (f <> First (Just (d, l))) d s
        return $ Location s' p'

nudgeLocation :: Direction -> Location -> Maybe Location
nudgeLocation d l = nudgeLocation' (First Nothing) d l

-- TODO: check if we can move along poly
nudgeSquare' :: First (Direction, Location) -> Direction -> Square -> Maybe Square
nudgeSquare' f d s = do
  l <- nudgeLocation' f d (squareLocation s)
  inhabitant l

nudgeSquare :: Direction -> Square -> Maybe Square
nudgeSquare d s = nudgeSquare' (First Nothing) d s

--------------------------------------------------------------------------------
-- Example

minimal :: Universe
minimal = Universe (M.fromList [(cid1, [shape1]), (cid2, [shape2])])
                   (M.fromList [(cid1, [shape2]), (cid2, [shape1])])
mono = Polyomino [V2 0 0]
cid1 = ChunkId 1
cid2 = ChunkId 2
shape1 = Shape cid1 (V2 0 0) cid2 (V2 0 0) mono
shape2 = Shape cid2 (V2 0 0) cid1 (V2 0 0) mono
