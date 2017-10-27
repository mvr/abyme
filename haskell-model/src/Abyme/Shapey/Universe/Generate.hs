{-# LANGUAGE FlexibleInstances #-}
module Abyme.Shapey.Universe.Generate where

import Data.List (nub, delete)
import qualified Data.Map.Strict as M
import Control.Lens hiding (elements)
import Control.Monad (guard)
import Linear

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Abyme.Direction
import Abyme.Polyomino
import Abyme.Shapey.Universe

monomino :: Polyomino
monomino = Polyomino [V2 0 0]

minimal :: Universe
minimal = Universe (M.fromList [(id1, shape1), (id2, shape2)])
  where id1 = ShapeId 1
        id2 = ShapeId 2
        shape1 = Shape id1 [(id2, V2 0 0)] monomino
        shape2 = Shape id2 [(id1, V2 0 0)] monomino

-- Assuming there isn't already a square there
growShape :: Universe -> Shape -> ((ShapeId, V2 Integer), V2 Integer) -> Universe
growShape u s (parent, n) =
  u & atShape s . shapePolyomino . polyominoSquares %~ (n:)
    & atShape s . shapeParentIds %~ (nub . (parent:))

potentialNewSquares :: Universe -> Shape -> [((ShapeId, V2 Integer), V2 Integer)]
potentialNewSquares u s = do
  (d, s, l') <- haloWithOriginal u s
  guard (not $ isInhabited u l')
  let pos = directionToVector d + s ^. squarePosition
      newparent = l' ^. locationShape
      parentPos = locationToPosition l' - pos
  return $ ((newparent ^. shapeId, parentPos), pos)

-- Assuming the square is uninhabited
growChild :: Universe -> Location -> Universe
growChild u l = u & universeShapes . at newId ?~ newShape
  where newId = newShapeId u
        newShape = Shape newId [(l ^. locationShape . shapeId, locationToPosition l)] monomino

potentialNewChildren :: Universe -> Shape -> [Location]
potentialNewChildren u r = filter (not . isInhabited u) $ constituentLocations u r

-- --------------------------------------------------------------------------------
-- -- Growing

randomShape :: Universe -> Gen Shape
randomShape u = do
  let allShapes = u ^.. universeShapes . traverse
  elements allShapes

randomSquare :: Universe -> Gen Square
randomSquare u = do
  piece <- randomShape u
  elements (constituentSquares u piece)

randomLocation :: Universe -> Gen Location
randomLocation u = do
  square <- randomSquare u
  subp <- elements allSubpositions
  return (Location square subp)

growRandomShape :: Universe -> Gen Universe
growRandomShape u = do
  s <- randomShape u
  let ss = potentialNewSquares u s
  if null ss then
    return u
  else do
    dat <- elements ss
    return $ growShape u s dat

growRandomChild :: Universe -> Gen Universe
growRandomChild u = do
  s <- randomShape u
  let cs = potentialNewChildren u s
  if null cs then
    return u
  else do
    location <- elements cs
    return $ growChild u location

growByOne :: Universe -> Gen Universe
growByOne u = oneof [growRandomShape u, growRandomChild u]

-- --------------------------------------------------------------------------------
-- -- Shrinking

-- TODO: just check parentId
removableShapes :: Universe -> [Shape]
removableShapes u = filter (uninhabited u) allShapes
  where allShapes = u ^.. universeShapes . traverse

removeShape :: Universe -> Shape -> Universe
removeShape u r = u & universeShapes . at (r ^. shapeId) .~ Nothing

pieceRemovableSquares :: Universe -> Shape -> [Square]
pieceRemovableSquares _u (Shape _ _ (Polyomino [_a])) = [] -- We can't leave an empty shape
pieceRemovableSquares u s = filter (uninhabited u) nonInternalBridges
  where nonInternalBridges = fmap (Square s) $ polyRemovableSquares (s ^. shapePolyomino)

allRemovableSquares :: Universe -> [Square]
allRemovableSquares u = do
  let allShapes = u ^.. universeShapes . traverse
  s <- allShapes
  pieceRemovableSquares u s

removeSquare :: Universe -> Square -> Universe
removeSquare u s = u & atShape (s ^. squareShape) . shapePolyomino . polyominoSquares %~ delete (s ^. squarePosition)

-- --------------------------------------------------------------------------------
-- -- Arbitrary

instance Arbitrary (V2 Integer) where
  arbitrary = sized $ \n -> do
    let m = ceiling $ sqrt $ (fromIntegral n :: Double)
    x <- choose (-m, m)
    y <- choose (-m, m)
    return $ V2 x y

instance Arbitrary Direction where
  arbitrary = elements [Up, Down, LEft, RIght]

-- This is dumb and slow
instance Arbitrary Polyomino where
  arbitrary = attempt `suchThat` polyIsConnected
    where attempt = do
            squares <- listOf arbitrary
            return (Polyomino $ nub squares)

  shrink (Polyomino ss) = filter polyIsConnected $ fmap Polyomino $ shrink ss

instance Arbitrary Universe where
  arbitrary = sized $ \n -> do
    if n == 0 then
      return minimal
    else do
      u <- resize (n - 1) arbitrary
      growByOne u

  shrink u = fmap (removeSquare u) (allRemovableSquares u)
             ++ fmap (removeShape u) (removableShapes u)
