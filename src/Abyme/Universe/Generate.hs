{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Abyme.Universe.Generate where

import Data.List (delete, nub)
import qualified Data.Map.Strict as M
import Control.Monad (guard)
import Control.Lens hiding (elements)
import Linear

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Abyme.Util (levelScale)
import Abyme.Polyomino
import Abyme.Universe
import Abyme.Addressing
import Abyme.Chunk

import Debug.Trace

monomino :: Shape
monomino = Shape (V2 0 0) (Polyomino [V2 0 0])

minimal :: Universe
minimal = Universe (M.fromList [(id1, region1), (id2, region2)])
  where id1 = RegionId 1
        id2 = RegionId 2
        region1 = Region id1 id2 (V2 0 0) [monomino]
        region2 = Region id2 id1 (V2 0 0) [monomino]

-- Assuming there isn't already a square there
growPiece :: Universe -> Piece -> V2 Integer -> Universe
growPiece u p n = fuseInhabitantRegions added (regionParent added (p ^. pieceRegion))
  where added = u & atPiece p . shapePolyomino . polyominoSquares %~ (n:)

potentialNewSquares :: Universe -> Region -> [(Piece, V2 Integer)]
potentialNewSquares u r = do
  p <- regionPieces r
  l <- halo u p
  guard (not $ isInhabited u l)
  return (p, locationToPosition l)

-- TODO: broken?
locationToPosition :: Location -> V2 Integer
locationToPosition (Location (Square _ p) subp) = levelScale *^ p + subp

-- Assuming the square is uninhabited
growChild :: Universe -> Location -> Universe
growChild u l = fuseInhabitantRegions (u & universeRegions . at newId ?~ newRegion) locationRegion
  where newId = newRegionId u
        locationRegion = l ^. locationSquare . squarePiece . pieceRegion
        newRegion = Region newId (locationRegion ^. regionId) (locationToPosition l) [monomino]

potentialNewChildren :: Universe -> Region -> [Location]
potentialNewChildren u r = filter (not . isInhabited u) $ constituentLocations u r

-- --------------------------------------------------------------------------------
-- -- Growing

randomRegion :: Universe -> Gen Region
randomRegion u = do
  let allRegions = u ^.. universeRegions . traverse
  elements allRegions

randomPiece :: Universe -> Gen Piece
randomPiece u = do
  region <- randomRegion u
  elements (regionPieces region)

growRandomPiece :: Universe -> Gen Universe
growRandomPiece u = do
  r <- randomRegion u
  let ss = potentialNewSquares u r
  if null ss then
    return u
  else do
    (piece, pos) <- elements ss
    return $ growPiece u piece pos

growRandomChild :: Universe -> Gen Universe
growRandomChild u = do
  r <- randomRegion u
  let cs = potentialNewChildren u r
  if null cs then
    return u
  else do
    location <- elements cs
    return $ growChild u location

growByOne :: Universe -> Gen Universe
growByOne u = oneof [growRandomPiece u, growRandomChild u]

-- --------------------------------------------------------------------------------
-- -- Shrinking

-- TODO: just check parentId
removableRegions :: Universe -> [Region]
removableRegions u = filter (uninhabited u) allRegions
  where allRegions = u ^.. universeRegions . traverse

removeRegion :: Universe -> Region -> Universe
removeRegion u r = u & universeRegions . at (r ^. regionId) .~ Nothing

pieceRemovableSquares :: Universe -> Piece -> [Square]
pieceRemovableSquares u p = fmap (Square p) $ polyRemovableSquares (p ^. pieceShape ^. shapePolyomino)

allRemovableSquares :: Universe -> [Square]
allRemovableSquares u = do
  let allRegions = u ^.. universeRegions . traverse
  region <- allRegions
  pieces <- regionPieces region
  pieceRemovableSquares u pieces

removeSquare :: Universe -> Square -> Universe
removeSquare u s = u & atPiece (s ^. squarePiece) . shapePolyomino . polyominoSquares %~ delete (s ^. squareCoordinates)

-- --------------------------------------------------------------------------------
-- -- Arbitrary

instance Arbitrary (V2 Integer) where
  arbitrary = sized $ \n -> do
    let m = ceiling $ sqrt $ (fromIntegral n :: Double)
    x <- choose (-m, m)
    y <- choose (-m, m)
    return $ V2 x y

instance Arbitrary Polyomino where
  arbitrary = attempt `suchThat` polyIsConnected
    where attempt = do
            squares <- listOf arbitrary
            return (Polyomino $ nub squares)

  shrink (Polyomino ss) = filter polyIsConnected $ fmap Polyomino $ shrink ss

instance Arbitrary Shape where
  arbitrary = Shape <$> arbitrary <*> arbitrary

-- instance Arbitrary Region where

instance Arbitrary Universe where
  arbitrary = sized $ \n -> do
    if n == 0 then
      return $ minimal
    else do
      u <- resize (n - 1) arbitrary
      growByOne u

  -- shrink u = fmap (removeSquare u) (allRemovableSquares u)
  --            ++ fmap (removeRegion u) (removableRegions u)
