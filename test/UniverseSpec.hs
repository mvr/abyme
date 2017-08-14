module UniverseSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

import Abyme.Universe
import Abyme.Universe.Generate
import Abyme.Universe.Normalise
import Abyme.Addressing
import Abyme.Chunk

spec :: Spec
spec = do
  describe "addressing" $ do
    prop "square occupies its location" $ \u -> do
      s <- randomSquare u
      return $ Just s === inhabitant u (squareLocation u s)

  describe "splitting" $ do
    prop "splitting then unsplitting is identity" $ \u -> do
      s <- randomPiece u
      let c = findChunk u s
      let (r, u') = splitChunkIntoRegion u c
      return $ normaliseUniverse u === normaliseUniverse (fuseInhabitantRegions u' (regionParent u r))
