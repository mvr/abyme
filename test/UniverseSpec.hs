module UniverseSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Abyme.Universe
import Abyme.Universe.Generate
import Abyme.Addressing

spec :: Spec
spec = describe "addressing" $ do
        prop "square occupies its location" $ \u -> do
          s <- randomSquare u
          return $ Just s == inhabitant u (squareLocation u s)
