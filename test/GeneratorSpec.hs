module GeneratorSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Abyme.Universe
import Abyme.Universe.Generator ()
import Abyme.Universe.Validator

spec :: Spec
spec = describe "generator" $ do
        prop "generates valid universes" $ \u ->
          validateUniverse u
