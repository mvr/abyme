module GenerateSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Abyme.Universe
import Abyme.Universe.Generate ()
import Abyme.Universe.Validate

spec :: Spec
spec = describe "generator" $ do
        prop "generates valid universes" $ \u ->
          validateUniverse u

        prop "shrinks universes to valid universes" $ \u ->
          let shrinks = shrink u in
          not (null shrinks) ==> forAll (elements shrinks) $ validateUniverse
