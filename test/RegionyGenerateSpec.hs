module RegionyGenerateSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Abyme.Regiony.Universe
import Abyme.Regiony.Universe.Generate ()
import Abyme.Regiony.Universe.Validate

spec :: Spec
spec = describe "generator" $ do
        prop "generates valid universes" $ \u ->
          validateUniverse u

        prop "shrinks universes to valid universes" $ \u ->
          let shrinks = shrink u in
          not (null shrinks) ==> forAll (elements shrinks) $ validateUniverse
