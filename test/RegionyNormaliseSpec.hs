module RegionyNormaliseSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Abyme.Regiony.Universe
import Abyme.Regiony.Universe.Normalise
import Abyme.Regiony.Universe.Generate ()

spec :: Spec
spec = describe "normalisation" $ do
        prop "is idempotent for shapes" $ \s ->
          normaliseShape s == normaliseShape (normaliseShape s)
-- TODO:
--        prop "is idempotent for regions" $ 1 == 1
        prop "is idempotent for universes" $ \u ->
          normaliseUniverse u == normaliseUniverse (normaliseUniverse u)
