module Abyme.Util where

import Data.List (intersect, union)
import Linear

levelScale :: Integer
levelScale = 2

unionize :: Eq a => [[a]] -> [[a]]
unionize [] = []
unionize (s:gs) = go [] s gs
  where go as g [] = g : unionize as
        go as g (b:bs) = if null $ intersect g b then
                           go (b:as) g bs
                         else
                           unionize $ [union g b] ++ as ++ bs

posDivMod :: V2 Integer -> (V2 Integer, V2 Integer)
posDivMod (V2 x y) = (V2 q q', V2 r r')
  where (q, r) = x `divMod` levelScale
        (q', r') = y `divMod` levelScale

fromJustOrDie :: String -> Maybe a -> a
fromJustOrDie _ (Just a) = a
fromJustOrDie m Nothing = error m
