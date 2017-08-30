module Abyme.Universe.Validate where

import Control.Lens
import Data.List (tails)

import Abyme.Universe
import Abyme.Addressing

-- TODO: return a descriptive error
-- TODO: check ids of key and value match
-- TODO: harder? check universe is connected

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

validateRegion :: Universe -> Region -> Bool
validateRegion u r@(Region rid pid _ ssh) = idsUnequal && shapesDontIntersect && everythingSitsOnSomething
  where idsUnequal = rid /= pid
        shapesDontIntersect = all (not . uncurry shapeIntersects) (pairs ssh)
        everythingSitsOnSomething = all (\s -> Just s == inhabitant u (squareLocation u s)) (constituentSquares u r)

validateUniverse :: Universe -> Bool
validateUniverse u = all (validateRegion u) (u ^.. universeRegions . traverse)
