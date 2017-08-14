module Abyme.Universe.Validate where

import Control.Lens
import Data.List (tails)

import Abyme.Universe

-- TODO: return a descriptive error
-- TODO: check ids of key and value match
-- TODO: check every square is sitting validly on parent region
-- TODO: harder? check universe is connected

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

validateRegion :: Region -> Bool
validateRegion (Region rid pid _ ssh) = rid /= pid && all (not . uncurry shapeIntersects) (pairs ssh)

validateUniverse :: Universe -> Bool
validateUniverse u = all validateRegion (u ^.. universeRegions . traverse)
