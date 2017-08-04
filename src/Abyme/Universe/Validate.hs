module Abyme.Universe.Validate where

import Control.Lens
import Data.List (tails)

import Abyme.Universe

-- TODO: return an error
-- TODO: check ids of key and value match

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

validateRegion :: Region -> Bool
validateRegion (Region rid pid _ ssh) = rid /= pid && all (not . uncurry shapeIntersects) (pairs ssh)

validateUniverse :: Universe -> Bool
validateUniverse u = all validateRegion (u ^.. universeRegions . traverse)
