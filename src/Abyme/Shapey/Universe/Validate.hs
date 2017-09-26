module Abyme.Shapey.Universe.Validate where

import Control.Lens
import Data.List (tails)

import Abyme.Polyomino
import Abyme.Shapey.Universe

-- TODO: return a descriptive error
-- TODO: check ids of key and value match
-- TODO: harder? check universe is connected

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

validateShape :: Universe -> Shape -> Bool
validateShape u r@(Shape sid pids ssh) = idsUnequal && regionIsConnected && everythingSitsOnSomething
  where idsUnequal = not $ any (\(pid, _) -> pid == sid) pids
        regionIsConnected = polyIsConnected ssh
        everythingSitsOnSomething = all (\s -> Just s == inhabitant u (squareLocation u s)) (constituentSquares u r)

validateUniverse :: Universe -> Bool
validateUniverse u = all (validateShape u) (u ^.. universeShapes . traverse)
