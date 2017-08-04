module Abyme.Universe.Normalise where

import Control.Lens
import Data.List (sort, sortBy)
import Data.Semigroup ((<>))
import Linear

import Abyme.Polyomino
import Abyme.Universe

-- This should be a normal form
-- Uh oh, would I have to check graph isomorphism?
-- Maybe I can get close enough

normalisePoly :: Polyomino -> (Polyomino, V2 Integer)
normalisePoly (Polyomino ps) = (Polyomino $ fmap (\x -> x - origin) sorted, origin)
  where sorted = sort ps
        origin = head sorted

normaliseShape :: Shape -> Shape
normaliseShape (Shape pos poly) = Shape (pos + origin) newpoly
  where (newpoly, origin) = normalisePoly poly

-- Disregarding the id/parentid
normaliseRegion :: Region -> Region
normaliseRegion (Region rid pid pos shs) = Region rid pid (pos + origin) newshs
  where normalisedshs = sort $ fmap normaliseShape shs
        origin = head normalisedshs ^. shapePosition
        newshs = fmap (\s -> s & shapePosition -~ origin) normalisedshs

compareRegionData :: Region -> Region -> Ordering
compareRegionData (Region _ _ pos shs) (Region _ _ pos' shs')
  = (pos `compare` pos') <> (shs `compare` shs')

normaliseUniverse :: Universe -> Universe
normaliseUniverse u = remapIds idMapping u'
  where u' = u & universeRegions . traverse %~ normaliseRegion
        normalised = sortBy compareRegionData $ u' ^.. universeRegions . traverse
        infIds = fmap RegionId [1..]
        idMapping = zip (normalised ^.. traverse . regionId) infIds
