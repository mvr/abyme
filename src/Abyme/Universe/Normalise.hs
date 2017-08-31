module Abyme.Universe.Normalise where

import Control.Lens
import Data.List (sort, sortBy, (\\))
import Data.Semigroup ((<>))
import Linear

import Abyme.Polyomino
import Abyme.Universe
import Abyme.Addressing

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
normaliseRegionData :: Region -> Region
normaliseRegionData (Region rid pid pos shs) = Region rid pid (pos + origin) newshs
  where normalisedshs = sort $ fmap normaliseShape shs
        origin = head normalisedshs ^. shapePosition
        newshs = fmap (\s -> s & shapePosition -~ origin) normalisedshs

compareRegionData :: Region -> Region -> Ordering
compareRegionData (Region _ _ pos shs) (Region _ _ pos' shs')
  = (pos `compare` pos') <> (shs `compare` shs')

-- A lot of this should short-circuit unless the universe is very symmetrical
compareRegion' :: Universe -> [Region] -> [Region] -> Region -> Region -> Ordering
compareRegion' u r1seen r2seen r1 r2 = (r1 `compareRegionData` r2)
                                       <> (length r1Children `compare` length r2Children)
                                       <> mconcat (fmap subcompare $ zip sortedr1Children sortedr2Children)
  where r1Children = childRegions u r1 \\ r1seen
        r2Children = childRegions u r2 \\ r2seen
        sortedr1Children = sortBy (compareRegion' u (r1:r1seen) r2seen) r1Children
        sortedr2Children = sortBy (compareRegion' u r1seen (r2:r2seen)) r2Children
        subcompare (r1', r2') = compareRegion' u (r1:r1seen) (r2:r2seen) r1' r2'

compareRegion :: Universe -> Region -> Region -> Ordering
compareRegion u r1 r2 = compareRegion' u [] [] r1 r2

normaliseUniverse :: Universe -> Universe
normaliseUniverse u = remapIds idMapping u'
  where u' = u & universeRegions . traverse %~ normaliseRegionData
        normalised = sortBy (compareRegion u') $ u' ^.. universeRegions . traverse
        infIds = fmap RegionId [1..]
        idMapping = zip (normalised ^.. traverse . regionId) infIds
