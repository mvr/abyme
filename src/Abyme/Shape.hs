{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Shape where

import Control.Lens
import Linear

import Abyme.Polyomino

data Shape a = Shape
  {
    _shapeData :: a,
    _shapePositionInChunk :: V2 Integer,
    _shapePositionOnParent :: V2 Integer,
    _shapePolyomino :: Polyomino
  } deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''Shape

-- Relative to chunk
shapeContainsPoint :: Shape a -> V2 Integer -> Bool
shapeContainsPoint (Shape _ chunkPos _ poly) point = polyContainsPoint (polyOffset poly chunkPos) point
