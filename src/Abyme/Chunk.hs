{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Abyme.Chunk where

import Control.Lens
import Linear

import Abyme.Shape

data Chunk a = Chunk
  {
    _chunkConstituents :: [Shape a],
    _chunkChildren :: [Shape a]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''Chunk
