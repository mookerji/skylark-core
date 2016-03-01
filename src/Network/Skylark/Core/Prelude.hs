-- |
-- Module:      Network.Skylark.Core.Prelude
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Prelude module for Skylark Core.

module Network.Skylark.Core.Prelude
  ( module Exports
  , fold'
  , foldM'
  , maybe'
  ) where

import BasicPrelude as Exports
import Data.UUID    as Exports (UUID, fromASCIIBytes, toASCIIBytes)
import Formatting   as Exports (float, int, sformat, stext, (%))

fold' :: Foldable t => t a -> b -> (b -> a -> b) -> b
fold' as b f = foldl' f b as

foldM' :: (Monad m, Foldable t) => t a -> b -> (b -> a -> m b) -> m b
foldM' as b f = foldM f b as

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m b a = maybe b a m
