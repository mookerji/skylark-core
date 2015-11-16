-- |
-- Module:      Network.Skylark.Core.Prelude
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Prelude module for Skylark Core.

module Network.Skylark.Core.Prelude
  ( module BasicPrelude
  , maybe'
  ) where

import BasicPrelude

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m b a = maybe b a m
