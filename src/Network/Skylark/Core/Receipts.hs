-- |
-- Module:      Network.Skylark.Core.Receipts
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Receipts module for Skylark Core.

module Network.Skylark.Core.Receipts
  ( generate
  ) where

import Data.UUID.V4
import Data.Time
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types

generate :: IO Receipt
generate = Receipt <$> nextRandom <*> getCurrentTime
