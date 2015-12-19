-- |
-- Module:      Test.Network.Skylark.Core.Test
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test module for Skylark Core.

module Test.Network.Skylark.Core.Test where

import BasicPrelude
import Network.Skylark.Core.Types

confTest :: Conf
confTest = Conf
  { _confFile          = Nothing
  , _confPort          = Nothing
  , _confTimeout       = Nothing
  , _confLogLevel      = Nothing
  }
