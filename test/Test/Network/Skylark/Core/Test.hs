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

confTest :: Config
confTest = Config
  { _cConfigFile    = Nothing
  , _cPort          = Nothing
  , _cTimeout       = Nothing
  , _cLogLevel      = Nothing
  }
