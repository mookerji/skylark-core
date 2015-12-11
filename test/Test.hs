-- |
-- Module:      Test
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test module for Skylark Core.

import           BasicPrelude
import qualified Test.Network.Skylark.Core.Config as Config
import           Test.Tasty

tests :: TestTree
tests = testGroup "Tests"
  [ Config.tests
  ]

main :: IO ()
main = defaultMain tests
