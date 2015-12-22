-- |
-- Module:      Test.Network.Skylark.Core.Test
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Setup module for Skylark Core.

module Test.Network.Skylark.Core.Setup where

import BasicPrelude
import Control.Lens
import Data.Default
import Data.Version
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Setup
import Network.Skylark.Core.Types
import Paths_skylark_core
import Test.Tasty
import Test.Tasty.HUnit

testCtxInit :: TestTree
testCtxInit =
  testGroup "New application context"
    [ testCase "Empty configuration" $
        void (newCtx mempty "" "") `catch` \(MandatoryConfException _) -> return ()
    , testCase "Default configuration" $ do
        i <- getDataFileName "conf/info_testing.yaml" >>= getDataFile
        let d = def & confAppName  .~ Just "Testing"
        t <- newCtx d (txt $ showVersion version) (i ^. ifTag)
        t ^. ctxPreamble   @?= "n=Testing v=0.1.0 t=c0e76c2"
    ]

tests :: TestTree
tests =
  testGroup "Setup tests"
    [ testCtxInit
    ]
