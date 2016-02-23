-- |
-- Module:      Test.Network.Skylark.Core.Conf
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test metrics stuff in StatGrab and Trace for Skylark Core.

module Test.Network.Skylark.Core.Metrics
  ( tests
  ) where

import BasicPrelude
import Network.Skylark.Core.Providers.StatGrab
import Network.Skylark.Core.Types
import Test.Tasty
import Test.Tasty.HUnit

testMetricSerialization :: TestTree
testMetricSerialization =
  testGroup "Sanity check statsd metric serialization"
    [ testCase "Counters" $ do
        let g = "skylark-test.dummyhost"
            b = "counter"
            v = 1
        txt (Counter g b v) @?= "skylark-test.dummyhost.counter:1|c"
    , testCase "Timers" $ do
        let g = "skylark-test.dummyhost"
            b = "timer"
            v = 1
        txt (Timer g b v)  @?= "skylark-test.dummyhost.timer:1.0|t"
    , testCase "Gauges" $ do
        let g = "skylark-test.dummyhost"
            b = "gauge"
            v = 1
        txt (Gauge g b v)  @?= "skylark-test.dummyhost.gauge:1.0|g"
    , testCase "Sets" $ do
        let g = "skylark-test.dummyhost"
            b = "set"
            v = 1
        txt (Set g b v)    @?= "skylark-test.dummyhost.set:1.0|s"
    ]

checkStatList :: [Metric] -> IO ()
checkStatList []     = return ()
checkStatList (m:xs) = case m of
  Gauge {} -> checkStatList xs
  _        -> assertFailure "Host stat metrics are not gauges."

testStatGrab :: TestTree
testStatGrab =
  testGroup "Sanity check that statgrab is available"
    [ testCase "statgrab runs" $
        void $ runStats sampleStats
    , testCase "statsgrab produces gauge metrics" $ do
        s <- runStats sampleStats
        assertBool "Invalid number of stats" $ length (measure "test!" s) >= 10
        checkStatList $ measure "test!" s
    ]

tests :: TestTree
tests =
  testGroup "Metrics tests"
    [ testStatGrab
    , testMetricSerialization
    ]
