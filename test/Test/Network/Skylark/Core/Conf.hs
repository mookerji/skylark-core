{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS  -fno-warn-orphans          #-}

-- |
-- Module:      Test.Network.Skylark.Core.Config
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test configuration Options module for Skylark Core.

module Test.Network.Skylark.Core.Conf
  ( tests
  ) where

import BasicPrelude
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Data.Default
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Types
import Options.Applicative
import System.Environment
import System.Envy
import Test.Network.Skylark.Core.Test
import Test.Tasty
import Test.Tasty.HUnit

parse :: [String] -> Maybe Config
parse = getParseResult . execParserPure (prefs idm) (parser parseConfig)

testGeneral :: TestTree
testGeneral =
  testGroup "General arguments"
    [ testCase "None" $
        parse [] @?= Just confTest
    , testCase "Junk" $
        parse ["junk"] @?= Nothing
    ]

testConfigFile :: TestTree
testConfigFile =
  testGroup "Config file argument"
    [ testCase "Short" $
        parse ["-c", "conf/prod.yaml"] @?= Just testOptions
    , testCase "Long" $
        parse ["--conf-file", "conf/prod.yaml"] @?= Just testOptions
    ] where
      testOptions = confTest & cConfigFile .~ Just "conf/prod.yaml"

testPort :: TestTree
testPort =
  testGroup "Port argument"
    [ testCase "Short" $
        parse ["-p", "3000"] @?= Just testOptions
    , testCase "Long" $
        parse ["--port", "3000"] @?= Just testOptions
    , testCase "Bad integer" $
        parse ["-p", "junk"] @?= Nothing
    ] where
      testOptions = confTest & cPort .~ Just 3000

testTimeout :: TestTree
testTimeout =
  testGroup "Timeout argument"
    [ testCase "Short" $
        parse ["-t", "1"] @?= Just testOptions
    , testCase "Long" $
        parse ["--timeout", "1"] @?= Just testOptions
    , testCase "Bad integer" $
        parse ["-t", "junk"] @?= Nothing
    ] where
      testOptions = confTest & cTimeout .~ Just 1

testLogLevel :: TestTree
testLogLevel =
  testGroup "Log level argument"
    [ testCase "Debug" $
        parse ["--log-level", "debug"] @?= Just (testOptions LevelDebug)
    , testCase "Info" $
        parse ["--log-level", "info"] @?= Just (testOptions LevelInfo)
    , testCase "Warn" $
        parse ["--log-level", "warn"] @?= Just (testOptions LevelWarn)
    , testCase "Error" $
        parse ["--log-level", "error"] @?= Just (testOptions LevelError)
    , testCase "Other" $
        parse ["--log-level", "wut"] @?= Just (testOptions (LevelOther "wut"))
    ] where
      testOptions level = confTest & cLogLevel .~ Just level

--------------------------------------------------------------------------------
-- Environmental parsing stuff

testEnv :: TestTree
testEnv =
  testGroup "Environmental configuration unit test"
    [ testCase "Empty configuration" $ do
        unsetEnv "SKYLARK_CONFFILE"
        unsetEnv "SKYLARK_PORT"
        unsetEnv "SKYLARK_TIMEOUT"
        unsetEnv "SKYLARK_LOGLEVEL"
        c <- decodeEnv :: IO (Either String Config)
        c @?= Right Config { _cConfigFile = Nothing
                           , _cPort       = Nothing
                           , _cTimeout    = Nothing
                           , _cLogLevel   = Nothing
                           }
    , testCase "Port and Timeout" $ do
        unsetEnv "SKYLARK_CONFFILE"
        setEnv "SKYLARK_PORT" "1"
        setEnv "SKYLARK_TIMEOUT" "1"
        unsetEnv "SKYLARK_LOGLEVEL"
        c <- decodeEnv :: IO (Either String Config)
        c @?= Right Config { _cConfigFile = Nothing
                           , _cPort       = Just 1
                           , _cTimeout    = Just 1
                           , _cLogLevel   = Nothing
                           }
     , testCase "String value" $ do
        setEnv "SKYLARK_CONFFILE" "l"
        unsetEnv "SKYLARK_PORT"
        setEnv "SKYLARK_TIMEOUT" "1"
        unsetEnv "SKYLARK_LOGLEVEL"
        c <- decodeEnv :: IO (Either String Config)
        c @?= Right Config { _cConfigFile = Just "l"
                           , _cPort       = Nothing
                           , _cTimeout    = Just 1
                           , _cLogLevel   = Nothing
                           }
     , testCase "LevelInfo" $ do
        unsetEnv "SKYLARK_CONFFILE"
        unsetEnv "SKYLARK_PORT"
        unsetEnv "SKYLARK_TIMEOUT"
        setEnv "SKYLARK_LOGLEVEL" "info"
        c <- decodeEnv :: IO (Either String Config)
        c @?= Right Config { _cConfigFile = Nothing
                           , _cPort       = Nothing
                           , _cTimeout    = Nothing
                           , _cLogLevel   = Just LevelInfo
                           }
     , testCase "LevelOther" $ do
        unsetEnv "SKYLARK_CONFFILE"
        unsetEnv "SKYLARK_PORT"
        unsetEnv "SKYLARK_TIMEOUT"
        setEnv "SKYLARK_LOGLEVEL" "other"
        c <- decodeEnv :: IO (Either String Config)
        c @?= Right Config { _cConfigFile = Nothing
                           , _cPort       = Nothing
                           , _cTimeout    = Nothing
                           , _cLogLevel   = Just (LevelOther "other")
                           }
    ]

testDataFileFetch :: TestTree
testDataFileFetch =
  testGroup "Testing reading of datafile"
    [ testCase "Existing test data file" $ do
        c <- getDataFile "config/testing.yaml" :: IO Config
        c @?= Config { _cConfigFile = Nothing
                     , _cPort       = Just 3031
                     , _cTimeout    = Just 121
                     , _cLogLevel   = Just LevelDebug
                     }
    , testCase "Existing data file" $ do
        c <- getDataFile "config/dev.yaml" :: IO Config
        c @?= Config { _cConfigFile = Nothing
                     , _cPort       = Just 3030
                     , _cTimeout    = Just 120
                     , _cLogLevel   = Just LevelInfo
                     }
    ]

testConfigMonoid :: TestTree
testConfigMonoid =
  testGroup "Testing monoid merging of Config's Maybe fields"
    [ testCase "Both a and b are Nothings" $ do
        let a = confTest
            b = a
        a `mappend` b @?= a
    , testCase "Both a and b have one Just" $ do
        let a = confTest { _cPort = Just 1 }
            b = a
        a `mappend` b @?= a
    , testCase "b has a Just" $ do
        let a = confTest
            b = a { _cPort = Just 1 }
        a `mappend` b @?= b
    , testCase "a has a Just" $ do
        let a = confTest { _cPort = Just 1 }
            b = a { _cPort = Nothing }
        a `mappend` b @?= a
    , testCase "Two separate Just fields are in the merged" $ do
        let a = confTest { _cPort = Just 1 }
            b = a { _cTimeout = Just 120 }
        a `mappend` b @?= a { _cTimeout = Just 120
                            , _cPort = Just 1
                            }
    ]

testCompleteConfig :: TestTree
testCompleteConfig =
  testGroup "Test parsing of a complete configuration"
    [ testCase "Sanity test on parsing of configuration" $ do
        let p = parser parseConfig
        c <- getCompleteConfig p _cConfigFile :: IO (Either String Config)
        either (flip assertBool False) ((@?=) def) c
    ]

testCompleteConfig1 :: TestTree
testCompleteConfig1 =
  testGroup "Test parsing with env config"
    [ testCase "Sanity test on parsing of configuration" $ do
        setEnv "SKYLARK_PORT" "2222"
        let p = parser parseConfig
        c <- getCompleteConfig p _cConfigFile :: IO (Either String Config)
        either (flip assertBool False) ((@?=) $ def {_cPort = Just 2222}) c
    ]

testCompleteConfig2 :: TestTree
testCompleteConfig2 =
  testGroup "Test parsing with env config and alternative config file"
    [ testCase "Sanity test on parsing of configuration" $ do
        setEnv "SKYLARK_CONFFILE" "./config/testing.yaml"
        unsetEnv "SKYLARK_PORT"
        let p   = parser parseConfig
            ans = Config { _cConfigFile = Just "./config/testing.yaml"
                         , _cPort       = Just 3031
                         , _cTimeout    = Just 121
                         , _cLogLevel   = Just LevelDebug
                         }
        c <- getCompleteConfig p _cConfigFile :: IO (Either String Config)
        either (flip assertBool False) ((@?=) ans) c
    ]

tests :: TestTree
tests =
  testGroup "Options tests"
    [ testGeneral
    , testCompleteConfig
    , testCompleteConfig1
    , testCompleteConfig2
    , testConfigFile
    , testPort
    , testTimeout
    , testLogLevel
    , testEnv
    , testDataFileFetch
    , testConfigMonoid
    ]
