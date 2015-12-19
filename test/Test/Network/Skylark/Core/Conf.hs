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

parse :: [String] -> Maybe Conf
parse = getParseResult . execParserPure (prefs idm) (parser parseConf)

testGeneral :: TestTree
testGeneral =
  testGroup "General arguments"
    [ testCase "None" $
        parse [] @?= Just confTest
    , testCase "Junk" $
        parse ["junk"] @?= Nothing
    ]

testConfFile :: TestTree
testConfFile =
  testGroup "Conf file argument"
    [ testCase "Short" $
        parse ["-c", "conf/prod.yaml"] @?= Just testOptions
    , testCase "Long" $
        parse ["--conf-file", "conf/prod.yaml"] @?= Just testOptions
    ] where
      testOptions = confTest & confFile .~ Just "conf/prod.yaml"

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
      testOptions = confTest & confPort .~ Just 3000

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
      testOptions = confTest & confTimeout .~ Just 1

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
      testOptions level = confTest & confLogLevel .~ Just level

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
        c <- decodeEnv :: IO (Either String Conf)
        c @?= Right Conf { _confFile     = Nothing
                         , _confPort     = Nothing
                         , _confTimeout  = Nothing
                         , _confLogLevel = Nothing
                         }
    , testCase "Port and Timeout" $ do
        unsetEnv "SKYLARK_CONFFILE"
        setEnv "SKYLARK_PORT" "1"
        setEnv "SKYLARK_TIMEOUT" "1"
        unsetEnv "SKYLARK_LOGLEVEL"
        c <- decodeEnv :: IO (Either String Conf)
        c @?= Right Conf { _confFile     = Nothing
                         , _confPort     = Just 1
                         , _confTimeout  = Just 1
                         , _confLogLevel = Nothing
                         }
     , testCase "String value" $ do
        setEnv "SKYLARK_CONFFILE" "l"
        unsetEnv "SKYLARK_PORT"
        setEnv "SKYLARK_TIMEOUT" "1"
        unsetEnv "SKYLARK_LOGLEVEL"
        c <- decodeEnv :: IO (Either String Conf)
        c @?= Right Conf { _confFile     = Just "l"
                         , _confPort     = Nothing
                         , _confTimeout  = Just 1
                         , _confLogLevel = Nothing
                         }
     , testCase "LevelInfo" $ do
        unsetEnv "SKYLARK_CONFFILE"
        unsetEnv "SKYLARK_PORT"
        unsetEnv "SKYLARK_TIMEOUT"
        setEnv "SKYLARK_LOGLEVEL" "info"
        c <- decodeEnv :: IO (Either String Conf)
        c @?= Right Conf { _confFile     = Nothing
                         , _confPort     = Nothing
                         , _confTimeout  = Nothing
                         , _confLogLevel = Just LevelInfo
                         }
     , testCase "LevelOther" $ do
        unsetEnv "SKYLARK_CONFFILE"
        unsetEnv "SKYLARK_PORT"
        unsetEnv "SKYLARK_TIMEOUT"
        setEnv "SKYLARK_LOGLEVEL" "other"
        c <- decodeEnv :: IO (Either String Conf)
        c @?= Right Conf { _confFile     = Nothing
                         , _confPort     = Nothing
                         , _confTimeout  = Nothing
                         , _confLogLevel = Just (LevelOther "other")
                         }
    ]

testDataFileFetch :: TestTree
testDataFileFetch =
  testGroup "Testing reading of datafile"
    [ testCase "Existing test data file" $ do
        c <- getDataFile "config/testing.yaml" :: IO Conf
        c @?= Conf { _confFile     = Nothing
                   , _confPort     = Just 3031
                   , _confTimeout  = Just 121
                   , _confLogLevel = Just LevelDebug
                   }
    , testCase "Existing data file" $ do
        c <- getDataFile "config/dev.yaml" :: IO Conf
        c @?= Conf { _confFile     = Nothing
                   , _confPort     = Just 3030
                   , _confTimeout  = Just 120
                   , _confLogLevel = Just LevelInfo
                   }
    ]

testConfMonoid :: TestTree
testConfMonoid =
  testGroup "Testing monoid merging of Conf's Maybe fields"
    [ testCase "Both a and b are Nothings" $ do
        let a = confTest
            b = a
        a <> b @?= a
    , testCase "Both a and b have one Just" $ do
        let a = confTest { _confPort = Just 1 }
            b = a
        a <> b @?= a
    , testCase "b has a Just" $ do
        let a = confTest
            b = a { _confPort = Just 1 }
        a <> b @?= b
    , testCase "a has a Just" $ do
        let a = confTest { _confPort = Just 1 }
            b = a { _confPort = Nothing }
        a <> b @?= a
    , testCase "Two separate Just fields are in the merged" $ do
        let a = confTest { _confPort = Just 1 }
            b = a { _confTimeout = Just 120 }
        a <> b @?= a { _confTimeout = Just 120
                     , _confPort    = Just 1
                     }
    ]

testCompleteConf :: TestTree
testCompleteConf =
  testGroup "Test parsing of a complete configuration"
    [ testCase "Sanity test on parsing of configuration" $ do
        unsetEnv "SKYLARK_CONFFILE"
        unsetEnv "SKYLARK_PORT"
        let p = parser parseConf
        c <- getCompleteConf p _confFile :: IO (Either String Conf)
        either (flip assertBool False) ((@?=) $ def {_confPort = Just 3030}) c
    ]

testCompleteConf1 :: TestTree
testCompleteConf1 =
  testGroup "Test parsing with env config"
    [ testCase "Sanity test on parsing of configuration" $ do
        unsetEnv "SKYLARK_CONFFILE"
        setEnv "SKYLARK_PORT" "2222"
        let p = parser parseConf
        c <- getCompleteConf p _confFile :: IO (Either String Conf)
        either (flip assertBool False) ((@?=) $ def {_confPort = Just 2222}) c
    ]

tests :: TestTree
tests =
  testGroup "Options tests"
    [ testGeneral
    , testCompleteConf
    , testCompleteConf1
    , testConfFile
    , testPort
    , testTimeout
    , testLogLevel
    , testEnv
    , testDataFileFetch
    , testConfMonoid
    ]