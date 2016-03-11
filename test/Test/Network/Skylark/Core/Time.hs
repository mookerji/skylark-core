-- |
-- Module:      Test.Network.Skylark.Core.Time
-- Copyright:   (c) 2015 Joshua Gross
-- License:     BSD3
-- Maintainer:  Joshua Gross <josh@swift-nav.com>
--
-- Test time parse / format.
{-# OPTIONS  -fno-warn-orphans          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Network.Skylark.Core.Time
  ( tests
  ) where

import Data.Time
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Time
import Network.Skylark.Core.Types
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary UTCTime where
    arbitrary = do
      randomDay   <- choose (1, 29) :: Gen Int
      randomMonth <- choose (1, 12) :: Gen Int
      randomYear  <- choose (1980, 2050) :: Gen Integer
      randomTime  <- choose (0, 86401) :: Gen Int
      return $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)

testUntxt :: TestTree
testUntxt =
  testGroup "UnTxt tests"
    [ testProperty "ISO8601" $
        \t -> monadicIO $ do
          let isoTime :: ISO8601 = ISO8601 t
          t' <- run $ untxt (txt isoTime)
          assert $ (t :: UTCTime) == (toUtcTime (t' :: ISO8601) :: UTCTime)
    ]

tests :: TestTree
tests =
  testGroup "UnTxt tests"
    [ testUntxt
    ]
