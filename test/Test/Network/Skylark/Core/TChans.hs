-- |
-- Module:      Test.Network.Skylark.Core.TChans
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test send and receive TChans module for Skylark Core.

module Test.Network.Skylark.Core.TChans
  ( tests
  ) where

import Control.Concurrent.STM
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.TChans
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

testWriterReader :: TestTree
testWriterReader =
  testGroup "Bo tests"
    [ testProperty "Empty write chan" $
        \count -> monadicIO $ do
          a <- run $ atomically $ do
            wc <- newTWChan count
            rc <- newTRChan wc
            tryReadTRChan rc
          assert $ (a :: Maybe Int) == Nothing
    , testProperty "0 length write chan" $
        \n -> monadicIO $ do
          pre $ n > 0
          a <- run $ atomically $ do
            wc <- newTWChan 0
            rc <- newTRChan wc
            forM_ (take n [1..]) (writeTWChan wc)
            catMaybes <$> replicateM n (tryReadTRChan rc)
          assert $ (a :: [Int]) == []
    , testProperty "1 length write chan with n writes" $
        \n -> monadicIO $ do
          pre $ n > 0
          as <- run $ atomically $ do
            wc <- newTWChan 1
            rc <- newTRChan wc
            forM_ (take n [1..]) (writeTWChan wc)
            catMaybes <$> replicateM n (tryReadTRChan rc)
          assert $ (as :: [Int]) == [n]
    , testProperty "n length write chan with less than n writes" $
        \n m -> monadicIO $ do
          pre $ n > 0
          pre $ m <= n
          as <- run $ atomically $ do
            wc <- newTWChan $ fromIntegral n
            rc <- newTRChan wc
            forM_ (take m [1..]) (writeTWChan wc)
            catMaybes <$> replicateM m (tryReadTRChan rc)
          assert $ (as :: [Int]) == take m [1..]
    , testProperty "n length write chan with greater then n writes" $
        \n m -> monadicIO $ do
          pre $ n > 0
          pre $ m > n
          as <- run $ atomically $ do
            wc <- newTWChan $ fromIntegral n
            rc <- newTRChan wc
            forM_ (take m [1..]) (writeTWChan wc)
            catMaybes <$> replicateM m (tryReadTRChan rc)
          assert $ (as :: [Int]) == take n (drop (m - n) [1..])
    ]

tests :: TestTree
tests =
  testGroup "TChans tests"
    [ testWriterReader
    ]

