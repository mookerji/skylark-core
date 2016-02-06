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

testWriterReaders :: TestTree
testWriterReaders =
  testGroup "Writer reader tests"
    [ testProperty "Empty write chan" $
        \n x -> monadicIO $ do
          as <- run $ atomically $ do
            wc <- newTWChan n
            rcs <- replicateM x (newTRChan wc)
            catMaybes <$> forM rcs tryReadTRChan
          assert $ (as :: [Int]) == []
    , testProperty "0 length write chan" $
        \n x -> monadicIO $ do
          pre $ n > 0
          as <- run $ atomically $ do
            wc <- newTWChan 0
            rcs <- replicateM x (newTRChan wc)
            forM_ (take n [1..]) (writeTWChan wc)
            forM rcs $ (catMaybes <$>) . replicateM n . tryReadTRChan
          assert $ (as :: [[Int]]) == replicate x []
    , testProperty "1 length write chan with n writes" $
        \n x -> monadicIO $ do
          pre $ n > 0
          as <- run $ atomically $ do
            wc <- newTWChan 1
            rcs <- replicateM x (newTRChan wc)
            forM_ (take n [1..]) (writeTWChan wc)
            forM rcs $ (catMaybes <$>) . replicateM n . tryReadTRChan
          assert $ (as :: [[Int]]) == replicate x [n]
    , testProperty "n length write chan with less than n writes" $
        \n m x -> monadicIO $ do
          pre $ n > 0
          pre $ m <= n
          as <- run $ atomically $ do
            wc <- newTWChan $ fromIntegral n
            rcs <- replicateM x (newTRChan wc)
            forM_ (take m [1..]) (writeTWChan wc)
            forM rcs $ (catMaybes <$>) . replicateM m . tryReadTRChan
          assert $ (as :: [[Int]]) == replicate x (take m [1..])
    , testProperty "n length write chan with greater then n writes" $
        \n m x -> monadicIO $ do
          pre $ n > 0
          pre $ m > n
          as <- run $ atomically $ do
            wc <- newTWChan $ fromIntegral n
            rcs <- replicateM x (newTRChan wc)
            forM_ (take m [1..]) (writeTWChan wc)
            forM rcs $ (catMaybes <$>) . replicateM m . tryReadTRChan
          assert $ (as :: [[Int]]) == replicate x (take n (drop (m - n) [1..]))
    ]

tests :: TestTree
tests =
  testGroup "TChans tests"
    [ testWriterReaders
    ]

