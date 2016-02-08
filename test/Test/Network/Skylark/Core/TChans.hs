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

writesRead :: Int -> Int -> Int -> IO [Int]
writesRead n x y = atomically $ do
  wc <- newTBChan $ fromIntegral n
  xc <- newTSChan wc
  forM_ (take x [1..]) (writeTBChan wc)
  catMaybes <$> replicateM y (tryReadTSChan xc)

writesReads :: Int -> Int -> Int -> Int -> IO [[Int]]
writesReads n r x y = atomically $ do
  wc  <- newTBChan $ fromIntegral n
  rcs <- replicateM r (newTMChan wc)
  forM_ (take x [1..]) (writeTBChan wc)
  forM rcs $ (catMaybes <$>) . replicateM y . tryReadTMChan

testWriterReader :: TestTree
testWriterReader =
  testGroup "Writer reader tests"
    [ testProperty "Empty write chan" $
        \n m -> monadicIO $ do
          as <- run $ writesRead n 0 m
          assert $ null as
    , testProperty "0 length write chan" $
        \n m -> monadicIO $ do
          pre $ n > 0
          as <- run $ writesRead 0 n m
          assert $ null as
    , testProperty "1 length write chan with n writes" $
        \n m -> monadicIO $ do
          pre $ n > 0
          pre $ m > 0
          as <- run $ writesRead 1 n m
          assert $ as == [n]
    , testProperty "n length write chan with less than n writes" $
        \n m -> monadicIO $ do
          pre $ n > 0
          pre $ m <= n
          as <- run $ writesRead n m m
          assert $ as == take m [1..]
    , testProperty "n length write chan with greater then n writes" $
        \n m -> monadicIO $ do
          pre $ n > 0
          pre $ m > n
          as <- run $ writesRead n m m
          assert $ as == take n (drop (m - n) [1..])
    ]

testWriterReaders :: TestTree
testWriterReaders =
  testGroup "Writer reader tests"
    [ testProperty "Empty write chan" $
        \r n m -> monadicIO $ do
          as <- run $ writesReads n r 0 m
          assert $ as == replicate r []
    , testProperty "0 length write chan" $
        \r n m -> monadicIO $ do
          pre $ n > 0
          as <- run $ writesReads 0 r n m
          assert $ as == replicate r []
    , testProperty "1 length write chan with n writes" $
        \r n m -> monadicIO $ do
          pre $ n > 0
          pre $ m > 0
          as <- run $ writesReads 1 r n m
          assert $ as == replicate r [n]
    , testProperty "n length write chan with less than n writes" $
        \r n m -> monadicIO $ do
          pre $ n > 0
          pre $ m <= n
          as <- run $ writesReads n r m m
          assert $ as == replicate r (take m [1..])
    , testProperty "n length write chan with greater then n writes" $
        \r n m -> monadicIO $ do
          pre $ n > 0
          pre $ m > n
          as <- run $ writesReads n r m m
          assert $ as == replicate r (take n (drop (m - n) [1..]))
    ]

tests :: TestTree
tests =
  testGroup "TChans tests"
    [ testWriterReader
    , testWriterReaders
    ]

