-- |
-- Module:      Network.Skylark.Core.MVars
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- MVars module for Skylark Core.

module Network.Skylark.Core.MVars
  ( newBarrier
  , signalBarrier
  , waitBarrier
  ) where

import Control.Concurrent.MVar
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types

newBarrier :: MonadIO m => m Barrier
newBarrier = liftIO newEmptyMVar

signalBarrier :: MonadIO m => Barrier -> m ()
signalBarrier = liftIO . flip putMVar ()

waitBarrier :: MonadIO m => Barrier -> m ()
waitBarrier = liftIO . readMVar
