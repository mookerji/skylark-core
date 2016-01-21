-- |
-- Module:      Network.Skylark.Core.Timers
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Timers module for Skylark Core.

module Network.Skylark.Core.Timers
  ( whenExpires
  ) where

import Control.Monad.Random
import Data.IORef
import Data.Time
import Network.Skylark.Core.Prelude

-- | Check timer and update if elapsed, run action if expired - interval is in microseconds.
whenExpires :: (MonadIO m, MonadRandom m) => UTCTime -> IORef UTCTime -> Word -> m () -> m ()
whenExpires time expires interval action = do
  expires' <- liftIO $ readIORef expires
  when (time >= expires') $ do
    interval' <- jitter
    liftIO $ writeIORef expires $ addUTCTime (fromIntegral $ div interval' 1000000) time
    action where
      jitter = getRandomR (interval - div interval 10, interval + div interval 10)
