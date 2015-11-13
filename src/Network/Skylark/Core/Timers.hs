-- |
-- Module:      Network.Skylark.Core.Timers
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Timers module for Skylark Core.

module Network.Skylark.Core.Timers where

import BasicPrelude
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Random
import Data.Time.Clock
import Network.Skylark.Core.Types

-- | Jitter value by a random factor.
jitter :: MonadRandom m => Double -> Double -> m Double
jitter v r = (* v) <$> getRandomR (1, 1 + r)

-- | Check timer and update if elapsed, run action if expired.
whenExpires :: MonadCore e m => TVar UTCTime -> Double -> m () -> m ()
whenExpires expires interval action = do
  rate <- view ctxJitterRate
  interval' <- jitter interval rate
  expired <- liftIO $ do
    time <- getCurrentTime
    atomically $ do
      expires' <- readTVar expires
      let expired = time >= expires'
      when expired $
        writeTVar expires $ addUTCTime (fromRational $ toRational interval') time
      return expired
  when expired action
