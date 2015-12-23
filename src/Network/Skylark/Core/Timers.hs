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
import Network.Skylark.Core.Constants
import Network.Skylark.Core.Prelude

-- | Jitter value by a random factor.
jitter :: MonadRandom m => Double -> m Double
jitter v = (* v) <$> getRandomR (1, 1 + jitterRate)

-- | Check timer and update if elapsed, run action if expired.
whenExpires :: (MonadRandom m, MonadIO m) => UTCTime -> IORef UTCTime -> Double -> m () -> m ()
whenExpires time expires interval a = do
  expires' <- liftIO $ readIORef expires
  when (time >= expires') $ do
    interval' <- jitter interval
    liftIO $ writeIORef expires $ addUTCTime (fromRational $ toRational interval') time
    a
