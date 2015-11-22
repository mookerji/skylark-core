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

import Control.Lens
import Control.Monad.Random
import Data.IORef
import Data.Time
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types

-- | Jitter value by a random factor.
jitter :: MonadRandom m => Double -> Double -> m Double
jitter v r = (* v) <$> getRandomR (1, 1 + r)

-- | Check timer and update if elapsed, run action if expired.
whenExpires :: MonadCore e m => UTCTime -> IORef UTCTime -> Double -> m () -> m ()
whenExpires time expires interval a = do
  expires' <- liftIO $ readIORef expires
  when (time >= expires') $ do
    rate <- view ctxJitterRate
    interval' <- jitter interval rate
    liftIO $ writeIORef expires $ addUTCTime (fromRational $ toRational interval') time
    a
