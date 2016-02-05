-- |
-- Module:      Network.Skylark.Core.Async
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Async module for Skylark Core.

module Network.Skylark.Core.Async
  ( withPeriodic
  , withPeriodicBound
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Network.Skylark.Core.Prelude

{-# ANN module ("HLint: ignore Use import/export shortcut"::String) #-}

-- | Periodically execute a monadic action (uses fork).
--
withPeriodic :: Int                -- ^ Interval (microseconds)
             -> IO a               -- ^ Some action
             -> (Async b -> IO c)  -- ^ Dereference the async
             -> IO c
withPeriodic interval action inner =
  flip withAsync inner $
    forever $ do
      void action
      threadDelay interval

-- | Periodically execute a monadic action (uses forkOS). Intended for
-- use when using an external FFI module that requires its own
-- thread-locals in a fixed OS thread.
--
withPeriodicBound :: Int                -- ^ Interval (microseconds)
                  -> IO a               -- ^ Some action
                  -> (Async b -> IO c)  -- ^ Dereference the async
                  -> IO c
withPeriodicBound interval action inner =
  flip withAsyncBound inner $
    forever $ do
      void action
      threadDelay interval
