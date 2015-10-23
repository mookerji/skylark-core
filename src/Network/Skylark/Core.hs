-- |
-- Module:      Network.Skylark.Core
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Core module for Skylark Core.

module Network.Skylark.Core
  ( module Network.Skylark.Core.Types
  , runCoreT
  , runResourceT
  ) where

import Control.Monad.Logger
import Control.Monad.Trans.AWS
import Network.Skylark.Core.Types

runCoreT :: HasCtx r => r -> Log -> CoreT r m a -> m a
runCoreT e l (CoreT m) = runAWST e (runLoggingT m l)
