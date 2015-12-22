-- |
-- Module:      Network.Skylark.Core
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Core module for Skylark Core.

module Network.Skylark.Core
  ( module Network.Skylark.Core.Types
  , runResourceT
  , runCoreT
  ) where

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Trans.AWS
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types

runCoreT :: HasCtx r => r -> CoreT r m a -> m a
runCoreT e (CoreT m) =
  runAWST e $ runLoggingT m $ e ^. ctxLog
