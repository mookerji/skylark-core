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
  , runCoreIO
  ) where

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types

runCoreT :: HasCtx e => e -> CoreT e m a -> m a
runCoreT e (CoreT m) =
  runAWST e $ runLoggingT m $ e ^. ctxLog

runCoreIO :: HasCtx e => e -> CoreT e (ResourceT IO) a -> IO a
runCoreIO e a =
  runResourceT $ runCoreT e a
