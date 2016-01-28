-- |
-- Module:      Network.Skylark.Core.Setup
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Shared Setup module: this contains things like utilities for
-- setting up application context.

module Network.Skylark.Core.Setup
  ( checkHealth
  , newCtx
  , newSettings
  , withHealthCheck
  ) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.Trans.AWS                 hiding (timeout)
import Data.Text
import Network.Skylark.Core
import Network.Skylark.Core.Async
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Constants
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Providers.StatGrab
import Network.Skylark.Core.Trace
import Network.Wai.Handler.Warp

--------------------------------------------------------------------------------
-- Setup for application context

-- | Setup WAI settings
--
newSettings :: Int -> Int -> Settings
newSettings port timeout =
  setPort port       $
  setTimeout timeout
  defaultSettings

-- | Initialize application context from configuration.
--
newCtx :: Conf         -- ^ Service configuration
       -> Text         -- ^ Cabal version
       -> Text         -- ^ Git tag
       -> IO Ctx
newCtx c version tag = do
  name     <- mandatory "app-name" $ c ^. confAppName
  port     <- mandatory "port"     $ c ^. confPort
  timeout  <- mandatory "timeout"  $ c ^. confTimeout
  let _ctxConf     = c
      _ctxPreamble = preamble name
      _ctxSettings = newSettings port timeout
  logLevel <- mandatory "log-level" $ c ^. confLogLevel
  _ctxEnv  <- newEnv Oregon $ FromEnv awsAccessKey awsSecretKey Nothing
  _ctxLog  <- newStderrTrace logLevel
  return Ctx {..} where
    preamble name =
      sformat ("n=" % stext % " v=" % stext % " t=" % stext)
        name version tag

-- | Core context with some periodic health checking.
--
withHealthCheck :: HasCtx e => e -> IO b -> IO b
withHealthCheck ctx action =
  withAsync action $ \a ->
    withPeriodic healthCheckInterval (checkHealth ctx) $ \b ->
      wait b >> wait a

--------------------------------------------------------------------------------
-- Setup for health monitoring

-- Emit a health check metrics
--
checkHealth :: HasCtx e => e -> IO ()
checkHealth ctx =
  runCoreIO ctx $ do
    gr <- getEventGroup
    s  <- runStats sampleStats
    mapM_ traceMetric $ measure gr s
