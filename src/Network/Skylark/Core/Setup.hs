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
  ) where

import Control.Lens
import Control.Monad.Trans.AWS                 hiding (timeout)
import Data.Text
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Constants
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Providers.StatGrab
import Network.Skylark.Core.Trace
import Network.Skylark.Core.Types
import Network.Wai.Handler.Warp

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

-- Emit a health check metrics
--
checkHealth :: MonadCore e m => m ()
checkHealth = do
  gr <- getEventGroup
  s  <- runStats sampleStats
  mapM_ traceMetric $ measure gr s
