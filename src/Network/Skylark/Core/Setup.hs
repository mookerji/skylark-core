-- |
-- Module:      Network.Skylark.Core.Setup
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Shared Setup module: this contains things like utilities for
-- setting up application context.

module Network.Skylark.Core.Setup
  ( newSettings
  , newCtx
  ) where

import Control.Lens
import Control.Monad.Trans.AWS        hiding (LogLevel, timeout)
import Data.Maybe
import Data.Text
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Constants
import Network.Skylark.Core.Prelude
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
  name     <- mandatory "log-level" $ c ^. confAppName
  let _ctxConf     = c
      _ctxPreamble = preamble $ txt name
  logLevel <- mandatory "log-level" $ c ^. confLogLevel
  _ctxEnv  <- newEnv Oregon $ FromEnv awsAccessKey awsSecretKey Nothing
  _ctxLog  <- newStderrTrace logLevel
  return Ctx {..} where
    preamble name =
      sformat ("n=" % stext % " v=" % stext % " t=" % stext)
        name version tag
