-- |
-- Module:      Network.Skylark.Core.Setup
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Setup module for Skylark Core.

module Network.Skylark.Core.Setup
 ( newSettings
 , newCtx
 ) where

import Control.Monad.Logger
import Control.Monad.Trans.AWS        hiding (LogLevel, timeout)
import Network.Skylark.Core.Constants
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Trace
import Network.Skylark.Core.Types
import Network.Wai.Handler.Warp

newSettings :: Int -> Int -> Settings
newSettings port timeout =
  setPort port       $
  setTimeout timeout
  defaultSettings

newCtx :: LogLevel -> Text -> Text -> Text -> IO Ctx
newCtx logLevel name version tag = do
  _ctxEnv          <- newEnv Oregon $ FromEnv awsAccessKey awsSecretKey Nothing
  _ctxLog          <- newStderrTrace logLevel
  let _ctxPreamble  = preamble
  return Ctx {..} where
    preamble =
      sformat ("n=" % stext % " v=" % stext % " t=" % stext)
        name version tag
