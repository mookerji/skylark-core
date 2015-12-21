{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module:      Network.Skylark.Core.Trace
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Trace module for Skylark Core.

module Network.Skylark.Core.Trace
  ( newStderrTrace
  , newStdoutTrace
  , traceNull
  , traceDebug
  , traceInfo
  , traceWarn
  , traceError
  ) where

import Control.Lens
import Control.Monad.Logger
import Data.Time
import Formatting
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types
import System.Log.FastLogger

traceLevel :: LogLevel -> LoggerSet -> Log
traceLevel level ls _loc _source level' s =
  unless (level' < level) $ do
    pushLogStr ls s
    flushLogStr ls

newStderrTrace :: LogLevel -> IO Log
newStderrTrace level = do
  ls <- newStderrLoggerSet defaultBufSize
  return $ traceLevel level ls

newStdoutTrace :: LogLevel -> IO Log
newStdoutTrace level = do
  ls <- newStdoutLoggerSet defaultBufSize
  return $ traceLevel level ls

traceNull :: Log
traceNull _loc _source _level _s =
  return ()

trace :: MonadCore e m => (Text -> m ()) -> Text -> m ()
trace logN s = do
  time <- liftIO getCurrentTime
  preamble <- view ctxPreamble
  logN $ sformat (stext % " " % stext % " " % stext % "\n")
    (txt time) preamble s

traceDebug :: MonadCore e m => Text -> m ()
traceDebug = trace logDebugN

traceInfo :: MonadCore e m => Text -> m ()
traceInfo = trace logInfoN

traceWarn :: MonadCore e m => Text -> m ()
traceWarn = trace logWarnN

traceError :: MonadCore e m => Text -> m ()
traceError = trace logErrorN
