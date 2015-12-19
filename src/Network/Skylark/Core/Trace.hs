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

trace :: LoggerSet -> Log
trace l _loc _source _level s = do
  pushLogStr l s
  flushLogStr l

newStderrTrace :: IO Log
newStderrTrace = do
  l <- newStderrLoggerSet defaultBufSize
  return $ trace l

newStdoutTrace :: IO Log
newStdoutTrace = do
  l <- newStdoutLoggerSet defaultBufSize
  return $ trace l

traceNull :: Log
traceNull _loc _source _level _s =
  return ()

traceLevel :: MonadCore e m => LogLevel -> (Text -> m ()) -> Text -> m ()
traceLevel level logN s = do
  level' <- view ctxLogLevel
  unless (level < level') $ do
    time <- liftIO getCurrentTime
    preamble <- view ctxPreamble
    logN $ sformat (stext % " " % stext % " " % stext % "\n")
      (txt time) preamble s

traceDebug :: MonadCore e m => Text -> m ()
traceDebug = traceLevel LevelDebug logDebugN

traceInfo :: MonadCore e m => Text -> m ()
traceInfo = traceLevel LevelInfo logInfoN

traceWarn :: MonadCore e m => Text -> m ()
traceWarn = traceLevel LevelWarn logWarnN

traceError :: MonadCore e m => Text -> m ()
traceError = traceLevel LevelError logErrorN
