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
  ( getEventGroup
  , newStderrTrace
  , newStdoutTrace
  , traceNull
  , traceDebug
  , traceDebug'
  , traceInfo
  , traceInfo'
  , traceWarn
  , traceWarn'
  , traceError
  , traceError'
  , traceMetric
  , traceEventCounter
  , traceEventTimer
  , traceEventGauge
  , traceEventSet
  ) where

import Control.Lens
import Control.Monad.Logger
import Data.Time
import Formatting
import Network.Skylark.Core.Conf
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types
import System.Log.FastLogger
import System.Statgrab

--------------------------------------------------------------------------------
-- Text logging

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

trace' :: MonadIO m => (Text -> m b) -> Text -> m b
trace' logN s = do
  time <- liftIO getCurrentTime
  logN $ sformat (stext % " " % stext % "\n")
    (txt time) s

traceDebug :: MonadCore e m => Text -> m ()
traceDebug = trace logDebugN

traceDebug' :: Text -> IO ()
traceDebug' s = newStderrTrace LevelDebug >>= runLoggingT (trace' logDebugN s)

traceInfo :: MonadCore e m => Text -> m ()
traceInfo = trace logInfoN

traceInfo' :: Text -> IO ()
traceInfo' s = newStderrTrace LevelInfo >>= runLoggingT (trace' logInfoN s)

traceWarn :: MonadCore e m => Text -> m ()
traceWarn = trace logWarnN

traceWarn' :: Text -> IO ()
traceWarn' s = newStderrTrace LevelWarn >>= runLoggingT (trace' logWarnN s)

traceError :: MonadCore e m => Text -> m ()
traceError = trace logErrorN

traceError' :: Text -> IO ()
traceError' s = newStderrTrace LevelError >>= runLoggingT (trace' logErrorN s)

--------------------------------------------------------------------------------
-- Event logging: emit metrics to the application log.

type ToMetric t = Group -> Bucket -> t -> Metric

getHostName :: IO HostName
getHostName =  hostName <$> runStats (snapshot :: Stats Host)

-- | Produces an string used to identify a group of events, typically
-- using the app and host name.
--
getEventGroup :: MonadCore e m => m Text
getEventGroup = do
  hostName <- liftIO getHostName
  appName  <- view confAppName >>= liftIO . mandatory "app-name"
  return $ sformat (stext % "." % stext) appName (txt hostName)

-- | Emit a key-value for any Metric constructor.
--
traceEvent :: (Num t, MonadCore e m)
           => ToMetric t         -- ^ Constructor for a metric
           -> Bucket             -- ^ Metric key
           -> t                  -- ^ Any numeric value
           -> m ()
traceEvent metricType key value = do
  group' <- getEventGroup
  trace logInfoN $ sformat ("event=metric : " % stext) (txt $ metricType group' key value)

-- | Emit a single metric event to the log.
--
traceMetric :: MonadCore e m => Metric -> m ()
traceMetric metric = trace logInfoN $ sformat ("event=metric : " % stext) (txt metric)

-- | Emit a key-value counter.
--
traceEventCounter :: MonadCore e m => Bucket -> Integer -> m ()
traceEventCounter = traceEvent Counter

-- | Emit a key-value timing measurement.
--
traceEventTimer :: MonadCore e m => Bucket -> Double -> m ()
traceEventTimer = traceEvent Timer

-- | Emit a key-value scalar quantity.
--
traceEventGauge :: MonadCore e m => Bucket -> Double -> m ()
traceEventGauge = traceEvent Gauge

-- | Emit a key-value event for which we want to measure unique occurrences.
--
traceEventSet :: MonadCore e m => Bucket -> Double -> m ()
traceEventSet = traceEvent Set
