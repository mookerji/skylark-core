-- |
-- Module:      Network.Skylark.Core.TChans
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- TChans module for Skylark Core.

module Network.Skylark.Core.TChans
  ( newTBChan
  , newTMChan
  , newTSChan
  , writeTBChan
  , readTMChan
  , tryReadTMChan
  , readTSChan
  , tryReadTSChan
  , newTBCChan
  , newTMCChan
  , newTSCChan
  , writeTBCChan
  , closeTBCChan
  , sinkTBCChan
  , readTMCChan
  , sourceTMCChan
  , readTSCChan
  , sourceTSCChan
  ) where

import           Control.Concurrent.STM
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           Network.Skylark.Core.Prelude hiding (first, head, last, tail)
import           Network.Skylark.Core.Types

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

newTBChan :: Word -> STM (TBChan a)
newTBChan n = do
  count <- newTVar n
  hole  <- newTVar TNil
  first <- newTVar hole
  last  <- newTVar hole
  return (TBChan count first last)

newTMChan :: TBChan a -> STM (TMChan a)
newTMChan (TBChan _count first last) = do
  hole      <- readTVar last
  new_first <- newTVar hole
  return (TMChan new_first first)

newTSChan :: TBChan a -> STM (TSChan a)
newTSChan (TBChan count first last) = do
  hole      <- readTVar last
  new_first <- newTVar hole
  return (TSChan count new_first first)

writeTBChan :: TBChan a -> a -> STM ()
writeTBChan (TBChan count first last) a = do
  new_listend <- newTVar TNil
  listend     <- readTVar last
  writeTVar listend (TCons a new_listend)
  writeTVar last new_listend
  n <- readTVar count
  if n > 0 then writeTVar count $! n - 1 else do
    listhead <- readTVar first
    head     <- readTVar listhead
    case head of
      TCons _a tail -> writeTVar first tail
      _tl           -> return ()
    writeTVar listhead TNull

readTMChan :: TMChan a -> STM a
readTMChan (TMChan first last) =
  loop where
    loop = do
      listhead <- readTVar first
      head     <- readTVar listhead
      case head of
        TNil  -> retry
        TNull -> do
          new_first <- readTVar last
          writeTVar first new_first
          loop
        TCons a tail -> do
          writeTVar first tail
          return a

tryReadTMChan :: TMChan a -> STM (Maybe a)
tryReadTMChan (TMChan first last) =
  loop where
    loop = do
      listhead <- readTVar first
      head     <- readTVar listhead
      case head of
        TNil  -> return Nothing
        TNull -> do
          new_first <- readTVar last
          writeTVar first new_first
          loop
        TCons a tail -> do
          writeTVar first tail
          return $ Just a

readTSChan :: TSChan a -> STM a
readTSChan (TSChan count first last) =
  loop where
    loop = do
      listhead <- readTVar first
      head     <- readTVar listhead
      case head of
        TNil  -> retry
        TNull -> do
          new_first <- readTVar last
          writeTVar first new_first
          loop
        TCons a tail -> do
          writeTVar first tail
          modifyTVar' count $ (-) 1
          return a

tryReadTSChan :: TSChan a -> STM (Maybe a)
tryReadTSChan (TSChan count first last) =
  loop where
    loop = do
      listhead <- readTVar first
      head     <- readTVar listhead
      case head of
        TNil  -> return Nothing
        TNull -> do
          new_first <- readTVar last
          writeTVar first new_first
          loop
        TCons a tail -> do
          writeTVar first tail
          modifyTVar' count $ (-) 1
          return $ Just a

newTBCChan :: Word -> STM (TBCChan a)
newTBCChan n = do
  closed <- newTVar False
  chan   <- newTBChan n
  return (TBCChan closed chan)

newTMCChan :: TBCChan a -> STM (TMCChan a)
newTMCChan (TBCChan closed chan) = do
  new_chan <- newTMChan chan
  return (TMCChan closed new_chan)

newTSCChan :: TBCChan a -> STM (TSCChan a)
newTSCChan (TBCChan closed chan) = do
  new_chan <- newTSChan chan
  return (TSCChan closed new_chan)

writeTBCChan :: TBCChan a -> a -> STM ()
writeTBCChan (TBCChan closed chan) a = do
  b <- readTVar closed
  unless b $ writeTBChan chan a

closeTBCChan :: TBCChan a -> STM ()
closeTBCChan (TBCChan closed _chan) =
  writeTVar closed True

sinkTBCChan :: MonadIO m => TBCChan a -> Sink a m ()
sinkTBCChan chan = do
  CL.mapM_ $ liftIO . atomically . writeTBCChan chan
  liftIO $ atomically $ closeTBCChan chan

readTMCChan :: TMCChan a -> STM (Maybe a)
readTMCChan (TMCChan closed chan) = do
  b <- readTVar closed
  if b then tryReadTMChan chan else Just <$> readTMChan chan

sourceTMCChan :: MonadIO m => TMCChan a -> Source m a
sourceTMCChan chan =
  loop where
    loop = do
      a <- liftIO $ atomically $ readTMCChan chan
      maybe (return ()) ((>> loop) . yield) a

readTSCChan :: TSCChan a -> STM (Maybe a)
readTSCChan (TSCChan closed chan) = do
  b <- readTVar closed
  if b then tryReadTSChan chan else Just <$> readTSChan chan

sourceTSCChan :: MonadIO m => TSCChan a -> Source m a
sourceTSCChan chan =
  loop where
    loop = do
      a <- liftIO $ atomically $ readTSCChan chan
      maybe (return ()) ((>> loop) . yield) a

