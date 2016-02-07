-- |
-- Module:      Network.Skylark.Core.TChans
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- TChans module for Skylark Core.

module Network.Skylark.Core.TChans where

import           Control.Concurrent.STM
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           Network.Skylark.Core.Prelude hiding (first, head, last, tail)
import           Network.Skylark.Core.Types

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

newTWChan :: Word -> STM (TWChan a)
newTWChan n = do
  count <- newTVar n
  hole  <- newTVar TNil
  first <- newTVar hole
  last  <- newTVar hole
  return (TWChan count first last)

newTRChan :: TWChan a -> STM (TRChan a)
newTRChan (TWChan _count first last) = do
  hole      <- readTVar last
  new_first <- newTVar hole
  return (TRChan new_first first)

newTXChan :: TWChan a -> STM (TXChan a)
newTXChan (TWChan count first last) = do
  hole      <- readTVar last
  new_first <- newTVar hole
  return (TXChan count new_first first)

writeTWChan :: TWChan a -> a -> STM ()
writeTWChan (TWChan count first last) a = do
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

readTRChan :: TRChan a -> STM a
readTRChan (TRChan first last) =
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

tryReadTRChan :: TRChan a -> STM (Maybe a)
tryReadTRChan (TRChan first last) =
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

readTXChan :: TXChan a -> STM a
readTXChan (TXChan count first last) =
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

tryReadTXChan :: TXChan a -> STM (Maybe a)
tryReadTXChan (TXChan count first last) =
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

newTWMChan :: Word -> STM (TWMChan a)
newTWMChan n = do
  closed <- newTVar False
  chan   <- newTWChan n
  return (TWMChan closed chan)

newTRMChan :: TWMChan a -> STM (TRMChan a)
newTRMChan (TWMChan closed chan) = do
  new_chan <- newTRChan chan
  return (TRMChan closed new_chan)

newTXMChan :: TWMChan a -> STM (TXMChan a)
newTXMChan (TWMChan closed chan) = do
  new_chan <- newTXChan chan
  return (TXMChan closed new_chan)

writeTWMChan :: TWMChan a -> a -> STM ()
writeTWMChan (TWMChan closed chan) a = do
  b <- readTVar closed
  unless b $ writeTWChan chan a

closeTWMChan :: TWMChan a -> STM ()
closeTWMChan (TWMChan closed _chan) =
  writeTVar closed True

sinkTWMChan :: MonadIO m => TWMChan a -> Sink a m ()
sinkTWMChan chan = do
  CL.mapM_ $ liftIO . atomically . writeTWMChan chan
  liftIO $ atomically $ closeTWMChan chan

readTRMChan :: TRMChan a -> STM (Maybe a)
readTRMChan (TRMChan closed chan) = do
  b <- readTVar closed
  if b then tryReadTRChan chan else Just <$> readTRChan chan

sourceTRMChan :: MonadIO m => TRMChan a -> Source m a
sourceTRMChan chan =
  loop where
    loop = do
      a <- liftIO $ atomically $ readTRMChan chan
      maybe (return ()) ((>> loop) . yield) a

readTXMChan :: TXMChan a -> STM (Maybe a)
readTXMChan (TXMChan closed chan) = do
  b <- readTVar closed
  if b then tryReadTXChan chan else Just <$> readTXChan chan

sourceTXMChan :: MonadIO m => TXMChan a -> Source m a
sourceTXMChan chan =
  loop where
    loop = do
      a <- liftIO $ atomically $ readTXMChan chan
      maybe (return ()) ((>> loop) . yield) a

