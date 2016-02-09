-- |
-- Module:      Network.Skylark.Core.TChans
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- TChans module for Skylark Core.

module Network.Skylark.Core.TChans
  ( newTWChan
  , newTRChan
  , writeTWChan
  , readTRChan
  , tryReadTRChan
  , newTWCChan
  , newTRCChan
  , writeTWCChan
  , closeTWCChan
  , sinkTWCChan
  , readTRCChan
  , sourceTRCChan
  ) where

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

newTWCChan :: Word -> STM (TWCChan a)
newTWCChan n = do
  closed <- newTVar False
  chan   <- newTWChan n
  return (TWCChan closed chan)

newTRCChan :: TWCChan a -> STM (TRCChan a)
newTRCChan (TWCChan closed chan) = do
  new_chan <- newTRChan chan
  return (TRCChan closed new_chan)

writeTWCChan :: TWCChan a -> a -> STM ()
writeTWCChan (TWCChan closed chan) a = do
  b <- readTVar closed
  unless b $ writeTWChan chan a

closeTWCChan :: TWCChan a -> STM ()
closeTWCChan (TWCChan closed _chan) =
  writeTVar closed True

sinkTWCChan :: MonadIO m => TWCChan a -> Sink a m ()
sinkTWCChan chan = do
  CL.mapM_ $ liftIO . atomically . writeTWCChan chan
  liftIO $ atomically $ closeTWCChan chan

readTRCChan :: TRCChan a -> STM (Maybe a)
readTRCChan (TRCChan closed chan) = do
  b <- readTVar closed
  if b then tryReadTRChan chan else Just <$> readTRChan chan

sourceTRCChan :: MonadIO m => TRCChan a -> Source m a
sourceTRCChan chan =
  loop where
    loop = do
      a <- liftIO $ atomically $ readTRCChan chan
      maybe (return ()) ((>> loop) . yield) a
