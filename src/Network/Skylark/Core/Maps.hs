-- |
-- Module:      Network.Skylark.Core.Maps
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Maps module for Skylark Core.

module Network.Skylark.Core.Maps
  ( get
  , put
  , tryPut
  , remove
  , list
  , bracketed
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Catch
import qualified Data.HashMap.Strict          as M
import           Network.Skylark.Core.Prelude hiding (bracket_)
import           Network.Skylark.Core.Types

get :: MonadMap k m => TVar (HashMap k v) -> k -> m (Maybe v)
get m k =
  liftIO $ atomically $ do
    n <- readTVar m
    return $ M.lookup k n

put :: MonadMap k m => TVar (HashMap k v) -> k -> v -> m ()
put m k v =
  liftIO $ atomically $ do
    n <- readTVar m
    writeTVar m $ M.insert k v n

tryPut :: MonadMap k m => TVar (HashMap k v) -> k -> v -> m Bool
tryPut m k v =
  liftIO $ atomically $ do
    n <- readTVar m
    if M.member k n then return False else do
      writeTVar m $ M.insert k v n
      return True

remove :: MonadMap k m => TVar (HashMap k v) -> k -> m ()
remove m k =
  liftIO $ atomically $ do
    n <- readTVar m
    writeTVar m $ M.delete k n

list :: MonadMap k m => TVar (HashMap k v) -> m [(k, v)]
list m =
  liftIO $ atomically $ do
    n <- readTVar m
    return $ M.toList n

bracketed :: MonadMap k m => TVar (HashMap k v) -> k -> v -> m c -> m c
bracketed m k v =
  bracket_ (put m k v) (remove m k)

