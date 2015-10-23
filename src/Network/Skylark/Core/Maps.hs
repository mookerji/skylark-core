{-# LANGUAGE ConstraintKinds #-}
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
  , remove
  ) where

import           BasicPrelude
import           Control.Concurrent.STM
import           Control.Lens
import qualified Data.HashMap.Strict as M
import           Network.Skylark.Core.Types

withMap :: (MonadMap k r m) => GettingMap k v r -> (TVarMap k v -> (HashMap k v) -> STM b) -> m b
withMap a action = do
  b <- view a
  liftIO $ atomically $ do
    readTVar b >>= action b

get :: (MonadMap k r m) => GettingMap k v r -> k -> m (Maybe v)
get a k =
  withMap a $ \_b c ->
    return $ M.lookup k c

put :: (MonadMap k r m) => GettingMap k v r -> k -> v -> m ()
put a k v =
  withMap a $ \b c ->
    writeTVar b $ M.insert k v c

remove :: (MonadMap k r m) => GettingMap k v r -> k -> m ()
remove a k =
  withMap a $ \b c ->
    writeTVar b $ M.delete k c
