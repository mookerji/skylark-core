{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module:      Network.Skylark.Core.Types
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Types module for Skylark Core.

module Network.Skylark.Core.Types where

import BasicPrelude
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.AWS hiding ( LogLevel, Request )
import Control.Monad.Trans.Resource
import Data.UUID
import Network.Wai

type Log              = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
type TVarMap k v      = TVar (HashMap k v)
type GettingMap k v r = Getting (TVarMap k v) r (TVarMap k v)

type MonadMap k r m =
  ( Eq k
  , Hashable k
  , MonadReader r m
  , MonadIO m
  )

newtype CoreT e m a = CoreT
  { unCoreT :: LoggingT (AWST' e m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadLogger
             )

type MonadCore e m =
  ( AWSConstraint e m
  , HasCtx e
  , MonadLogger m
  )

data Ctx = Ctx
  { _ctxName       :: Text
  , _ctxVersion    :: Text
  , _ctxTag        :: Text
  , _ctxLogLevel   :: LogLevel
  , _ctxRequest    :: Request
  , _ctxSessionUid :: UUID
  }

class HasEnv a => HasCtx a where
  context :: Lens' a Ctx

  ctxName :: Lens' a Text
  ctxName = context . lens _ctxName (\s a -> s { _ctxName = a } )

  ctxVersion :: Lens' a Text
  ctxVersion = context . lens _ctxVersion (\s a -> s { _ctxVersion = a } )

  ctxTag :: Lens' a Text
  ctxTag = context . lens _ctxTag (\s a -> s { _ctxTag = a } )

  ctxLogLevel :: Lens' a LogLevel
  ctxLogLevel = context . lens _ctxLogLevel (\s a -> s { _ctxLogLevel = a } )

  ctxRequest :: Lens' a Request
  ctxRequest = context . lens _ctxRequest (\s a -> s { _ctxRequest = a } )

  ctxSessionUid :: Lens' a UUID
  ctxSessionUid = context . lens _ctxSessionUid (\s a -> s { _ctxSessionUid = a } )

instance MonadBase b m => MonadBase b (CoreT r m) where
  liftBase = liftBaseDefault

instance MonadTrans (CoreT r) where
  lift = CoreT . lift . lift

instance MonadResource m => MonadResource (CoreT r m) where
  liftResourceT = lift . liftResourceT

instance Monad m => MonadReader r (CoreT r m) where
  ask     = CoreT ask
  local f = CoreT . local f . unCoreT
  reader  = CoreT . reader
