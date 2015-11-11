{-# OPTIONS  -fno-warn-orphans          #-}
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
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.AWS hiding ( LogLevel, Request )
import Control.Monad.Trans.Resource
import Data.Text
import Data.Text.Lazy ( toStrict )
import Data.Text.Lazy.Builder
import Data.Time.Clock
import Data.Time.Format
import Data.UUID
import Formatting
import Network.Wai

type Log = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

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
  , MonadRandom m
  )

data Ctx = Ctx
  { _ctxEnv        :: Env
  , _ctxLog        :: Log
  , _ctxName       :: Text
  , _ctxVersion    :: Text
  , _ctxTag        :: Text
  , _ctxLogLevel   :: LogLevel
  , _ctxRequest    :: Request
  , _ctxSessionUid :: UUID
  }

class HasEnv a => HasCtx a where
  context       :: Lens' a Ctx

  ctxEnv        :: Lens' a Env
  ctxLog        :: Lens' a Log
  ctxName       :: Lens' a Text
  ctxVersion    :: Lens' a Text
  ctxTag        :: Lens' a Text
  ctxLogLevel   :: Lens' a LogLevel
  ctxRequest    :: Lens' a Request
  ctxSessionUid :: Lens' a UUID

  ctxEnv        = context . lens _ctxEnv        (\s a -> s { _ctxEnv = a } )
  ctxLog        = context . lens _ctxLog        (\s a -> s { _ctxLog = a } )
  ctxName       = context . lens _ctxName       (\s a -> s { _ctxName = a } )
  ctxVersion    = context . lens _ctxVersion    (\s a -> s { _ctxVersion = a } )
  ctxTag        = context . lens _ctxTag        (\s a -> s { _ctxTag = a } )
  ctxLogLevel   = context . lens _ctxLogLevel   (\s a -> s { _ctxLogLevel = a } )
  ctxRequest    = context . lens _ctxRequest    (\s a -> s { _ctxRequest = a } )
  ctxSessionUid = context . lens _ctxSessionUid (\s a -> s { _ctxSessionUid = a } )

instance HasCtx Ctx where
  context = id

instance HasEnv Ctx where
  environment = ctxEnv

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

instance MonadRandom m => MonadRandom (CoreT r m) where
  getRandom   = lift getRandom
  getRandomR  = lift . getRandomR
  getRandoms  = lift getRandoms
  getRandomRs = lift . getRandomRs

instance MonadRandom m => MonadRandom (ResourceT m) where
  getRandom   = lift getRandom
  getRandoms  = lift getRandoms
  getRandomR  = lift . getRandomR
  getRandomRs = lift . getRandomRs

type MonadMap k m =
  ( Eq k
  , Hashable k
  , MonadIO m
  )

class Txt a where
  txt :: a -> Text

instance Txt String where
  txt = pack

instance Txt ByteString where
  txt = decodeUtf8

instance Txt Builder where
  txt = toStrict . toLazyText

instance Txt Double where
  txt = show

instance Txt Word8 where
  txt = show

instance Txt Word32 where
  txt = show

instance Txt UUID where
  txt = toText

instance Txt UTCTime where
  txt time =
    txt $ formatTime defaultTimeLocale "%FT%T%z" time

instance Txt Request where
  txt req =
    sformat ("method=" % stext % " path=" % stext)
      (txt $ requestMethod req) (txt $ rawPathInfo req)

