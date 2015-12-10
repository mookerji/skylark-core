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

import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.AWS hiding ( LogLevel )
import Control.Monad.Trans.Resource
import Data.Text
import Data.Text.Lazy ( toStrict )
import Data.Text.Lazy.Builder
import Data.Time
import Data.UUID
import Network.AWS.DynamoDB
import Network.Skylark.Core.Prelude

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
  , _ctxLogLevel   :: LogLevel
  , _ctxPreamble   :: Text
  , _ctxJitterRate :: Double
  }

class HasEnv a => HasCtx a where
  ctxId         :: Lens' a Ctx
  ctxEnv        :: Lens' a Env
  ctxLog        :: Lens' a Log
  ctxLogLevel   :: Lens' a LogLevel
  ctxPreamble   :: Lens' a Text
  ctxJitterRate :: Lens' a Double

  ctxEnv        = ctxId . lens _ctxEnv        (\s a -> s { _ctxEnv = a } )
  ctxLog        = ctxId . lens _ctxLog        (\s a -> s { _ctxLog = a } )
  ctxLogLevel   = ctxId . lens _ctxLogLevel   (\s a -> s { _ctxLogLevel = a } )
  ctxPreamble   = ctxId . lens _ctxPreamble   (\s a -> s { _ctxPreamble = a } )
  ctxJitterRate = ctxId . lens _ctxJitterRate (\s a -> s { _ctxJitterRate = a } )

instance HasCtx Ctx where
  ctxId = id

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

type AttributeValueMap = HashMap Text AttributeValue

data Upsert = Upsert
  { _upsertTable :: Text
  , _upsertTime  :: UTCTime
  , _upsertKey   :: AttributeValueMap
  , _upsertExprs :: [Text]
  , _upsertVals  :: AttributeValueMap
  } deriving ( Eq, Show )

class HasUpsert a where
  upsertId    :: Lens' a Upsert
  upsertTable :: Lens' a Text
  upsertTime  :: Lens' a UTCTime
  upsertKey   :: Lens' a AttributeValueMap
  upsertExprs :: Lens' a [Text]
  upsertVals  :: Lens' a AttributeValueMap

  upsertTable = upsertId . lens _upsertTable (\s a -> s { _upsertTable = a } )
  upsertTime  = upsertId . lens _upsertTime  (\s a -> s { _upsertTime = a } )
  upsertKey   = upsertId . lens _upsertKey   (\s a -> s { _upsertKey = a } )
  upsertExprs = upsertId . lens _upsertExprs (\s a -> s { _upsertExprs = a } )
  upsertVals  = upsertId . lens _upsertVals  (\s a -> s { _upsertVals = a } )

instance HasUpsert Upsert where
  upsertId = id

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

instance Txt Int32 where
  txt = show

instance Txt UUID where
  txt = toText

instance Txt UTCTime where
  txt time =
    txt $ formatTime defaultTimeLocale "%FT%T%z" time
