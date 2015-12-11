{-# OPTIONS  -fno-warn-orphans          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module:      Network.Skylark.Core.Types
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Types module for Skylark Core.

module Network.Skylark.Core.Types where

import Control.Lens hiding ((.=))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.AWS hiding ( LogLevel )
import Control.Monad.Trans.Resource
import Data.Aeson hiding ((.!=), (.=))
import Data.Default
import Data.Monoid
import Data.Text (unpack, pack)
import Data.Text.Lazy ( toStrict )
import Data.Text.Lazy.Builder
import Data.Time
import Data.Word
import Data.UUID
import Network.AWS.DynamoDB
import Network.Skylark.Core.Prelude
import System.Envy
import Text.Read (readMaybe)

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

--------------------------------------------------------------------------------
-- Common type classes

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

instance ToJSON UUID where
  toJSON = toJSON . toText

instance FromJSON UUID where
  parseJSON (String s) = maybe mzero return (Data.UUID.fromText s)
  parseJSON _ = mzero

instance FromJSON LogLevel where
  parseJSON (String "debug") = return LevelDebug
  parseJSON (String "info")  = return LevelInfo
  parseJSON (String "warn")  = return LevelWarn
  parseJSON (String "error") = return LevelError
  parseJSON (String s)       = return $ LevelOther s
  parseJSON _                = mzero

--------------------------------------------------------------------------------
-- Service configuration

instance Var LogLevel where
  toVar LevelDebug = "debug"
  toVar LevelInfo = "info"
  toVar LevelWarn = "warn"
  toVar LevelError = "error"
  toVar (LevelOther s) = unpack s

  fromVar "debug" = return LevelDebug
  fromVar "info"  = return LevelInfo
  fromVar "warn"  = return LevelWarn
  fromVar "error" = return LevelError
  fromVar s       = return $ LevelOther (pack s)

instance (Var a, Show a, Read a) => Var (Maybe a) where
  toVar (Just v) = (unpack . show) v
  toVar Nothing = ""
  fromVar = readMaybe

-- | A record type representing full or partial configuration of an
-- HTTP service. Remaining unspecified fields are filled in with the
-- default values from Default.
--
data Config = Config
  { _cConfigFile    :: Maybe String   -- ^ Service configuration file location
  , _cPort          :: Maybe Word32   -- ^ Port to listen on
  , _cTimeout       :: Maybe Word32   -- ^ Connection timeout (sec)
  , _cLogLevel      :: Maybe LogLevel -- ^ Logging level
  } deriving ( Eq, Show )

class HasConfig a where
  cId          :: Lens' a Config
  cConfigFile  :: Lens' a (Maybe String)
  cPort        :: Lens' a (Maybe Word32)
  cTimeout     :: Lens' a (Maybe Word32)
  cLogLevel    :: Lens' a (Maybe LogLevel)

  cConfigFile  = cId . lens _cConfigFile (\s a -> s { _cConfigFile = a } )
  cPort        = cId . lens _cPort       (\s a -> s { _cPort = a } )
  cTimeout     = cId . lens _cTimeout    (\s a -> s { _cTimeout = a } )
  cLogLevel    = cId . lens _cLogLevel   (\s a -> s { _cLogLevel = a } )

instance HasConfig Config where
  cId = id

instance Default Config where
  def = Config
    { _cConfigFile = Just "/conf/dev.yaml"
    , _cPort       = Just 3030
    , _cTimeout    = Just 120
    , _cLogLevel   = Just LevelInfo
    }

instance FromJSON Config where
  parseJSON (Object v) =
    Config                 <$>
      v .:? "conf-file"    <*>
      v .:? "port"         <*>
      v .:? "timeout"      <*>
      v .:? "log-level"
  parseJSON _ = mzero

instance FromEnv Config where
  fromEnv =
    Config                        <$>
      envMaybe "SKYLARK_CONFFILE" <*>
      envMaybe "SKYLARK_PORT"     <*>
      envMaybe "SKYLARK_TIMEOUT"  <*>
      envMaybe "SKYLARK_LOGLEVEL"

instance ToEnv Config where
  toEnv Config{..} =
    makeEnv [ "SKYLARK_CONFFILE" .= _cConfigFile
            , "SKYLARK_PORT"     .= _cPort
            , "SKYLARK_TIMEOUT"  .= _cTimeout
            , "SKYLARK_LOGLEVEL" .= _cLogLevel
            ]

instance Monoid Config where
  mempty = Config
    { _cConfigFile    = Nothing
    , _cPort          = Nothing
    , _cTimeout       = Nothing
    , _cLogLevel      = Nothing
    }

  a `mappend` b = Config
    { _cConfigFile    = merge _cConfigFile a b
    , _cPort          = merge _cPort a b
    , _cTimeout       = merge _cTimeout a b
    , _cLogLevel      = merge _cLogLevel a b
    }

-- | Given a record field accessor. return the second non-Nothing
-- Value for a record field.
--
merge :: forall a a1. (a1 -> Maybe a) -> a1 -> a1 -> Maybe a
merge f a b = getLast $! (mappend `on` (Last . f)) a b
