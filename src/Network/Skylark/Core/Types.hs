{-# OPTIONS  -fno-warn-orphans          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module:      Network.Skylark.Core.Types
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Types module for Skylark Core.

module Network.Skylark.Core.Types where

import Control.Lens                 hiding ((.=))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.AWS      hiding (LogLevel)
import Control.Monad.Trans.Resource
import Data.Aeson                   hiding ((.!=), (.=))
import Data.Default
import Data.Monoid
import Data.Text                    (pack, unpack)
import Data.Text.Lazy               (toStrict)
import Data.Text.Lazy.Builder       hiding (fromText)
import Data.Time
import Data.UUID
import Network.AWS.DynamoDB
import Network.Skylark.Core.Prelude
import System.Envy
import Text.Read                    (readMaybe)

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

data Ctx = Ctx
  { _ctxEnv      :: Env
  , _ctxLog      :: Log
  , _ctxPreamble :: Text
  }

class HasEnv a => HasCtx a where
  ctxId         :: Lens' a Ctx
  ctxEnv        :: Lens' a Env
  ctxLog        :: Lens' a Log
  ctxPreamble   :: Lens' a Text

  ctxEnv        = ctxId . lens _ctxEnv        (\s a -> s { _ctxEnv = a } )
  ctxLog        = ctxId . lens _ctxLog        (\s a -> s { _ctxLog = a } )
  ctxPreamble   = ctxId . lens _ctxPreamble   (\s a -> s { _ctxPreamble = a } )

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

type MonadCore e m =
  ( AWSConstraint e m
  , HasCtx e
  , MonadLogger m
  , MonadRandom m
  )

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
  parseJSON (String s) = maybe mzero return (fromText s)
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
  toVar LevelDebug     = "debug"
  toVar LevelInfo      = "info"
  toVar LevelWarn      = "warn"
  toVar LevelError     = "error"
  toVar (LevelOther s) = unpack s

  fromVar "debug" = return LevelDebug
  fromVar "info"  = return LevelInfo
  fromVar "warn"  = return LevelWarn
  fromVar "error" = return LevelError
  fromVar s       = return $ LevelOther (pack s)

instance (Var a, Show a, Read a) => Var (Maybe a) where
  toVar (Just v) = (unpack . show) v
  toVar Nothing  = ""

  fromVar = readMaybe

-- | A record type representing full or partial configuration of an
-- HTTP service. Remaining unspecified fields are filled in with the
-- default values from Default.
--
data Conf = Conf
  { _confFile     :: Maybe String   -- ^ Service configuration file location
  , _confPort     :: Maybe Int      -- ^ Port to listen on
  , _confTimeout  :: Maybe Int      -- ^ Connection timeout (sec)
  , _confLogLevel :: Maybe LogLevel -- ^ Logging level
  } deriving ( Eq, Show )

class HasConf a where
  confId       :: Lens' a Conf
  confFile     :: Lens' a (Maybe String)
  confPort     :: Lens' a (Maybe Int)
  confTimeout  :: Lens' a (Maybe Int)
  confLogLevel :: Lens' a (Maybe LogLevel)

  confFile      = confId . lens _confFile     (\s a -> s { _confFile = a } )
  confPort      = confId . lens _confPort     (\s a -> s { _confPort = a } )
  confTimeout   = confId . lens _confTimeout  (\s a -> s { _confTimeout = a } )
  confLogLevel  = confId . lens _confLogLevel (\s a -> s { _confLogLevel = a } )

instance HasConf Conf where
  confId = id

instance Default Conf where
  def = Conf
    { _confFile     = Just "conf/dev.yaml"
    , _confPort     = Just 5000
    , _confTimeout  = Just 120
    , _confLogLevel = Just LevelInfo
    }

instance FromJSON Conf where
  parseJSON (Object v) =
    Conf                <$>
      v .:? "conf-file" <*>
      v .:? "port"      <*>
      v .:? "timeout"   <*>
      v .:? "log-level"
  parseJSON _ = mzero

instance FromEnv Conf where
  fromEnv =
    Conf                           <$>
      envMaybe "SKYLARK_CONF_FILE" <*>
      envMaybe "SKYLARK_PORT"      <*>
      envMaybe "SKYLARK_TIMEOUT"   <*>
      envMaybe "SKYLARK_LOG_LEVEL"

instance Monoid Conf where
  mempty = Conf
    { _confFile     = Nothing
    , _confPort     = Nothing
    , _confTimeout  = Nothing
    , _confLogLevel = Nothing
    }

  mappend a b = Conf
    { _confFile     = merge _confFile a b
    , _confPort     = merge _confPort a b
    , _confTimeout  = merge _confTimeout a b
    , _confLogLevel = merge _confLogLevel a b
    }

-- | Given a record field accessor. return the second non-Nothing
-- Value for a record field.
--
merge :: (a -> Maybe b) -> a -> a -> Maybe b
merge f a b = getLast $! (mappend `on` (Last . f)) a b

type MonadConf a =
  ( HasConf  a
  , Default  a
  , FromJSON a
  , FromEnv  a
  , Monoid   a
  )

data ConfException = MandatoryConfException String
  deriving ( Show, Eq )

instance Exception ConfException
