{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module:      Network.Skylark.API.Time
-- Copyright:   (c) 2015 Joshua Gross
-- License:     BSD3
-- Maintainer:  Joshua Gross <josh@swift-nav.com>
--
-- Time module for Skylark services.
-- Largely inspired by Amazonka's Data.Time.
--
-- TODOs: this can easily be extended to allow multiple date formats
--  per `Format` type, for slightly more flexible parsing.

module Network.Skylark.Core.Time
  (
  -- * Formats
    ISO8601 (..)
  -- * Helpers
  , IsUTCTime
  , toUtcTime
  , parseTime
  , renderFormattedTime
  -- * Re-export Data.Time
  , module Data.Time
  ) where

import Data.Aeson
import Data.Data                    (Data)
import Data.Text
import Data.Time                    hiding (parseTime)
import GHC.Generics                 (Generic)
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types

newtype ISO8601 = ISO8601 { iso8601 :: UTCTime }
  deriving ( Eq, Read, Show, Data, Typeable, Generic )

class IsUTCTime a where
  toUtcTime :: a -> UTCTime
  parseTime :: Monad m => Text -> m a
  renderFormattedTime :: a -> String

-- Txt instances
instance Txt ISO8601 where
  txt = pack . renderFormattedTime

-- UnTxt instances
instance UnTxt ISO8601 where
  untxt = parseTime

-- FromJSON instances
instance FromJSON ISO8601 where
  parseJSON (String s) = untxt s
  parseJSON _ = mzero

-- ToJSON instances
instance ToJSON ISO8601 where
  toJSON = toJSON . txt

-- TimeFormat
instance IsUTCTime ISO8601 where
  toUtcTime = iso8601
  parseTime = fmap ISO8601 . parseTimeM False defaultTimeLocale "%FT%T%z" . unpack
  renderFormattedTime = formatTime defaultTimeLocale "%FT%T%z" . toUtcTime
