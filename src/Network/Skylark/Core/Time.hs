{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

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
    -- * Time
      Format (..)
    , Time   (..)
    -- * Formats
    , ISO8601
    -- * Helpers
    , toUTCTime
    , convert
    ) where

import Data.Aeson
import Data.Data                    (Data)
import Data.Tagged
import Data.Text
import Data.Time                    (UTCTime, defaultTimeLocale)
import Data.Time.Format             (formatTime, parseTimeM)
import GHC.Generics                 (Generic)
import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types

data Format
  = ISO8601Format
    deriving (Eq, Read, Show, Data, Typeable, Generic)

deriving instance Typeable 'ISO8601Format

data Time :: Format -> * where
  Time :: UTCTime -> Time a
    deriving (Data, Typeable, Generic)

deriving instance Eq   (Time a)
deriving instance Ord  (Time a)
deriving instance Read (Time a)
deriving instance Show (Time a)

convert :: Time a -> Time b
convert (Time t) = Time t

-- Types -> tagged Time types
type ISO8601 = Time 'ISO8601Format

class TimeFormat a where
  format :: Tagged a String

toUTCTime :: Time a -> UTCTime
toUTCTime (Time a) = a

-- Define time formats for formatting dates
instance TimeFormat ISO8601 where
  format = Tagged "%FT%T%z"

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

-- Parse time.
parseTime :: forall a m. TimeFormat (Time a) => Monad m => Text -> m (Time a)
parseTime t =
  Time <$> parseTimeM False defaultTimeLocale (untag f) (unpack t)
  where
    f :: Tagged (Time a) String
    f = format

-- From amazonka. Given a tagged time format, untag it, get its formatter and
--  format the date.
renderFormattedTime :: forall a. TimeFormat (Time a) => Time a -> String
renderFormattedTime (Time t) =
  formatTime defaultTimeLocale (untag f) t
  where
    f :: Tagged (Time a) String
    f = format
