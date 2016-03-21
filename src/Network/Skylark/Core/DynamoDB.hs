{-# LANGUAGE DefaultSignatures #-}

-- |
-- Module:      Network.Skylark.Core.DynamoDB
-- Copyright:   (c) 2016 Joshua Gross
-- License:     BSD3
-- Maintainer:  Joshua Gross <josh@swift-nav.com>
--
-- Common types shared internally in Skylark API model types.

module Network.Skylark.Core.DynamoDB where

import           Control.Lens                 hiding ((.=))
import           Data.Aeson                   as A
import qualified Data.HashMap.Strict          as M
import qualified Data.Vector                  as V
import           Network.AWS.DynamoDB
import           Network.Skylark.Core.Prelude

-- Convert from AttributeValue Map for DynamoDB
class FromAttributeValue a where
  decodeItem :: HashMap Text AttributeValue -> Maybe a

  default decodeItem :: A.FromJSON a => HashMap Text AttributeValue -> Maybe a
  decodeItem x
    | Success b <- getResult x = Just b
    | otherwise                = Nothing
    where
      getResult = fromJSON . Object . M.map attributeValueToValue

-- Convert to AttributeValue Map for DynamoDB
class ToAttributeValue a where
  encodeItem :: a -> HashMap Text AttributeValue

  default encodeItem :: A.ToJSON a => a -> HashMap Text AttributeValue
  encodeItem = (^. avM) . valueToAttributeValue . A.toJSON

-- DynamoDB responses will always contain a list of 0-n items...
fromQueryResponse :: FromAttributeValue a => QueryResponse -> [a]
fromQueryResponse q = mapMaybe decodeItem (q ^. qrsItems)

-- | Convert a JSON value to an AttributeValue.
--
-- Code inspired by a thread on Amazonka:
-- https://github.com/brendanhay/amazonka/issues/263#issuecomment-175295969
valueToAttributeValue :: A.Value -> AttributeValue
valueToAttributeValue (A.String v)  = attributeValue & avS    .~ Just v
valueToAttributeValue (A.Number v)  = attributeValue & avN    .~ Just (show v)
valueToAttributeValue (A.Bool v)    = attributeValue & avBOOL .~ Just v
valueToAttributeValue (A.Array vs)  =
  attributeValue & avL    .~ fmap valueToAttributeValue (V.toList vs)
valueToAttributeValue (A.Object v)  =
  attributeValue & avM    .~ fmap valueToAttributeValue v
valueToAttributeValue A.Null        = attributeValue & avNULL .~ Just True

-- | Convert an AttributeValue to a Value.
--
-- Again, code inspired by a thread on Amazonka:
-- https://github.com/brendanhay/amazonka/issues/263#issuecomment-175295969
attributeValueToValue :: AttributeValue -> A.Value
attributeValueToValue av
  | Just v <- av ^. avS                 = A.String v
  | Just v <- av ^. avN                 = A.Number $ read v
  | Just v <- av ^. avBOOL              = A.Bool v
  |      v <- av ^. avM, not (M.null v) = A.Object $ fmap attributeValueToValue v
  |     vs <- av ^. avL                 = A.Array (attributeValueToValue <$> V.fromList vs)
  | Just _ <- av ^. avNULL              = A.Null
  | otherwise                           = A.Null
