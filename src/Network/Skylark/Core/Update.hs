-- |
-- Module:      Network.Skylark.Core.Update
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Update module for Skylark Core.

module Network.Skylark.Core.Update
  ( AttributeValueMap
  , Update
  , table
  , time
  , key
  , openExprs
  , openVals
  , closeExprs
  , closeVals
  , open
  , close
  ) where

import           BasicPrelude
import           Control.Lens
import           Control.Monad.Trans.AWS
import qualified Data.HashMap.Strict as M
import           Data.Time
import           Network.AWS.DynamoDB
import           Network.Skylark.Core.Types

type AttributeValueMap = HashMap Text AttributeValue

class Update a where
  table :: a -> Text
  time  :: a -> UTCTime
  key   :: a -> AttributeValueMap

  openExprs :: a -> [Text]
  openExprs _item = mempty

  openVals :: a -> AttributeValueMap
  openVals _item = M.empty

  closeExprs :: a -> [Text]
  closeExprs = openExprs

  closeVals :: a -> AttributeValueMap
  closeVals = openVals

iso8601 :: UTCTime -> Text
iso8601 = txt . formatTime defaultTimeLocale "%FT%T%z"

update :: (AWSConstraint e m, Update a) => a -> Text -> [Text] -> AttributeValueMap -> m ()
update item expr exprs vals =
  void $ send $ updateItem (table item) &
    uiKey .~ key item &
    uiConditionExpression .~ Just expr &
    uiUpdateExpression .~ Just updateExpr &
    uiExpressionAttributeValues .~ updateVals where
      updateExpr = "SET " <> intercalate ", " exprs
      updateVals = vals <> M.fromList
        [ (":time", attributeValue & avS .~ Just (iso8601 $ time item))
        ]

open :: (AWSConstraint e m, Update a) => a -> m ()
open item =
  update item expr exprs (openVals item) where
    expr = "attribute_not_exists(updated_at) OR updated_at <= :time"
    exprs = openExprs item <>
      [ "opened_at  = if_not_exists(opened_at, :time)"
      , "updated_at = if_not_exists(closed_at, :time)"
      ]

close :: (AWSConstraint e m, Update a) => a -> m ()
close item =
  update item expr exprs (closeVals item) where
    expr = "attribute_not_exists(updated_at) OR updated_at <= :time"
    exprs = closeExprs item <>
      [ "opened_at  = if_not_exists(opened_at, :time)"
      , "updated_at = if_not_exists(closed_at, :time)"
      , "closed_at  = if_not_exists(closed_at, :time)"
      ]
