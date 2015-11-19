-- |
-- Module:      Network.Skylark.Core.Upserts
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Upserts module for Skylark Core.

module Network.Skylark.Core.Upserts
  ( open
  , close
  ) where

import           Control.Lens
import qualified Data.HashMap.Strict as M
import           Network.AWS.DynamoDB
import           Network.Skylark.Core.Prelude
import           Network.Skylark.Core.Types

update :: HasUpsert a => a -> Text -> [Text] -> UpdateItem
update item expr exprs =
  updateItem (item ^. upsertTable) &
    uiKey .~ item ^. upsertKey &
    uiConditionExpression .~ Just expr &
    uiUpdateExpression .~ Just updateExpr &
    uiExpressionAttributeValues .~ updateVals where
      updateExpr = "SET " <> intercalate ", " exprs
      updateVals = item ^. upsertVals <> M.fromList
        [ (":time", attributeValue & avS .~ Just (txt $ item ^. upsertTime))
        ]

open :: HasUpsert a => a -> UpdateItem
open item =
  update item expr exprs where
    expr = "attribute_not_exists(updated_at) OR updated_at <= :time"
    exprs = item ^. upsertExprs <>
      [ "opened_at  = if_not_exists(opened_at, :time)"
      , "updated_at = if_not_exists(closed_at, :time)"
      ]

close :: HasUpsert a => a -> UpdateItem
close item =
  update item expr exprs where
    expr = "attribute_not_exists(updated_at) OR updated_at <= :time"
    exprs = item ^. upsertExprs <>
      [ "opened_at  = if_not_exists(opened_at, :time)"
      , "updated_at = if_not_exists(closed_at, :time)"
      , "closed_at  = if_not_exists(closed_at, :time)"
      ]
