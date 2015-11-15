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

import           BasicPrelude
import           Control.Lens
import           Control.Monad.Trans.AWS
import qualified Data.HashMap.Strict as M
import           Data.Time
import           Network.AWS.DynamoDB
import           Network.Skylark.Core.Types

iso8601 :: UTCTime -> Text
iso8601 = txt . formatTime defaultTimeLocale "%FT%T%z"

update :: MonadUpsert e m a => a -> Text -> [Text] -> m ()
update item expr exprs =
  void $ send $ updateItem (item ^. upsertTable) &
    uiKey .~ item ^. upsertKey &
    uiConditionExpression .~ Just expr &
    uiUpdateExpression .~ Just updateExpr &
    uiExpressionAttributeValues .~ updateVals where
      updateExpr = "SET " <> intercalate ", " exprs
      updateVals = item ^. upsertVals <> M.fromList
        [ (":time", attributeValue & avS .~ Just (iso8601 $ item ^. upsertTime))
        ]

open :: MonadUpsert e m a => a -> m ()
open item =
  update item expr exprs where
    expr = "attribute_not_exists(updated_at) OR updated_at <= :time"
    exprs = item ^. upsertExprs <>
      [ "opened_at  = if_not_exists(opened_at, :time)"
      , "updated_at = if_not_exists(closed_at, :time)"
      ]

close :: MonadUpsert e m a => a -> m ()
close item =
  update item expr exprs where
    expr = "attribute_not_exists(updated_at) OR updated_at <= :time"
    exprs = item ^. upsertExprs <>
      [ "opened_at  = if_not_exists(opened_at, :time)"
      , "updated_at = if_not_exists(closed_at, :time)"
      , "closed_at  = if_not_exists(closed_at, :time)"
      ]
