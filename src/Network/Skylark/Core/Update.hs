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
  , open
  , close
  , key
  , openExprs
  , closeExprs
  , openVals
  , closeVals
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
  update :: AWSConstraint e m => Text -> Text -> [Text] -> AttributeValueMap -> a -> m ()
  update table expr exprs vals item = do
    t <- liftIO timestamp
    void $ send $
      updateItem table &
        uiKey .~ key item &
        uiConditionExpression .~ Just expr &
        uiUpdateExpression .~ Just updateExpr &
        uiExpressionAttributeValues .~ updateVals t where
          updateExpr = "SET " <> intercalate ", " exprs
          updateVals t = vals <> M.fromList
            [ (":t", attributeValue & avS .~ Just t)
            ]

  open :: AWSConstraint e m => Text -> a -> m ()
  open table item = update table expr exprs vals item where
    expr = "attribute_not_exists(updated_at) OR updated_at <= :t"
    exprs = openExprs item <>
      [ "opened_at  = if_not_exists(opened_at, :t)"
      , "updated_at = if_not_exists(closed_at, :t)"
      ]
    vals = openVals item

  close :: AWSConstraint e m => Text -> a -> m ()
  close table item = update table expr exprs vals item where
    expr = "attribute_not_exists(updated_at) OR updated_at <= :t"
    exprs = closeExprs item <>
      [ "opened_at  = if_not_exists(opened_at, :t)"
      , "updated_at = if_not_exists(closed_at, :t)"
      , "closed_at  = if_not_exists(closed_at, :t)"
      ]
    vals = closeVals item

  key :: a -> AttributeValueMap

  openExprs :: a -> [Text]
  openExprs _item = mempty

  closeExprs :: a -> [Text]
  closeExprs = openExprs

  openVals :: a -> AttributeValueMap
  openVals _item = M.empty

  closeVals :: a -> AttributeValueMap
  closeVals = openVals

timestamp :: IO Text
timestamp = do
  t <- getCurrentTime
  return $ txt $ formatTime defaultTimeLocale "%FT%T%z" t
