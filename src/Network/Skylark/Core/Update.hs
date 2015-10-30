-- |
-- Module:      Network.Skylark.Core.Update
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Update module for Skylark Core.

module Network.Skylark.Core.Update
  ( Update (..)
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
  key :: a -> AttributeValueMap

  openValues :: a -> AttributeValueMap
  openValues _item = M.empty

  closeValues :: a -> AttributeValueMap
  closeValues _item = M.empty

  openUpdateExprs :: a -> [Text]
  openUpdateExprs _item = mempty

  closeUpdateExprs :: a -> [Text]
  closeUpdateExprs _item = mempty

  openItem :: AWSConstraint e m => Text -> a -> m ()
  openItem table item = do
    t <- liftIO $ timestamp
    void $ send $
      updateItem table &
        uiKey .~ key item &
        uiExpressionAttributeValues .~ values t &
        uiConditionExpression .~ Just conditionExpr &
        uiUpdateExpression .~ Just updateExpr where
          values t = openValues item <>
            M.fromList
              [ (":t", attributeValue & avS .~ Just t)
              ]
          conditionExpr = "attribute_not_exists(updated_at) OR updated_at <= :t"
          updateExpr = "SET " <> intercalate ", " updateExprs
          updateExprs = openUpdateExprs item <>
            [ "opened_at  = if_not_exists(opened_at, :t)"
            , "updated_at = if_not_exists(closed_at, :t)"
            ]

  closeItem :: AWSConstraint e m => Text -> a -> m ()
  closeItem table item = do
    t <- liftIO $ timestamp
    void $ send $
      updateItem table &
        uiKey .~ key item &
        uiExpressionAttributeValues .~ values t &
        uiConditionExpression .~ Just conditionExpr &
        uiUpdateExpression .~ Just updateExpr where
          values t = closeValues item <>
            M.fromList
              [ (":t", attributeValue & avS .~ Just t)
              ]
          conditionExpr = "attribute_not_exists(updated_at) OR updated_at <= :t"
          updateExpr = "SET " <> intercalate ", " updateExprs
          updateExprs = closeUpdateExprs item <>
            [ "opened_at  = if_not_exists(opened_at, :t)"
            , "updated_at = if_not_exists(closed_at, :t)"
            , "closed_at  = if_not_exists(closed_at, :t)"
            ]

timestamp :: IO Text
timestamp = do
  t <- getCurrentTime
  return $ txt $ formatTime defaultTimeLocale "%FT%T%z" t
