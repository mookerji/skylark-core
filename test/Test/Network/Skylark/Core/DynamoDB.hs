{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS  -fno-warn-orphans #-}

-- |
-- Module:      Test.Network.Skylark.Core.DynamoDB
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Test DynamoDB module for Skylark Core.

module Test.Network.Skylark.Core.DynamoDB
  ( tests
  ) where


import           Control.Lens
import           Data.Aeson
import           Data.Derive.Arbitrary
import           Data.DeriveTH
import qualified Data.Text                      as T
import           GHC.Generics
import           Network.Skylark.Core.Data.JSON
import           Network.Skylark.Core.DynamoDB
import           Network.Skylark.Core.Prelude
import           Network.Skylark.Core.TH
import           Network.Skylark.Core.Types
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

data ADT = A1 | A2 | A3 deriving (Eq, Show, Read, Generic)

$(derive makeArbitrary ''ADT)

$(deriveNullary ''ADT)

data Format = Format
  { _fmtName         :: T.Text
  , _fmtMajorVersion :: T.Text
  } deriving (Eq, Show, Generic)

$(deriveJSON ''Format)

$(makeLenses ''Format)

$(derive makeArbitrary ''Format)

instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary

data SampleRecord = SampleRecord
  { _srText        :: T.Text
  , _sfTextArray   :: [T.Text]
  , _sfMaybeText   :: Maybe T.Text
  , _sfFormat      :: Format
  , _sfFormatArray :: [Format]
  , _sfAdt         :: ADT
  , _sfAdts        :: [ADT]
  , _sfDouble      :: Double
  , _sfMaybeDouble :: Maybe Double
  , _sfInt         :: Int
  , _sfMaybeInt    :: Maybe Int
  , _sfIntArray    :: [Int]
  , _sfBool        :: Bool
  , _sfMaybeBool   :: Maybe Bool
  } deriving ( Eq, Show, Generic )

defaultRecord :: SampleRecord
defaultRecord = SampleRecord
  { _srText        = "foo"
  , _sfTextArray   = ["foo", "bar"]
  , _sfMaybeText   = Just "baz"
  , _sfFormat      = Format "foo" "bar"
  , _sfFormatArray = [Format "foo" "bar", Format "baz" "bazbar"]
  , _sfAdt         = A1
  , _sfAdts        = [A1, A2]
  , _sfDouble      = 2.5
  , _sfMaybeDouble = Just 2.5
  , _sfInt         = 2
  , _sfMaybeInt    = Just 3
  , _sfIntArray    = [2, 3, 4]
  , _sfBool        = False
  , _sfMaybeBool   = Just False
  }

$(deriveJSON ''SampleRecord)

$(makeLenses ''SampleRecord)

instance ToAttributeValue SampleRecord

instance FromAttributeValue SampleRecord

$(derive makeArbitrary ''SampleRecord)

type Model a =
  ( Arbitrary a
  , Eq a
  , Show a
  , FromJSON a
  , ToJSON a
  , ToAttributeValue a
  , FromAttributeValue a
  )

ddbRoundTrip :: Model a => a -> IO Bool
ddbRoundTrip model = return $ decodeItem (encodeItem model) == Just model

mkQuickCheckTests :: Model a => String -> a -> TestTree
mkQuickCheckTests name example =
  testGroup (name <> " QuickCheck")
    [ testProperty "DDB round trip" $
        \model ->
          monadicIO $ do
            result <- lift $ ddbRoundTrip (model `asTypeOf` example)
            Test.QuickCheck.Monadic.assert result
    ]

tests :: TestTree
tests =
  testGroup "DynamoDB serialization tests"
    [ mkQuickCheckTests "SampleRecord" defaultRecord
    ]
