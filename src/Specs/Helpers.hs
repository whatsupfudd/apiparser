{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Specs.Helpers (DirectOrRef (..), ReferenceObject (..), XmlObject (..), ExternalDocs (..)) where

import Data.Text ( Text )
import Data.Aeson ( FromJSON, ToJSON, (.:), (.:?), (.=), (.!=), object, withObject, parseJSON, toJSON, Value (..) )
import qualified Data.Aeson as Aes
import qualified Data.Aeson.KeyMap as Aes
import qualified Data.Aeson.Types as Aes
import GHC.Generics ( Generic )
import qualified Data.Map as Mp


data DirectOrRef direct =
  DirectProxy direct
  | RefProxy ReferenceObject
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON direct => FromJSON (DirectOrRef direct) where
  parseJSON :: Value -> Aes.Parser (DirectOrRef direct)
  parseJSON = withObject "DirectOrRef" $ \o -> do
    if Aes.member "$ref" o
      then RefProxy <$> parseJSON (Object o)
      else DirectProxy <$> parseJSON (Object o)


data ReferenceObject = ReferenceObject {
  ref :: Text
  , summary :: Maybe Text
  , description :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ReferenceObject where
  parseJSON :: Value -> Aes.Parser ReferenceObject
  parseJSON = withObject "ReferenceObject" $ \o -> do
    ref <- o .: "$ref"
    summary <- o .:? "summary"
    description <- o .:? "description"
    pure $ ReferenceObject ref summary description

 
data XmlObject = XmlObject {
  name :: Maybe Text
  , namespace :: Maybe Text
  , prefix :: Maybe Text
  , attribute :: Maybe Bool
  , wrapped :: Maybe Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- TODO:
data ExternalDocs = ExternalDocs {
  url :: Text
  , description :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

