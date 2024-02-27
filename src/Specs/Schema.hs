{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Specs.Schema where

import Data.Text ( Text )
import Data.Aeson ( FromJSON, ToJSON, (.:), (.:?), (.=), (.!=), object, withObject, parseJSON, toJSON, Value (..) )
import qualified Data.Aeson as Aes
import qualified Data.Aeson.KeyMap as Aes
import qualified Data.Aeson.Types as Aes
import GHC.Generics ( Generic )
import qualified Data.Map as Mp


import Specs.Helpers ( DirectOrRef(..), ReferenceObject(..), ExternalDocs(..), XmlObject(..) )

-- OK:
data Schema = Schema {
    -- OAS definition:
    type_ :: Maybe Text
    , description :: Maybe Text
    , properties :: Maybe (Mp.Map Text Schema)
    , required :: Maybe [Text]
    -- , items :: Maybe (Mp.Map Text (DirectOrRef Schema))
    , enum :: Maybe [Text]
    , format :: Maybe Text
    , default_ :: Maybe Value
    , minimum :: Maybe Value
    , maximum :: Maybe Value
    , anyOf :: Maybe [DirectOrRef Schema]
    , allOf :: Maybe [DirectOrRef Schema]
    , oneOf :: Maybe [DirectOrRef Schema]
    , not_ :: Maybe (DirectOrRef Schema)
    , nullable :: Maybe Bool    
    , readOnly :: Maybe Bool
    , writeOnly :: Maybe Bool
    -- OpenAPI extra components:
    , discriminator :: Maybe Discriminator
    , xml :: Maybe XmlObject
    , externalDocs :: Maybe ExternalDocs
    , example :: Maybe Value
    , extensions :: Maybe (Mp.Map Text Value)
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Schema where
  parseJSON :: Value -> Aes.Parser Schema
  parseJSON = withObject "Schema" $ \o -> do
    type_ <- o .:? "type"
    description <- o .:? "description"
    properties <- o .:? "properties"
    required <- o .:? "required"
    -- items <- o .:? "items"
    enum <- o .:? "enum"
    format <- o .:? "format"
    default_ <- o .:? "default"
    minimum <- o .:? "minimum"
    maximum <- o .:? "maximum"
    anyOf <- o .:? "anyOf"
    allOf <- o .:? "allOf"
    oneOf <- o .:? "oneOf"
    not_ <- o .:? "not"
    nullable <- o .:? "nullable"
    readOnly <- o .:? "readOnly"
    writeOnly <- o .:? "writeOnly"
    discriminator <- o .:? "discriminator"
    xml <- o .:? "xml"
    externalDocs <- o .:? "externalDocs"
    example <- o .:? "example"
    extensions <- o .:? "x-oaiMeta"
    pure $ Schema type_ description properties required enum
                  format default_ minimum maximum
                  anyOf allOf oneOf not_
                  nullable readOnly writeOnly
                  discriminator xml externalDocs example extensions


-- OK:
data Discriminator = Discriminator {
  propertyName :: Text
  , mapping :: Maybe (Mp.Map Text Text)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
