{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Specs.OpenApi where

import Data.Text ( Text )
import Data.Aeson ( FromJSON, ToJSON, (.:), (.:?), (.=), (.!=), object, withObject, parseJSON, toJSON, Value (..) )
import qualified Data.Aeson as Aes
import qualified Data.Aeson.KeyMap as Aes
import qualified Data.Aeson.Types as Aes
import GHC.Generics ( Generic )
import qualified Data.Map as Mp

import Specs.Schema
import Specs.Helpers


-- Auto-defini par Copilot:
data Path = Path {
    path :: Text
    , operations :: [Operation]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


data Content = Content {
  name :: String
  , in_ :: String
  , description :: String
  , required :: Bool
  , deprecated :: Bool
  , allowEmptyValue :: Bool
  , style :: String
  , explode :: Bool
  , allowReserved :: Bool
  , schema :: Schema
  , example :: String
  , examples :: [Example]
  , content :: [Content]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- ^^^ Auto-defini par Copilot:

-- CHECK VVV:

data Security = Security
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Callback = Callback
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Encoding = Encoding {
  contentType :: Maybe Text
  , headers :: Maybe (Mp.Map Text (DirectOrRef Header))
  , style :: Maybe Text
  , explode :: Maybe Bool
  , allowReserved :: Maybe Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- CHECK ^^^:

-- OK:
data Info = Info {
  title :: Text
  , summary :: Maybe Text
  , description :: Maybe Text
  , termsOfService :: Maybe Text
  , contact :: Maybe Contact
  , license :: Maybe License
  , version :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data Contact = Contact {
  name :: Text
  , url :: Text
  , email :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data License = License {
  name :: Text
  , url :: Maybe Text
  , identifier :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- OK:
data Server = Server {
  url :: Text
  , description :: Maybe Text
  , variables :: Maybe [SrvVariable]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- OK:
data SrvVariable = SrvVariable {
  enum :: [Text]
  , default_ :: Text
  , description :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- OK:
data Components = Components {
  schemas :: Mp.Map Text Schema
  , responses :: Maybe (Mp.Map Text (DirectOrRef Response))
  , parameters :: Maybe (Mp.Map Text (DirectOrRef Parameter))
  , examples :: Maybe (Mp.Map Text (DirectOrRef Example))
  , requestBodies :: Maybe (Mp.Map Text (DirectOrRef RequestBody))
  , headers :: Maybe (Mp.Map Text (DirectOrRef Header))
  , securitySchemes :: Maybe (Mp.Map Text (DirectOrRef SecurityScheme))
  , links :: Maybe (Mp.Map Text (DirectOrRef Link))
  , callbacks :: Maybe (Mp.Map Text (DirectOrRef Callback))
  , pathItems :: Maybe (Mp.Map Text (DirectOrRef PathItem))
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data SecurityScheme = SecurityScheme {
  type_ :: Text
  , description :: Maybe Text
  , name :: Maybe Text  -- REQUIRED (openai misses it).
  , in_ :: Maybe Text  -- REQUIRED (openai misses it).
  , scheme :: Text
  , bearerFormat :: Maybe Text
  , flows :: Maybe OAuthFlows   -- REQUIRED (openai misses it).
  , openIdConnectUrl :: Maybe Text   -- REQUIRED (openai misses it).
  , extensions :: Maybe (Mp.Map Text Value)
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON SecurityScheme where
  parseJSON :: Value -> Aes.Parser SecurityScheme
  parseJSON = withObject "SecurityScheme" $ \o -> do
    type_ <- o .: "type"
    description <- o .:? "description"
    name <- o .:? "name"
    in_ <- o .:? "in"
    scheme <- o .: "scheme"
    bearerFormat <- o .:? "bearerFormat"
    flows <- o .:? "flows"
    openIdConnectUrl <- o .:? "openIdConnectUrl"
    extensions <- o .:? "x-oaiMeta"
    pure $ SecurityScheme type_ description name in_ scheme bearerFormat flows openIdConnectUrl extensions

-- OK:
data OAuthFlows = OAuthFlows {
  implicit :: Maybe OAuthFlow
  , password :: Maybe OAuthFlow
  , clientCredentials :: Maybe OAuthFlow
  , authorizationCode :: Maybe OAuthFlow
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data OAuthFlow = OAuthFlow {
  authorizationUrl :: Text
  , tokenUrl :: Text
  , refreshUrl :: Maybe Text
  , scopes :: Mp.Map Text Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data PathItem = PathItem {
  ref :: Maybe Text
  , summary :: Maybe Text
  , description :: Maybe Text
  , get :: Maybe Operation
  , put :: Maybe Operation
  , post :: Maybe Operation
  , delete :: Maybe Operation
  , options :: Maybe Operation
  , head :: Maybe Operation
  , patch :: Maybe Operation
  , trace :: Maybe Operation
  , servers :: Maybe [Server]
  , parameters :: Maybe [DirectOrRef Parameter]
  , extensions :: Maybe (Mp.Map Text Value)
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON PathItem where
  parseJSON :: Value -> Aes.Parser PathItem
  parseJSON = withObject "PathItem" $ \o -> do
    ref <- o .:? "ref"
    summary <- o .:? "summary"
    description <- o .:? "description"
    get <- o .:? "get"
    put <- o .:? "put"
    post <- o .:? "post"
    delete <- o .:? "delete"
    options <- o .:? "options"
    head <- o .:? "head"
    patch <- o .:? "patch"
    trace <- o .:? "trace"
    servers <- o .:? "servers"
    parameters <- o .:? "parameters"
    extensions <- o .:? "x-oaiMeta"
    pure $ PathItem ref summary description get put post delete options head patch trace servers parameters extensions


type SecurityRequirement = Mp.Map Text [Text]

-- OK:
data Operation = Operation {
  tags :: Maybe [Text]
  , summary :: Maybe Text
  , description :: Maybe Text
  , externalDocs :: Maybe ExternalDocs
  , operationId :: Maybe Text
  , parameters :: Maybe [DirectOrRef Parameter]
  , requestBody :: Maybe (DirectOrRef RequestBody)
  , responses :: Maybe (Mp.Map Text (DirectOrRef Response))
  , callbacks :: Maybe (Mp.Map Text (DirectOrRef Callback))
  , deprecated :: Maybe Bool
  , security :: Maybe SecurityRequirement
  , servers :: Maybe [Server]
  , extensions :: Maybe (Mp.Map Text Value)
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Operation where
  parseJSON :: Value -> Aes.Parser Operation
  parseJSON = withObject "Operation" $ \o -> do
    tags <- o .:? "tags"
    summary <- o .:? "summary"
    description <- o .:? "description"
    externalDocs <- o .:? "externalDocs"
    operationId <- o .:? "operationId"
    parameters <- o .:? "parameters"
    requestBody <- o .:? "requestBody"
    responses <- o .:? "responses"
    callbacks <- o .:? "callbacks"
    deprecated <- o .:? "deprecated"
    security <- o .:? "security"
    servers <- o .:? "servers"
    extensions <- o .:? "x-oaiMeta"
    pure $ Operation tags summary description externalDocs operationId parameters requestBody responses callbacks deprecated security servers extensions


-- OK:
data Parameter = Parameter {
  name :: Text
  , in_ :: Text
  , description :: Maybe Text
  , required :: Maybe Bool
  , deprecated :: Maybe Bool
  , allowEmptyValue :: Maybe Bool
  , style :: Maybe Text
  , explode :: Maybe Bool
  , allowReserved :: Maybe Bool
  , schema :: Maybe Schema
  , example :: Maybe Value
  , examples :: Maybe (Mp.Map Text (DirectOrRef Example))
  , content :: Maybe (Mp.Map Text MediaType)
  , extensions :: Maybe (Mp.Map Text Value)
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Parameter where
  parseJSON :: Value -> Aes.Parser Parameter
  parseJSON = withObject "Parameter" $ \o -> do
    name <- o .: "name"
    in_ <- o .: "in"
    description <- o .:? "description"
    required <- o .:? "required"
    deprecated <- o .:? "deprecated"
    allowEmptyValue <- o .:? "allowEmptyValue"
    style <- o .:? "style"
    explode <- o .:? "explode"
    allowReserved <- o .:? "allowReserved"
    schema <- o .:? "schema"
    example <- o .:? "example"
    examples <- o .:? "examples"
    content <- o .:? "content"
    extensions <- o .:? "x-oaiMeta"
    pure $ Parameter name in_ description required deprecated allowEmptyValue style explode allowReserved schema example examples content extensions

-- OK:
data Example = Example {
  summary :: Maybe Text
  , description :: Maybe Text
  , value :: Maybe Value
  , externalValue :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- OK:
data MediaType = MediaType {
  schema :: Maybe (DirectOrRef Schema)
  , example :: Maybe Value
  , examples :: Maybe (Mp.Map Text (DirectOrRef Example))
  , encoding :: Maybe (Mp.Map Text Encoding)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- OK:
data Response = Response { 
  description :: Text
  , headers :: Maybe (Mp.Map Text (DirectOrRef Header))
  , content :: Maybe (Mp.Map Text MediaType)
  , links :: Maybe (Mp.Map Text (DirectOrRef Link))
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data Header = Header {
  description :: Maybe Text
  , required :: Maybe Bool
  , deprecated :: Maybe Bool
  , allowEmptyValue :: Maybe Bool
  , style :: Maybe Text
  , explode :: Maybe Bool
  , allowReserved :: Maybe Bool
  , schema :: Maybe Schema
  , example :: Maybe Value
  , examples :: Maybe (Mp.Map Text (DirectOrRef Example))
  , content :: Maybe (Mp.Map Text MediaType)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data RequestBody = RequestBody {
  description :: Maybe Text
  , content :: Maybe (Mp.Map Text MediaType)
  , required :: Maybe Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data Link = Link {
  operationRef :: Maybe Text
  , operationId :: Maybe Text
  , parameters :: Maybe (Mp.Map Text Text)
  , requestBody :: Maybe Text
  , description :: Maybe Text
  , server :: Maybe Server
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- OK:
data Tag = Tag {
  name :: Text
  , description :: Maybe Text
  , externalDocs :: Maybe ExternalDocs
  , extensions :: Maybe (Mp.Map Text Value)
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Tag where
  parseJSON :: Value -> Aes.Parser Tag
  parseJSON = withObject "Tag" $ \o -> do
    name <- o .: "name"
    description <- o .:? "description"
    externalDocs <- o .:? "externalDocs"
    extensions <- o .:? "x-oaiMeta"
    pure $ Tag name description externalDocs extensions


-- TODO:

-- Main:
data OpenAPI = OpenAPI {
  openapi :: String       -- ok.
  , info :: Info       -- ok.
  , servers :: [Server]       -- ok.
  , paths :: Mp.Map Text PathItem     -- ok.
  , webhooks :: Maybe (Mp.Map Text (DirectOrRef PathItem))     -- ok.
  , components :: Components   -- ok.
  , security :: Maybe [Mp.Map Text [Text]]   -- ok.
  , tags :: Maybe [Tag]
  , externalDocs :: Maybe ExternalDocs
  , extensions :: Maybe (Mp.Map Text Value)
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON OpenAPI where
  parseJSON :: Value -> Aes.Parser OpenAPI
  parseJSON = withObject "OpenAPI" $ \o -> do
    openapi <- o .: "openapi"
    info <- o .: "info"
    servers <- o .: "servers"
    paths <- o .: "paths"
    webhooks <- o .:? "webhooks"
    components <- o .: "components"
    security <- o .:? "security"
    tags <- o .:? "tags"
    externalDocs <- o .:? "externalDocs"
    extensions <- o .:? "x-oaiMeta"
    pure $ OpenAPI openapi info servers paths webhooks components security tags externalDocs extensions
