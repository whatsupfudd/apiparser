{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

module Specs.PrettyPrint (pPrint) where

import Data.Text ( Text, pack, unpack )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Mp
import Data.Aeson ( Value(..) )

import Specs.OpenApi
import Specs.Schema
import Specs.Helpers ( DirectOrRef(..), ReferenceObject(..), ExternalDocs(..), XmlObject(..) )
import GHC.Stack (popCallStack)


labelWith :: Text -> Text -> Text
labelWith label value = label <> value <> "\n"

pTextList :: [Text] -> Text
pTextList aList =
  foldl (\accum t -> case accum of "" -> t; _ -> accum <> ", " <> t) "" aList

pTextMap :: Mp.Map Text Text -> Text
pTextMap aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ": " <> v <> "\n") "" aMap


pPrint :: OpenAPI -> Text
pPrint spec =
  labelWith "info: " (pInfo spec.info)
  <> labelWith "servers:\n" (pServers spec.servers)
  <> labelWith "paths:\n" (pPaths spec.paths)
  <> labelWith "components:\n" (pComponents spec.components)

pInfo :: Info -> Text
pInfo info =
  labelWith "title: " info.title
  <> maybe "" (labelWith "desc: ") info.description
  <> maybe "" (labelWith "termsOfService: ") info.termsOfService
  <> maybe "" (labelWith "contact: " . pContact) info.contact
  <> maybe "" (labelWith "license: " . pLicense) info.license
  <> labelWith "version: " info.version


pContact :: Contact -> Text
pContact contact =
  labelWith "name: " contact.name
  <> labelWith "url: " contact.url
  <> maybe "" (labelWith "email: ") contact.email

pLicense :: License -> Text
pLicense aLicense =
  labelWith "name: " aLicense.name
  <> maybe "" (labelWith "url: ") aLicense.url


pServers :: [Server] -> Text
pServers servers =
  foldl (\accum aServer -> accum <> pServer aServer) "" servers

pServer :: Server -> Text
pServer server =
 labelWith "url: " server.url
  <> maybe "" (labelWith "desc: ") server.description
  <> maybe "" (labelWith "vars: " . pSrvVars) server.variables


pPaths :: Mp.Map Text PathItem -> Text
pPaths aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ":\n" <> pPath v <> "\n") "" aMap

pPath :: PathItem -> Text
pPath aPath =
  maybe "" (labelWith "desc: ") aPath.description
  <> maybe "" (pOperation "get") aPath.get
  <> maybe "" (pOperation "put") aPath.put
  <> maybe "" (pOperation "post") aPath.post
  <> maybe "" (pOperation "delete") aPath.delete
  <> maybe "" (pOperation "options") aPath.options
  <> maybe "" (pOperation "head") aPath.head
  <> maybe "" (pOperation "patch") aPath.patch
  <> maybe "" (pOperation "trace") aPath.trace
  <> maybe "" (labelWith "servers" . pServers) aPath.servers
  <> maybe "" (labelWith "parameters" . pParameters) aPath.parameters


pOperation :: Text -> Operation -> Text
pOperation label anOp =
  label <> ":\n"
  <> maybe "" (labelWith "tags:\n" . pTags) anOp.tags
  <> maybe "" (labelWith "summary: ") anOp.summary
  <> maybe "" (labelWith "description: ") anOp.description
  <> maybe "" (labelWith "operationId: ") anOp.operationId
  <> maybe "" (labelWith "params.:\n" . pParameters) anOp.parameters
  <> maybe "" (labelWith "responses:\n" . pResponses) anOp.responses
  <> maybe "" (labelWith "requestBody:\n" . refRequestBody) anOp.requestBody
  <> maybe "" (labelWith "callbacks:\n" . prettyCallbacks) anOp.callbacks
  <> maybe "" (labelWith "deprecated: " . (pack . show)) anOp.deprecated
  <> maybe "" (labelWith "security:\n" . pSecurityMap) anOp.security
  <> maybe "" (labelWith "servers:\n" . pServers) anOp.servers


pSecurityMap :: Mp.Map Text [Text] -> Text
pSecurityMap aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ": " <> pTextList v) "" aMap


pTags :: [Text] -> Text
pTags aTags =
  foldl (\accum t -> let fT = " - " <> t in case accum of "" -> fT; _ -> accum <> "\n" <> fT) "" aTags


pResponses :: Mp.Map Text (DirectOrRef Response) -> Text
pResponses aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ":\n" <> refResponse v <> "\n") "" aMap

refResponse :: DirectOrRef Response -> Text
refResponse aResp =
  case aResp of
    DirectProxy r -> pResponse r
    RefProxy r -> pReference r

pResponse :: Response -> Text
pResponse aResp =
  "desc.: " <> aResp.description <> "\n"
  <> maybe "" (labelWith "headers:\n" . pHeaders) aResp.headers
  <> maybe "" (labelWith "content:\n" . pMediaTypeMap) aResp.content
  <> maybe "" (labelWith "links:\n" . pLinks) aResp.links


refMediaType :: DirectOrRef MediaType -> Text
refMediaType = \case
  DirectProxy aMediaType -> pMediaType aMediaType
  RefProxy aRef -> pReference aRef

pMediaTypeMap :: Mp.Map Text MediaType -> Text
pMediaTypeMap aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ":\n" <> pMediaType v <> "\n") "" aMap


pMediaType :: MediaType -> Text
pMediaType media =
    maybe "" (labelWith "schema:\n" . refSchema) media.schema
    <> maybe "" (labelWith "example: " . pExample) media.example
    <> maybe "" (labelWith "examples:\n" . pExamples) media.examples
    <> maybe "" (labelWith "encoding: " . pEncodingMap) media.encoding

refSchema :: DirectOrRef Schema -> Text
refSchema = \case
  DirectProxy aSchema -> pSchema aSchema
  RefProxy aRef -> pReference aRef

pSchema :: Schema -> Text
pSchema schema =
  maybe "" (labelWith "desc.:") schema.description
  <> maybe "" (labelWith "type: ") schema.type_
  <> maybe "" (labelWith "format: ") schema.format
  <> maybe "" (labelWith "properties:\n" . pSchemaMap) schema.properties
  <> maybe "" (labelWith "enum: " . pTextList) schema.enum
  <> maybe "" (labelWith "default: " . pExample) schema.default_
  <> maybe "" (labelWith "min: " . pExample) schema.minimum
  <> maybe "" (labelWith "max: " . pExample) schema.maximum
  <> maybe "" (labelWith "nullable: " . (pack . show)) schema.nullable
  <> maybe "" (labelWith "discriminator: " . pDiscriminator) schema.discriminator
  <> maybe "" (labelWith "xml: " . pXml) schema.xml
  <> maybe "" (labelWith "externalDocs: " . pExternalDocs) schema.externalDocs
  <> maybe "" (labelWith "extensions:\n" . (pack . show)) schema.extensions


pXml :: XmlObject -> Text
pXml xmlObj =
  maybe "" (labelWith "name: ") xmlObj.name
  <> maybe "" (labelWith "namespace: ") xmlObj.namespace
  <> maybe "" (labelWith "prefix: ") xmlObj.prefix
  <> maybe "" (labelWith "attribute: " . (pack . show)) xmlObj.attribute
  <> maybe "" (labelWith "wrapped: " . (pack . show)) xmlObj.wrapped

pExternalDocs :: ExternalDocs -> Text
pExternalDocs _ = "TODO."


pDiscriminator :: Discriminator -> Text
pDiscriminator aDisc =
  "discriminator: " <> aDisc.propertyName
  <> maybe "" (labelWith "mapping: " . pTextMap) aDisc.mapping


pEncodingMap :: Mp.Map Text Encoding -> Text
pEncodingMap aMap =
  "encoding: " <>
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ": " <> pEncoding v <> "\n") "" aMap


pEncoding :: Encoding -> Text
pEncoding aEnc =
  maybe "" (labelWith "contentType: ") aEnc.contentType
  <> maybe "" (labelWith "headers: " . pHeaders) aEnc.headers
  <> maybe "" (labelWith "style: ") aEnc.style
  <> maybe "" (labelWith "explode: " . (pack . show)) aEnc.explode
  <> maybe "" (labelWith "allowReserved: " . (pack . show)) aEnc.allowReserved


pExample :: Value -> Text
pExample = \case
  String aVal -> aVal
  Number aVal -> pack (show aVal)
  Bool aVal -> pack (show aVal)
  Null -> "N/A"
  v -> pack $ "unk. val.: " <> show v


refRequestBody :: DirectOrRef RequestBody -> Text
refRequestBody = \case
  DirectProxy reqBd -> pRequestBody reqBd
  RefProxy aRef -> pReference aRef

pRequestBody :: RequestBody -> Text
pRequestBody reqBd =
  maybe "" (labelWith "desc: ") reqBd.description
  <> maybe "" (labelWith "required: " . (pack . show)) reqBd.required
  <> maybe "" (labelWith "content:\n" . pMediaTypeMap) reqBd.content

pComponents :: Components -> Text
pComponents comps =
  labelWith "schemas:\n" (pSchemaMap comps.schemas)
  <> maybe "" (labelWith "responses:\n" . pResponses) comps.responses
  <> maybe "" (labelWith "params.:\n" . prettyCompParams) comps.parameters
  <> maybe "" (labelWith "egs.:\n" . pExamples) comps.examples
  <> maybe "" (labelWith "headers:\n" . pHeaders) comps.headers
  <> maybe "" (labelWith "secu.:\n" . pSecuritySchemes) comps.securitySchemes
  <> maybe "" (labelWith "links:\n" . pLinks) comps.links
  <> maybe "" (labelWith "callbacks:\n" . prettyCallbacks) comps.callbacks

pHeaders :: Mp.Map Text (DirectOrRef Header) -> Text
pHeaders aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ": " <> (labelWith "header: " . refHeader) v) "" aMap


pSecuritySchemes :: Mp.Map Text (DirectOrRef SecurityScheme) -> Text
pSecuritySchemes aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ": " <> refSecurityScheme v) "" aMap


refSecurityScheme :: DirectOrRef SecurityScheme -> Text
refSecurityScheme = \case
  DirectProxy aScheme -> pSecurityScheme aScheme
  RefProxy aRef -> pReference aRef

pSecurityScheme :: SecurityScheme -> Text
pSecurityScheme aScheme =
 labelWith "type: " aScheme.type_
  <> maybe "" (labelWith "desc: ") aScheme.description
  <> maybe "" (labelWith "name: ") aScheme.name
  <> maybe "" (labelWith "in: ") aScheme.in_
  <> labelWith "scheme: " aScheme.scheme
  <> maybe "" (labelWith "bearerFormat: ") aScheme.bearerFormat
  <> maybe "" (labelWith "flows: " . pOAuthFlows) aScheme.flows
  <> maybe "" (labelWith "openIdConnectUrl: ") aScheme.openIdConnectUrl


pOAuthFlows :: OAuthFlows -> Text
pOAuthFlows aFlow =
  maybe "" (labelWith "implicit: " . pOAuthFlow) aFlow.implicit
  <> maybe "" (labelWith "password: " . pOAuthFlow) aFlow.password
  <> maybe "" (labelWith "clientCredentials: " . pOAuthFlow) aFlow.clientCredentials
  <> maybe "" (labelWith "authorizationCode: " . pOAuthFlow) aFlow.authorizationCode

pOAuthFlow :: OAuthFlow -> Text
pOAuthFlow aFlow =
  labelWith "authUrl" aFlow.authorizationUrl
  <> labelWith "tokenUrl" aFlow.tokenUrl
  <> maybe "" (labelWith "refreshUrl: ") aFlow.refreshUrl
  <> labelWith "scopes: " (pTextMap aFlow.scopes)


refHeader :: DirectOrRef Header -> Text
refHeader = \case
  DirectProxy aHeader -> pHeader aHeader
  RefProxy aRef -> pReference aRef

pHeader :: Header -> Text
pHeader aHeader =
  maybe "" (labelWith "desc: ") aHeader.description
  <> maybe "" (labelWith "required: " . (pack . show)) aHeader.required
  <> maybe "" (labelWith "deprecated: " . (pack . show)) aHeader.deprecated
  <> maybe "" (labelWith "allowEmptyValue: " . (pack . show)) aHeader.allowEmptyValue
  <> maybe "" (labelWith "style: ") aHeader.style
  <> maybe "" (labelWith "explode: " . (pack . show)) aHeader.explode
  <> maybe "" (labelWith "allowReserved: " . (pack . show)) aHeader.allowReserved
  <> maybe "" (labelWith "schema: " . pSchema) aHeader.schema
  <> maybe "" (labelWith "example: " . pExample) aHeader.example
  <> maybe "" (labelWith "examples: " . pExamples) aHeader.examples
  <> maybe "" (labelWith "content: " . pMediaTypeMap) aHeader.content


pLinks :: Mp.Map Text (DirectOrRef Link) -> Text
pLinks aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ": " <> (labelWith "link: " . refLink) v) "" aMap

refLink :: DirectOrRef Link -> Text
refLink = \case
  DirectProxy aLink -> pLink aLink
  RefProxy aRef -> pReference aRef

pLink :: Link -> Text
pLink aLink =
  maybe "" (labelWith "opRef: ") aLink.operationRef
  <> maybe "" (labelWith "opId: ") aLink.operationId
  <> maybe "" (labelWith "params: " . pTextMap) aLink.parameters
  <> maybe "" (labelWith "reqBody: ") aLink.requestBody
  <> maybe "" (labelWith "desc: ") aLink.description
  <> maybe "" (labelWith "server: " . pServer) aLink.server

pSrvVars :: [SrvVariable] -> Text
pSrvVars aList =
  foldl (\accum aVar -> accum <> pSrvVar aVar) "" aList

pSrvVar :: SrvVariable -> Text
pSrvVar aVar =
  (labelWith "enum: " . pTextList) aVar.enum
  <> labelWith "default: " aVar.default_
  <> maybe "" (labelWith "desc: ") aVar.description


prettyCallbacks :: Mp.Map Text (DirectOrRef Callback) -> Text
prettyCallbacks aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ": " <> refCallBack v) "" aMap

refCallBack :: DirectOrRef Callback -> Text
refCallBack = \case
  DirectProxy aCallBack -> pCallback aCallBack
  RefProxy aRef -> pReference aRef

pCallback :: Callback -> Text
pCallback aCallBack =
  "TODO."

pSchemaMap :: Mp.Map Text Schema -> Text
pSchemaMap aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ":\n" <> pSchema v) "" aMap


prettyCompParams :: Mp.Map Text (DirectOrRef Parameter) -> Text
prettyCompParams aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> labelWith ": " (refParam v)) "" aMap


pExamples :: Mp.Map Text (DirectOrRef Example) -> Text
pExamples aMap =
  Mp.foldrWithKey (\k v acc -> acc <> " - " <> k <> ": " <> refExample v) "" aMap

refExample :: DirectOrRef Example -> Text
refExample =
  \case
    DirectProxy anEx -> pack $ show anEx
    RefProxy aRef -> pack (show aRef.ref)


pParameters :: [DirectOrRef Parameter] -> Text
pParameters params =
  foldl (\acc v -> acc <> refParam v <> "\n") "" params

refParam :: DirectOrRef Parameter -> Text
refParam = \case
  DirectProxy aParam -> pParameter aParam
  RefProxy aRef -> "param:\n" <> pReference aRef

pParameter :: Parameter -> Text
pParameter param =
  labelWith "name: " param.name
  <> labelWith "in: " param.in_
  <> maybe "" (labelWith "desc: ") param.description
  <> labelWith "required: " ((pack . show) param.required)
  <> maybe "" (labelWith "schema: " . pSchema) param.schema
  <> maybe "" (labelWith "example: " . pExample) param.example
  <> maybe "" (labelWith "examples: " . pExamples) param.examples
  <> maybe "" (labelWith "content: " . pMediaTypeMap) param.content
  <> maybe "" (labelWith "allowEmptyValue: " . (pack . show)) param.allowEmptyValue
  <> maybe "" (labelWith "style: ") param.style
  <> maybe "" (labelWith "explode: " . (pack . show)) param.explode
  <> maybe "" (labelWith "allowReserved: " . (pack . show)) param.allowReserved
  <> maybe "" (labelWith "extensions: " . (pack . show)) param.extensions

pReference :: ReferenceObject -> Text
pReference aRef =
    "$ref: " <> aRef.ref <> maybe "" (", summ: " <>) aRef.summary
        <> maybe "" (", desc: " <>) aRef.description
        <> "\n"

