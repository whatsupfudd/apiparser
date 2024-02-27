module Generators.Servant where

import qualified Options.Runtime as RtOpts
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as Tio (writeFile)

import Servant.API

import Specs.OpenApi (OpenAPI)
import Generators.Base (ClientSpec (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data API = API

data StackConfig = StackConfig {
  resolver :: Text
  , useSystemGHC :: Bool
  , extraDeps :: [Text]
  }
  -- contains 2 packages: <client api> and <client api>-servant

defaultStackConfig :: StackConfig
defaultStackConfig = StackConfig {
  resolver = "\n  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/21/22.yaml"
  , useSystemGHC = True
  , extraDeps = []
  }

genStackFile :: RtOpts.RunOptions -> Text -> StackConfig -> Text
genStackFile opts packageName stackConfig = 
  "resolver: " <> stackConfig.resolver <> "\n"
  <> "packages:\n" <> "  - " <> packageName <> "\n" <> "  - " <> packageName <> "-servant\n"
  <> "extra-deps: " <> (if null stackConfig.extraDeps then "" else (pack . show) stackConfig.extraDeps) <> "\n"
  <> "system-ghc: " <> (if stackConfig.useSystemGHC then "true" else "false") <> "\n"

data PackageSpec = PackageSpec {
    name :: Text
    , version :: Text
    , author :: Text
    , synopsis :: Text
    , category :: Text
    , description :: Text
    , dependencies :: [Text]
    , ghcOptions :: [Text]
    , defaultExtensions :: [Text]
  }

defaultPackageSpec :: Text -> PackageSpec
defaultPackageSpec aName = PackageSpec {
  name = aName
  , version = "0.1.0.0"
  , author = "author"
  , synopsis = aName <> " client."
  , category = "Web"
  , description = "Client for " <> aName <> " auto-generated from OpenAPI specification."
  , dependencies = [
      "base >= 4.7 && < 5", "aeson", "casing", "text", "servant"
      , "servant-auth-client", "servant-client", "servant-multipart-client"
      , "http-client", "bytestring", "cpphs", "http-types"
    ]
  , ghcOptions = [
      "-Wall", "-fwarn-tabs", "-fwarn-incomplete-uni-patterns"
      , "-fwarn-incomplete-record-updates"
    ]
  , defaultExtensions = [
      "OverloadedStrings", "DataKinds", "TypeOperators", "TypeFamilies"
      , "GADTs", "FlexibleInstances", "FlexibleContexts", "MultiParamTypeClasses"
      , "StrictData", "ScopedTypeVariables", "DeriveGeneric", "DeriveFunctor"
    ]
  }

genPackageFile :: RtOpts.RunOptions -> PackageSpec -> Text
genPackageFile opts packageSpec =
  "name: " <> packageSpec.name <> "\n"
  <> "version: " <> packageSpec.version <> "\n"
  <> "license: BSD3\n"
  <> "author: " <> packageSpec.author <> "\n"
  <> "maintainer: " <> packageSpec.author <> "\n"
  <> "synopsis: " <> packageSpec.synopsis <> "\n"
  <> "category: " <> packageSpec.category <> "\n\n"
  <> "description: " <> packageSpec.description <> "\n"
  <> "dependencies:\n" <> genListItems packageSpec.dependencies <> "\n"
  <> "ghc-options:\n" <> genListItems packageSpec.ghcOptions <> "\n"
  <> "default-extensions:\n" <> genListItems packageSpec.defaultExtensions <> "\n"
  

genListItems :: [Text] -> Text
genListItems items =
  foldl (\acc item -> acc <> "  - " <> item <> "\n") "" items


genLicenseFile :: RtOpts.RunOptions -> Text -> Text
genLicenseFile opts author = "Copyright " <> author <> " (c) 2021-2022\n\
\\n\
\All rights reserved.\n\
\\n\
\Redistribution and use in source and binary forms, with or without\n\
\modification, are permitted provided that the following conditions are met:\n\
\\n\
\    * Redistributions of source code must retain the above copyright\n\
\      notice, this list of conditions and the following disclaimer.\n\
\\n\
\    * Redistributions in binary form must reproduce the above\n\
\      copyright notice, this list of conditions and the following\n\
\      disclaimer in the documentation and/or other materials provided\n\
\      with the distribution.\n\
\\n\
\    * Neither the name of Author name here nor the names of other\n\
\      contributors may be used to endorse or promote products derived\n\
\      from this software without specific prior written permission.\n\
\\n\
\THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n\
\\"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n\
\LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n\
\A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n\
\OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\n\
\SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT\n\
\LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,\n\
\DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\n\
\THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n\
\(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\n\
\OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"

data ServantSpec = ServantSpec {
  api :: API
  , clientFunctions :: [Text]
  , dataTypes :: [Text]
  }

data ModuleIntro = ModuleIntro {

  }

data ProjectSpec = ProjectSpec {
  stackConfig :: StackConfig
  , clientPkg :: PackageSpec
  , servantPkg :: PackageSpec
  , servantSpec :: ServantSpec
  }

writeClient :: RtOpts.RunOptions -> FilePath -> Text -> OpenAPI -> IO (Either String ())
writeClient opts destDir pkgName oapiSpec = do
  putStrLn "@[writeClient] starting."
  let
    eiClientSpec = openApiToBase oapiSpec
  case eiClientSpec of
    Left err -> do
      putStrLn $ "@[writeClient] error: " <> err
      pure $ Left err
    Right clientSpec -> do
      rezA <- writeProjectStructure opts destDir pkgName
      case rezA of
        Left errMsg -> do
          pure $ Left errMsg
        Right projStruct ->
          let
            eiServantDef = baseToServant opts clientSpec
          in
          case eiServantDef of
            Left err -> do
              pure $ Left err
            Right servantDef -> do
              rezB <- writeServantDef opts servantDef
              case rezB of
                Left errMsg -> do
                  pure $ Left errMsg
                Right _ -> do
                  pure $ Right ()
      pure $ Right ()


writeProjectStructure :: RtOpts.RunOptions -> FilePath -> Text -> IO (Either String ())
writeProjectStructure opts destDir pkgName =
  let
    stackConfig = defaultStackConfig
    projectSpec = ProjectSpec {
      stackConfig = stackConfig
      , clientPkg = defaultPackageSpec pkgName
      , servantPkg = defaultPackageSpec (pkgName <> "-servant")
      , servantSpec = ServantSpec { api = API, clientFunctions = [], dataTypes = [] }
      }
    clientDir = destDir </> unpack pkgName
    servantDir = clientDir <> "-servant"
    packageDir = "src" </> (unpack pkgName)
  in do
  createDirectoryIfMissing True destDir
  Tio.writeFile (destDir </> "stack.yaml") $ genStackFile opts pkgName stackConfig
  -- Client package:
  createDirectoryIfMissing False clientDir
  Tio.writeFile (clientDir </> "LICENSE") $ genLicenseFile opts projectSpec.clientPkg.author
  createDirectoryIfMissing True (clientDir </> packageDir </> "Client/Internal")
  -- Files Client.hs, Client/Internal/Helper.hs.
  createDirectoryIfMissing False (clientDir </> "test")
  Tio.writeFile (clientDir </> "package.yaml") $ genPackageFile opts projectSpec.clientPkg
  Tio.writeFile (clientDir </> "Setup.hs") $ "import Distribution.Simple\nmain = defaultMain"
  -- Servant package:
  createDirectoryIfMissing False servantDir
  Tio.writeFile (servantDir </> "LICENSE") $ genLicenseFile opts projectSpec.clientPkg.author
  Tio.writeFile (servantDir </> "Setup.hs") $ "import Distribution.Simple\nmain = defaultMain"
  createDirectoryIfMissing True (servantDir </> packageDir </> "Internal")
  -- Files Api.hs, Resources.hs, Internal/Aeson.hs.
  Tio.writeFile (servantDir </> "package.yaml") $ genPackageFile opts projectSpec.servantPkg
  pure $ Right ()


baseToServant :: RtOpts.RunOptions -> ClientSpec -> Either String ServantSpec
baseToServant opts cSpec =
  Right $ ServantSpec { api = API, clientFunctions = [], dataTypes = [] }

writeServantDef :: RtOpts.RunOptions -> ServantSpec -> IO (Either String ())
writeServantDef opts content = do
  putStrLn "@[writeServantDef] starting."
  pure $ Right ()

writeModuleIntro :: RtOpts.RunOptions -> ModuleIntro -> FilePath -> IO (Either String ())
writeModuleIntro opts header outFile = do
  pure $ Right ()

openApiToBase :: OpenAPI -> Either String ClientSpec
openApiToBase api =
  Right $ ClientSpec { routes = [] }
