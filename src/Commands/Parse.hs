module Commands.Parse where

import Data.Text ( Text, unpack )
import Data.Yaml ( decodeFileEither, ParseException (..) )
import qualified System.FilePath as Spsx

import qualified Options.Runtime as Rto

import Specs.OpenApi ( OpenAPI )
import Specs.PrettyPrint ( pPrint )
import Generators.Servant (writeClient)

parseCmd :: FilePath -> Text -> FilePath -> Rto.RunOptions -> IO ()
parseCmd filePath pkgName destDir rtOpts = do
  let
    fileExt = Spsx.takeExtension filePath
  rezA <- case fileExt of
    ".yaml" -> do
      eiRez <- decodeFileEither filePath :: IO (Either ParseException OpenAPI)
      case eiRez of
        Left err -> pure . Left $ "@[parseCmd] err: " <> show err
        Right aContent ->
          pure $ Right aContent
    ".json" -> do
      pure . Left $ "@[parseCmd] json not supported yet."
    _ -> pure . Left $ "@[parseCmd] unknown specification extension: " <> fileExt
  case rezA of
    Left errMsg -> putStrLn $ "@[parseComd] err: " <> errMsg
    Right aContent ->
      case rtOpts.debug of
        0 -> do
          rezA <- writeClient rtOpts destDir pkgName aContent
          pure ()
        1 -> putStrLn $ "@[parseCmd] pPrint:\n" <> unpack (pPrint aContent)
        _ -> putStrLn $ "@[parseCmd] aContent:\n" <> show aContent
  pure ()
