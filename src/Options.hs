module Options  (
  module Options.Cli
  , module Options.ConfFile
  , module RtOpts
  , mergeOptions
 )
where

import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT

import Options.Cli
import Options.ConfFile
import qualified Options.Runtime as RtOpts

mergeOptions :: CliOptions -> FileOptions -> EnvOptions -> RtOpts.RunOptions
mergeOptions cli file env =
  -- TODO: put proper priority filling of values for the Runtime Options.
  let
    defO = RtOpts.defaultRun
    -- Update from config file:
    fileO =
      let
        dbgO = case file.debug of
          Nothing -> defO
          Just aVal -> setDebug defO aVal
        {-
          pkgName0 = case file.packageName of
            Nothing -> dbgO
            Just aVal -> dbgO { packageName = DT.pack aVal } :: RunOptions
        -}
      in
      dbgO
    -- TODO: update from CLI options
    cliO = case cli.debug of
      Nothing -> fileO
      Just aVal -> setDebug fileO aVal
    -- TODO: update from ENV options
    envO = cliO
  in 
  envO

setDebug :: RtOpts.RunOptions -> Int -> RtOpts.RunOptions
setDebug rtOpts aVal =
  rtOpts { RtOpts.debug = aVal }