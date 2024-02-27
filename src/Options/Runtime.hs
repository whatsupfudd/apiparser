module Options.Runtime (defaultRun, RunOptions (..)) where
-- import Data.Int (Int)

import Data.Text (Text)


data RunOptions = RunOptions {
    debug :: Int
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
  }
