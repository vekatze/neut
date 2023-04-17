module Entity.Config.Check (Config (..)) where

import Entity.Config.Log qualified as Log

data Config = Config
  { mFilePathString :: Maybe FilePath,
    shouldInsertPadding :: Bool,
    logCfg :: Log.Config
  }
