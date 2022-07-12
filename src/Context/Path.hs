module Context.Path where

import Path

newtype Context = Context
  { getLibraryDirPath :: IO (Path Abs Dir)
  -- getModuleFilePath :: Maybe Hint -> ModuleID -> IO (Path Abs File)
  }

data Config = Config
  {
  }

-- currentModule :: Module,
-- throwCtx :: Throw.Context
