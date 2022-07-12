module Context.Module where

import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Entity.Hint
import Entity.Module
import Entity.ModuleID
import Path

newtype Context = Context
  { getModuleFilePath :: Maybe Hint -> ModuleID -> IO (Path Abs File)
  }

data Config = Config
  { mainModule :: Module,
    throwCtx :: Throw.Context,
    pathCtx :: Path.Context
  }
