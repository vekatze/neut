module Context.Module where

import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Data.Text as T
import Entity.Hint
import Entity.Module
import Entity.ModuleID
import Path

data Context = Context
  { getModuleFilePath :: Maybe Hint -> ModuleID -> IO (Path Abs File),
    getModule :: Hint -> ModuleID -> T.Text -> IO Module
  }

data Config = Config
  { mainModule :: Module,
    throwCtx :: Throw.Context,
    pathCtx :: Path.Context
  }
