module Context.Module where

import qualified Context.Path as Path
import qualified Context.Throw as Throw
import qualified Data.Text as T
import Entity.Hint
import Entity.Module
import Entity.ModuleID
import qualified Entity.StrictGlobalLocator as SGL
import Path

data Context = Context
  { getModuleFilePath :: Maybe Hint -> ModuleID -> IO (Path Abs File),
    getModule :: Hint -> ModuleID -> T.Text -> IO Module,
    getSourcePath :: SGL.StrictGlobalLocator -> IO (Path Abs File)
  }

data Config = Config
  { mainModule :: Module,
    throwCtx :: Throw.Context,
    pathCtx :: Path.Context
  }

-- isMainFile :: Context -> Source -> IO Bool
-- isMainFile ctx source = do
--   sourcePathList <- mapM (getSourcePath ctx) $ Map.elems $ moduleTarget (sourceModule source)
--   return $ elem (sourceFilePath source) sourcePathList
