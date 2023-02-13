module Scene.Collect where

import Context.Env qualified as Env
import Control.Monad.Catch
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Entity.Module
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Prelude hiding (log)

class
  ( Env.Context m,
    MonadThrow m
  ) =>
  Context m

collectTargetList :: Context m => Maybe Target -> m [Target]
collectTargetList mTarget = do
  flip getTargetList mTarget <$> Env.getMainModule

collectSourceList :: Context m => Maybe FilePath -> m [SGL.StrictGlobalLocator]
collectSourceList mFilePathStr = do
  mainModule <- Env.getMainModule
  case mFilePathStr of
    Just filePathStr -> do
      sgl <- SGL.reflectInMainModule filePathStr
      return [sgl]
    Nothing -> do
      return (Map.elems $ moduleTarget mainModule)
