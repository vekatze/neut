module Scene.Clean
  ( Context,
    clean,
  )
where

import Context.Env qualified as Env
import Context.Path qualified as Path
import Control.Monad
import Entity.Module
import Prelude hiding (log)

class
  ( Env.Context m,
    Path.Context m
  ) =>
  Context m

clean :: Context m => m ()
clean = do
  mainModule <- Env.getMainModule
  let targetDir = getTargetDir mainModule
  b <- Path.doesDirExist targetDir
  when b $ Path.removeDirRecur $ getTargetDir mainModule
