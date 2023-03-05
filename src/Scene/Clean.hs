module Scene.Clean (clean) where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Entity.Module
import Prelude hiding (log)

clean :: App ()
clean = do
  mainModule <- Module.getMainModule
  let targetDir = getTargetDir mainModule
  b <- Path.doesDirExist targetDir
  when b $ do
    Path.removeDirRecur targetDir
  Path.ensureDir targetDir
