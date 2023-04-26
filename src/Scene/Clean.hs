module Scene.Clean (clean) where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Prelude hiding (log)

clean :: App ()
clean = do
  mainModule <- Module.getMainModule
  buildDir <- Path.getBuildDir mainModule
  b <- Path.doesDirExist buildDir
  when b $ do
    Path.removeDirRecur buildDir
  Path.ensureDir buildDir
