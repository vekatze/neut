module Scene.Clean (clean) where

import Context.App
import Context.Env qualified as Env
import Context.Path qualified as Path
import Control.Monad
import Prelude hiding (log)

clean :: App ()
clean = do
  mainModule <- Env.getMainModule
  buildDir <- Path.getBaseBuildDir mainModule
  b <- Path.doesDirExist buildDir
  when b $ do
    Path.removeDirRecur buildDir
  Path.ensureDir buildDir
