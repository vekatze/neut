module Scene.Clean (clean) where

import Context.App
import Context.Env qualified as Env
import Context.Path qualified as Path
import Control.Monad
import Scene.Unravel (unravelModule)
import Prelude hiding (log)

clean :: App ()
clean = do
  mainModule <- Env.getMainModule
  moduleList <- unravelModule mainModule
  forM_ moduleList $ \someModule -> do
    baseBuildDir <- Path.getBaseBuildDir someModule
    b <- Path.doesDirExist baseBuildDir
    when b $ do
      Path.removeDirRecur baseBuildDir
    Path.ensureDir baseBuildDir
