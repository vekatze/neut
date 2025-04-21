module Move.Scene.Clean (clean) where

import Move.Context.App
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Control.Monad
import Move.Scene.Unravel (unravelModule)
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
