module Move.Scene.Clean (clean) where

import Control.Monad
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Unravel qualified as Unravel
import Path.IO
import Rule.Module (extractModule)
import Prelude hiding (log)

clean :: App ()
clean = do
  mainModule <- Env.getMainModule
  h <- Unravel.new
  moduleList <- toApp $ Unravel.unravelModule h (extractModule mainModule)
  forM_ moduleList $ \someModule -> do
    baseBuildDir <- toApp $ Path.getBaseBuildDir someModule
    b <- doesDirExist baseBuildDir
    when b $ do
      removeDirRecur baseBuildDir
    ensureDir baseBuildDir
