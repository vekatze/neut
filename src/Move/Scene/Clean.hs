module Move.Scene.Clean
  ( Handle,
    new,
    clean,
  )
where

import Control.Monad
import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Unravel qualified as Unravel
import Path.IO
import Rule.Module (MainModule, extractModule)
import Prelude hiding (log)

data Handle = Handle
  { mainModule :: MainModule,
    unravelHandle :: Unravel.Handle
  }

new :: App Handle
new = do
  mainModule <- Env.getMainModule
  unravelHandle <- Unravel.new
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  moduleList <- Unravel.unravelModule (unravelHandle h) (extractModule (mainModule h))
  forM_ moduleList $ \someModule -> do
    baseBuildDir <- Path.getBaseBuildDir someModule
    b <- doesDirExist baseBuildDir
    when b $ do
      removeDirRecur baseBuildDir
    ensureDir baseBuildDir
