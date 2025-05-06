module Command.Common.Move.Clean
  ( Handle,
    new,
    clean,
  )
where

import Control.Monad
import Error.Rule.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.Path qualified as Path
import Main.Move.Context.Platform qualified as Platform
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Unravel qualified as Unravel
import Main.Rule.Module (extractModule)
import Path.IO
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    platformHandle :: Platform.Handle,
    unravelHandle :: Unravel.Handle
  }

new :: Base.Handle -> IO Handle
new baseHandle = do
  let envHandle = Base.envHandle baseHandle
  let pathHandle = Base.pathHandle baseHandle
  let platformHandle = Base.platformHandle baseHandle
  unravelHandle <- Unravel.new baseHandle
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  let mainModule = Env.getMainModule (envHandle h)
  moduleList <- Unravel.unravelModule (unravelHandle h) (extractModule mainModule)
  forM_ moduleList $ \someModule -> do
    baseBuildDir <- Platform.getBaseBuildDir (platformHandle h) someModule
    b <- doesDirExist baseBuildDir
    when b $ do
      removeDirRecur baseBuildDir
    ensureDir baseBuildDir
