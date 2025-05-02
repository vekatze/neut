module Move.Scene.Clean
  ( Handle,
    new,
    clean,
  )
where

import Control.Monad
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Unravel qualified as Unravel
import Path.IO
import Rule.Module (extractModule)
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    unravelHandle :: Unravel.Handle
  }

new :: Base.Handle -> IO Handle
new baseHandle = do
  let envHandle = Base.envHandle baseHandle
  let pathHandle = Base.pathHandle baseHandle
  unravelHandle <- Unravel.new baseHandle
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  let mainModule = Env.getMainModule (envHandle h)
  moduleList <- Unravel.unravelModule (unravelHandle h) (extractModule mainModule)
  forM_ moduleList $ \someModule -> do
    baseBuildDir <- Env.getBaseBuildDir (envHandle h) someModule
    b <- doesDirExist baseBuildDir
    when b $ do
      removeDirRecur baseBuildDir
    ensureDir baseBuildDir
