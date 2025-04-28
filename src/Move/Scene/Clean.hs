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
import Move.Context.Locator qualified as Locator
import Move.Context.Path qualified as Path
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Unravel qualified as Unravel
import Path.IO
import Rule.Module (extractModule)
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    unravelHandle :: Unravel.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Locator.Handle -> App Handle
new envHandle gensymHandle locatorHandle = do
  unravelHandle <- Unravel.new envHandle gensymHandle locatorHandle
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  mainModule <- Env.getMainModule (envHandle h)
  moduleList <- Unravel.unravelModule (unravelHandle h) (extractModule mainModule)
  forM_ moduleList $ \someModule -> do
    baseBuildDir <- Path.getBaseBuildDir someModule
    b <- doesDirExist baseBuildDir
    when b $ do
      removeDirRecur baseBuildDir
    ensureDir baseBuildDir
