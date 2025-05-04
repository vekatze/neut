module Main.Move.Scene.Install
  ( Handle,
    new,
    install,
  )
where

import Control.Monad
import Data.Text qualified as T
import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.Path qualified as Path
import Main.Move.Scene.Init.Base qualified as Base
import Main.Rule.Module (extractModule)
import Main.Rule.Target qualified as Target
import Path
import Path.IO
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  Handle {..}

install :: Handle -> Target.MainTarget -> Path Abs Dir -> EIO ()
install h targetOrZen dir = do
  let mainModule = Env.getMainModule (envHandle h)
  execPath <- Path.getExecutableOutputPath (pathHandle h) targetOrZen (extractModule mainModule)
  case targetOrZen of
    Target.Named targetName _ -> do
      execName <- parseRelFile $ T.unpack targetName
      let destPath = dir </> execName
      copyFile execPath destPath
    Target.Zen {} ->
      return ()
