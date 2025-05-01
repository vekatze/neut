module Move.Scene.Install
  ( Handle,
    new,
    install,
  )
where

import Control.Monad
import Data.Text qualified as T
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Init.Base qualified as Base
import Path
import Path.IO
import Rule.Module (extractModule)
import Rule.Target qualified as Target
import Prelude hiding (log)

data Handle
  = Handle
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
