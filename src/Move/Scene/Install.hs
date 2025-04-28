module Move.Scene.Install
  ( Handle,
    new,
    install,
  )
where

import Control.Monad
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
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

new :: Env.Handle -> Color.Handle -> Debug.Handle -> App Handle
new envHandle colorHandle debugHandle = do
  pathHandle <- Path.new envHandle colorHandle debugHandle
  return $ Handle {..}

install :: Handle -> Target.MainTarget -> Path Abs Dir -> EIO ()
install h targetOrZen dir = do
  mainModule <- Env.getMainModule (envHandle h)
  execPath <- Path.getExecutableOutputPath (pathHandle h) targetOrZen (extractModule mainModule)
  case targetOrZen of
    Target.Named targetName _ -> do
      execName <- parseRelFile $ T.unpack targetName
      let destPath = dir </> execName
      copyFile execPath destPath
    Target.Zen {} ->
      return ()
