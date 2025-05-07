module Command.Common.Move.Build.Install
  ( Handle,
    new,
    install,
  )
where

import Control.Monad
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Kernel.Common.Rule.Module (extractModule)
import Kernel.Common.Rule.Target qualified as Target
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.Path qualified as Path
import Kernel.Move.Scene.Init.Global qualified as Global
import Path
import Path.IO
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
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
