module Command.Common.Build.Install
  ( Handle,
    new,
    install,
  )
where

import Control.Monad
import Data.Text qualified as T
import Error.EIO (EIO)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Target qualified as Target
import Path
import Path.IO
import Prelude hiding (log)

newtype Handle = Handle
  { pathHandle :: Path.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  Handle {..}

install :: Handle -> Target.MainTarget -> Path Abs Dir -> EIO ()
install h targetOrZen dir = do
  execPath <- Path.getExecutableOutputPath (pathHandle h) targetOrZen
  case targetOrZen of
    Target.Named targetName _ -> do
      execName <- parseRelFile $ T.unpack targetName
      let destPath = dir </> execName
      copyFile execPath destPath
    Target.Zen {} ->
      return ()
