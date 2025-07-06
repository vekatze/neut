module Command.LSP.Internal.Source.Reflect
  ( Handle,
    new,
    reflect,
  )
where

import Error.EIO (EIO)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Module
import Kernel.Common.Module.FindModuleFile qualified as Module
import Kernel.Common.Module.FromPath qualified as Module
import Kernel.Common.Source
import Path

newtype Handle = Handle
  { envHandle :: Env.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  Handle {..}

reflect :: Handle -> FilePath -> EIO (Maybe Source)
reflect h srcPath = do
  srcPath' <- parseAbsFile srcPath
  m <- getModule h srcPath'
  return $
    Just
      Source
        { sourceFilePath = srcPath',
          sourceModule = m,
          sourceHint = Nothing
        }

getModule :: Handle -> Path Abs File -> EIO Module
getModule h srcPath = do
  let srcDir = parent srcPath
  moduleFilePath <- Module.findModuleFile srcDir
  let MainModule mainModule = Env.getMainModule (envHandle h)
  if moduleLocation mainModule == moduleFilePath
    then return mainModule
    else Module.fromFilePath moduleFilePath
