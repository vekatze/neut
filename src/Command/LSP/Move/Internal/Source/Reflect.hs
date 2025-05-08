module Command.LSP.Move.Internal.Source.Reflect
  ( Handle,
    new,
    reflect,
  )
where

import Aux.Error.Rule.EIO (EIO)
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Module.FindModuleFile qualified as Module
import Kernel.Common.Move.Module.FromPath qualified as Module
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Module
import Kernel.Common.Rule.Source
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
