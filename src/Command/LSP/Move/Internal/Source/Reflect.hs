module Command.LSP.Move.Internal.Source.Reflect
  ( Handle,
    new,
    reflect,
  )
where

import Error.Rule.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Module.Reflect qualified as Module
import Main.Rule.Module
import Main.Rule.Source
import Path

newtype Handle = Handle
  { envHandle :: Env.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
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
  moduleFilePath <- Module.findModuleFile srcDir srcDir
  let MainModule mainModule = Env.getMainModule (envHandle h)
  if moduleLocation mainModule == moduleFilePath
    then return mainModule
    else Module.fromFilePath moduleFilePath
