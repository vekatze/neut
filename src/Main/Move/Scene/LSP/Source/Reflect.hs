module Main.Move.Scene.LSP.Source.Reflect
  ( Handle,
    new,
    reflect,
  )
where

import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Module.Reflect qualified as Module
import Main.Rule.Module
import Main.Rule.Source
import Path

data Handle
  = Handle
  { envHandle :: Env.Handle,
    moduleReflectHandle :: Module.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  let moduleReflectHandle = Module.new gensymHandle
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
    else Module.fromFilePath (moduleReflectHandle h) moduleFilePath
