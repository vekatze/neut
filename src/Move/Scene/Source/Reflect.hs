module Move.Scene.Source.Reflect
  ( Handle,
    new,
    reflect,
  )
where

import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Scene.Module.Reflect qualified as Module
import Path
import Rule.Module
import Rule.Source

data Handle
  = Handle
  { envHandle :: Env.Handle,
    moduleHandle :: Module.Handle
  }

new :: Env.Handle -> Module.Handle -> App Handle
new envHandle moduleHandle = do
  return $ Handle {..}

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
  MainModule mainModule <- Env.getMainModule (envHandle h)
  if moduleLocation mainModule == moduleFilePath
    then return mainModule
    else Module.fromFilePath (moduleHandle h) moduleFilePath
