module Scene.Source.Reflect (reflect) where

import Context.App
import Context.Env (getMainModule)
import Entity.Module
import Entity.Source
import Path
import Scene.Module.Reflect qualified as Module

reflect :: FilePath -> App (Maybe Source)
reflect srcPath = do
  srcPath' <- parseAbsFile srcPath
  m <- getModule srcPath'
  return $
    Just
      Source
        { sourceFilePath = srcPath',
          sourceModule = m,
          sourceHint = Nothing
        }

getModule :: Path Abs File -> App Module
getModule srcPath = do
  let srcDir = parent srcPath
  moduleFilePath <- Module.findModuleFile srcDir srcDir
  mainModule <- getMainModule
  if moduleLocation mainModule == moduleFilePath
    then return mainModule
    else Module.fromFilePath moduleFilePath
