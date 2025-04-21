module Move.Scene.Source.Reflect (reflect) where

import Move.Context.App
import Move.Context.Env (getMainModule)
import Rule.Module
import Rule.Source
import Path
import Move.Scene.Module.Reflect qualified as Module

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
