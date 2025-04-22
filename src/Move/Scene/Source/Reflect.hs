module Move.Scene.Source.Reflect (reflect) where

import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env (getMainModule)
import Move.Scene.Module.Reflect qualified as Module
import Path
import Rule.Module
import Rule.Source

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
  moduleFilePath <- toApp $ Module.findModuleFile srcDir srcDir
  MainModule mainModule <- getMainModule
  if moduleLocation mainModule == moduleFilePath
    then return mainModule
    else do
      h <- Module.new
      toApp $ Module.fromFilePath h moduleFilePath
