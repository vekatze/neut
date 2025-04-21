module Move.Scene.Source.Reflect (reflect) where

import Control.Monad.Reader (asks)
import Move.Context.App
import Move.Context.App.Internal qualified as App
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
  moduleFilePath <- Module.findModuleFile srcDir srcDir
  mainModule <- getMainModule
  if moduleLocation mainModule == moduleFilePath
    then return mainModule
    else do
      counter <- asks App.counter
      let h = Module.Handle {counter}
      toApp $ Module.fromFilePath h moduleFilePath
