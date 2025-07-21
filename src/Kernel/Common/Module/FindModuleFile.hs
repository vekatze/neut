module Kernel.Common.Module.FindModuleFile
  ( findModuleFile,
    getCurrentModuleFilePath,
  )
where

import App.App (App)
import App.Run (raiseError')
import Data.Text qualified as T
import Kernel.Common.Const (moduleFile)
import Path
import Path.IO

findModuleFile :: Path Abs Dir -> App (Path Abs File)
findModuleFile baseDir = do
  findModuleFile' baseDir baseDir

getCurrentModuleFilePath :: App (Path Abs File)
getCurrentModuleFilePath = do
  baseDir <- getCurrentDir
  findModuleFile baseDir

findModuleFile' :: Path Abs Dir -> Path Abs Dir -> App (Path Abs File)
findModuleFile' baseDir moduleRootDirCandidate = do
  let moduleFileCandidate = moduleRootDirCandidate </> moduleFile
  moduleFileExists <- doesFileExist moduleFileCandidate
  case (moduleFileExists, moduleRootDirCandidate /= parent moduleRootDirCandidate) of
    (True, _) ->
      return moduleFileCandidate
    (_, True) ->
      findModuleFile' baseDir $ parent moduleRootDirCandidate
    _ ->
      raiseError' $ "Could not find a module file (Context: " <> T.pack (toFilePath baseDir) <> ")"
