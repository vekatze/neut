module Kernel.Common.Module.FindModuleFile
  ( findModuleFile,
    getCurrentModuleFilePath,
  )
where

import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (raiseError')
import Kernel.Common.Const (moduleFile)
import Path
import Path.IO

findModuleFile :: Path Abs Dir -> EIO (Path Abs File)
findModuleFile baseDir = do
  findModuleFile' baseDir baseDir

getCurrentModuleFilePath :: EIO (Path Abs File)
getCurrentModuleFilePath = do
  baseDir <- getCurrentDir
  findModuleFile baseDir

findModuleFile' :: Path Abs Dir -> Path Abs Dir -> EIO (Path Abs File)
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
