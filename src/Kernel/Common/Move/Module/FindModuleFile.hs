module Kernel.Common.Move.Module.FindModuleFile
  ( findModuleFile,
    getCurrentModuleFilePath,
  )
where

import Data.Text qualified as T
import Kernel.Common.Rule.Const (moduleFile)
import Language.Common.Move.Raise (raiseError')
import Library.Error.Rule.EIO (EIO)
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
