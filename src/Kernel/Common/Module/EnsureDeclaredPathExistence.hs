module Kernel.Common.Module.EnsureDeclaredPathExistence (ensureDeclaredPathExistence) where

import App.App (App)
import App.Run (raiseError)
import Control.Monad (unless)
import Data.Text qualified as T
import Kernel.Common.Module (SomePath)
import Logger.Hint (Hint)
import Path
import Path.IO (doesDirExist, doesFileExist)

ensureDeclaredPathExistence :: Path Abs Dir -> (Hint, SomePath Rel) -> App ()
ensureDeclaredPathExistence moduleRootDir (m, path) =
  case path of
    Left dirPath -> do
      exists <- doesDirExist (moduleRootDir </> dirPath)
      unless exists $ raiseError m $ "No such directory exists: " <> T.pack (toFilePath dirPath)
    Right filePath -> do
      exists <- doesFileExist (moduleRootDir </> filePath)
      unless exists $ raiseError m $ "No such file exists: " <> T.pack (toFilePath filePath)
