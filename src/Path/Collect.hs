module Path.Collect
  ( collectFilePathList,
    hasExtension,
  )
where

import App.App (App)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Path
import Path.IO (listDirRecur, resolveDir', resolveFile')
import System.Directory qualified as Dir

collectFilePathList :: (Path Abs File -> Bool) -> [FilePath] -> App [Path Abs File]
collectFilePathList isTarget pathStringList =
  concat <$> forM pathStringList (resolveTarget isTarget)

resolveTarget :: (Path Abs File -> Bool) -> FilePath -> App [Path Abs File]
resolveTarget isTarget pathString = do
  isDir <- liftIO $ Dir.doesDirectoryExist pathString
  if isDir
    then do
      dirPath <- resolveDir' pathString
      (_, filePathList) <- listDirRecur dirPath
      return $ filter isTarget filePathList
    else do
      filePath <- resolveFile' pathString
      return [filePath]

hasExtension :: String -> Path a File -> Bool
hasExtension ext path =
  case splitExtension path of
    Just (_, ext')
      | ext' == ext ->
          True
    _ ->
      False
