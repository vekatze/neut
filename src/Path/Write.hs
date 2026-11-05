module Path.Write
  ( writeText,
    writeLazyByteString,
    placeAtomically,
    printText,
  )
where

import Control.Exception (IOException, onException, try)
import Control.Monad (void)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Text.Encoding
import Path
import Path.IO (removeFile, renameFile, setModificationTime)
import System.IO (hClose, openBinaryTempFileWithDefaultPermissions)

writeText :: Path Abs File -> T.Text -> IO ()
writeText path content =
  B.writeFile (toFilePath path) $ encodeUtf8 content

writeLazyByteString :: Path Abs File -> L.ByteString -> IO ()
writeLazyByteString path =
  L.writeFile (toFilePath path)

placeAtomically :: Path Abs File -> UTCTime -> (Path Abs File -> IO ()) -> IO ()
placeAtomically finalPath timeStamp write = do
  stagingPath <- createStagingFile finalPath
  let place = do
        write stagingPath
        setModificationTime stagingPath timeStamp
        renameFile stagingPath finalPath
  place `onException` removeStagingFile stagingPath

createStagingFile :: Path Abs File -> IO (Path Abs File)
createStagingFile finalPath = do
  (stagingPath, stagingHandle) <-
    openBinaryTempFileWithDefaultPermissions (toFilePath $ parent finalPath) (toFilePath (filename finalPath) <> ".tmp")
  hClose stagingHandle
  parseAbsFile stagingPath

removeStagingFile :: Path Abs File -> IO ()
removeStagingFile stagingPath = do
  void $ try @IOException $ removeFile stagingPath

printText :: T.Text -> IO ()
printText content = do
  B.putStr $ encodeUtf8 content
