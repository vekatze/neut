module Act.Dependency (get, tidy) where

import Control.Monad (forM_, unless, when)
import Crypto.Hash.SHA256 as SHA256 (hash)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Entity.Basic (Alias, Checksum (Checksum), URL (..), showChecksum)
import Entity.Global (getLibraryDirPath, note')
import Entity.Log (raiseError')
import Entity.Module
  ( Module (moduleDependency, moduleLocation),
    addDependency,
    getMainModule,
    moduleFile,
    ppModule,
  )
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import Path.IO (doesFileExist, ensureDir, resolveDir, withSystemTempFile)
import qualified Scene.Parse.Module as Module
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.Process
  ( CreateProcess (std_err),
    StdStream (CreatePipe),
    createProcess,
    proc,
    waitForProcess,
  )

get :: Alias -> URL -> IO ()
get alias url = do
  mainModule <- getMainModule
  withSystemTempFile (T.unpack alias) $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias url
    archive <- B.hGetContents tempFileHandle
    let checksum = computeChecksum archive
    extractToLibDir tempFilePath alias checksum
    addDependencyToModuleFile mainModule alias url checksum
    getLibraryModule alias checksum >>= tidy'

tidy :: IO ()
tidy =
  getMainModule >>= tidy'

tidy' :: Module -> IO ()
tidy' targetModule = do
  note' $ "context: " <> T.pack (toFilePath (moduleLocation targetModule))
  let dependency = Map.toList $ moduleDependency targetModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary alias url checksum

installIfNecessary :: Alias -> URL -> Checksum -> IO ()
installIfNecessary alias url checksum = do
  isInstalled <- checkIfInstalled checksum
  unless isInstalled $
    withSystemTempFile (T.unpack alias) $ \tempFilePath tempFileHandle -> do
      download tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let archiveChecksum = computeChecksum archive
      when (checksum /= archiveChecksum) $
        raiseError' $
          "the checksum of the module `"
            <> alias
            <> "` is different from the expected one:"
            <> "\n- "
            <> showChecksum checksum
            <> " (expected)"
            <> "\n- "
            <> showChecksum archiveChecksum
            <> " (actual)"
      extractToLibDir tempFilePath alias checksum
      getLibraryModule alias checksum >>= tidy'

checkIfInstalled :: Checksum -> IO Bool
checkIfInstalled checksum = do
  getLibraryModuleFilePath checksum >>= doesFileExist

getLibraryModuleFilePath :: Checksum -> IO (Path Abs File)
getLibraryModuleFilePath checksum = do
  moduleDir <- getModuleDir checksum
  return $ moduleDir </> moduleFile

getLibraryModule :: Alias -> Checksum -> IO Module
getLibraryModule alias checksum@(Checksum c) = do
  moduleFilePath <- getLibraryModuleFilePath checksum
  moduleFileExists <- doesFileExist moduleFilePath
  if not moduleFileExists
    then raiseError' $ "could not find the module file for `" <> alias <> "` (" <> c <> ")."
    else Module.parse moduleFilePath

getModuleDir :: Checksum -> IO (Path Abs Dir)
getModuleDir (Checksum checksum) = do
  libDir <- getLibraryDirPath
  resolveDir libDir $ T.unpack checksum

download :: Path Abs File -> Alias -> URL -> IO ()
download tempFilePath alias (URL url) = do
  let curlCmd = proc "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]
  (_, _, Just curlErrorHandler, curlHandler) <-
    createProcess curlCmd {std_err = CreatePipe}
  note' $ "downloading `" <> alias <> "` from " <> url
  curlExitCode <- waitForProcess curlHandler
  raiseIfFailure "curl" curlExitCode curlErrorHandler

computeChecksum :: B.ByteString -> Checksum
computeChecksum fileByteString =
  Checksum $ TE.decodeUtf8 $ Base64.encode $ SHA256.hash fileByteString

extractToLibDir :: Path Abs File -> Alias -> Checksum -> IO ()
extractToLibDir tempFilePath alias c@(Checksum checksum) = do
  targetDirPath <- getModuleDir c
  ensureDir targetDirPath
  let tarCmd = proc "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]
  (_, _, Just tarErrorHandler, tarHandler) <-
    createProcess tarCmd {std_err = CreatePipe}
  note' $ "extracting `" <> alias <> "` (" <> checksum <> ")"
  tarExitCode <- waitForProcess tarHandler
  raiseIfFailure "tar" tarExitCode tarErrorHandler

addDependencyToModuleFile :: Module -> Alias -> URL -> Checksum -> IO ()
addDependencyToModuleFile targetModule alias url checksum = do
  note' $ "adding the dependency of `" <> alias <> "` to the module file"
  let targetModule' = addDependency alias url checksum targetModule
  TIO.writeFile (toFilePath $ moduleLocation targetModule') $ ppModule targetModule'

raiseIfFailure :: T.Text -> ExitCode -> Handle -> IO ()
raiseIfFailure procName exitCode h =
  case exitCode of
    ExitSuccess ->
      return ()
    ExitFailure i -> do
      errStr <- TIO.hGetContents h
      raiseError' $
        "the child process `"
          <> procName
          <> "` failed with the following message (exitcode = "
          <> T.pack (show i)
          <> "):\n"
          <> errStr
