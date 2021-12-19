module Command.Get (get, tidy) where

import Control.Monad (forM_, unless, when)
import Crypto.Hash.SHA256 as SHA256 (hash)
import Data.Basic ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import Data.Global (getLibraryDirPath, moduleFileName, note')
import qualified Data.HashMap.Lazy as Map
import Data.Log (raiseError')
import Data.Module (Alias, Checksum (Checksum), showChecksum)
import Data.Spec
  ( Spec (specDependency, specLocation),
    URL (..),
    addDependency,
    ppSpec,
  )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Parse.Spec as Spec
import Path (Abs, Dir, File, Path, toFilePath)
import Path.IO (doesFileExist, ensureDir, resolveDir, resolveFile, withSystemTempFile)
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
  spec <- Spec.getMainSpec
  withSystemTempFile (T.unpack alias) $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias url
    archive <- B.hGetContents tempFileHandle
    let checksum = computeChecksum archive
    extractToLibDir tempFilePath alias checksum
    addDependencyToModuleFile spec alias url checksum
    getLibrarySpec alias checksum >>= tidy'

tidy :: IO ()
tidy =
  Spec.getMainSpec >>= tidy'

tidy' :: Spec -> IO ()
tidy' spec = do
  note' $ "context: " <> T.pack (toFilePath (specLocation spec))
  let dependency = Map.toList $ specDependency spec
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
      getLibrarySpec alias checksum >>= tidy'

checkIfInstalled :: Checksum -> IO Bool
checkIfInstalled checksum = do
  getLibraryModuleFilePath checksum >>= doesFileExist

getLibraryModuleFilePath :: Checksum -> IO (Path Abs File)
getLibraryModuleFilePath checksum = do
  moduleDir <- getModuleDir checksum
  resolveFile moduleDir moduleFileName

getLibrarySpec :: Alias -> Checksum -> IO Spec
getLibrarySpec alias checksum@(Checksum c) = do
  specFilePath <- getLibraryModuleFilePath checksum
  moduleFileExists <- doesFileExist specFilePath
  if not moduleFileExists
    then raiseError' $ "could not find the spec file for the module `" <> alias <> "` (" <> c <> ")."
    else Spec.parse specFilePath

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

addDependencyToModuleFile :: Spec -> Alias -> URL -> Checksum -> IO ()
addDependencyToModuleFile spec alias url checksum = do
  note' $ "adding the dependency of `" <> alias <> "` to the module file"
  let spec' = addDependency alias url checksum spec
  TIO.writeFile (toFilePath $ specLocation spec) $ ppSpec spec'

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
