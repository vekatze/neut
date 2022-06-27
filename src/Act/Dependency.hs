module Act.Dependency (get, tidy) where

import Context.Log
import Context.Log.IO
import Control.Monad
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Entity.Global
import Entity.Log
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleURL
import Path
import Path.IO
import System.Exit
import System.IO
import System.Process

get :: ModuleAlias -> ModuleURL -> IO ()
get alias url = do
  mainModule <- getMainModule
  let logCtx = logContextIO
  withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
    download logCtx tempFilePath alias url
    archive <- B.hGetContents tempFileHandle
    let checksum = computeModuleChecksum archive
    extractToLibDir logCtx tempFilePath alias checksum
    addDependencyToModuleFile logCtx mainModule alias url checksum
    getLibraryModule alias checksum >>= tidy' logCtx

tidy :: IO ()
tidy = do
  let logCtx = logContextIO
  getMainModule >>= tidy' logCtx

tidy' :: LogContext IO -> Module -> IO ()
tidy' logCtx targetModule = do
  printNote' logCtx $ "context: " <> T.pack (toFilePath (moduleLocation targetModule))
  let dependency = Map.toList $ moduleDependency targetModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary logCtx (ModuleAlias alias) url checksum

installIfNecessary :: LogContext IO -> ModuleAlias -> ModuleURL -> ModuleChecksum -> IO ()
installIfNecessary logCtx alias url checksum = do
  isInstalled <- checkIfInstalled checksum
  unless isInstalled $
    withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
      download logCtx tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let archiveModuleChecksum = computeModuleChecksum archive
      when (checksum /= archiveModuleChecksum) $
        raiseError' $
          "the checksum of the module `"
            <> extract alias
            <> "` is different from the expected one:"
            <> "\n- "
            <> showModuleChecksum checksum
            <> " (expected)"
            <> "\n- "
            <> showModuleChecksum archiveModuleChecksum
            <> " (actual)"
      extractToLibDir logCtx tempFilePath alias checksum
      getLibraryModule alias checksum >>= tidy' logCtx

checkIfInstalled :: ModuleChecksum -> IO Bool
checkIfInstalled checksum = do
  getLibraryModuleFilePath checksum >>= doesFileExist

getLibraryModuleFilePath :: ModuleChecksum -> IO (Path Abs File)
getLibraryModuleFilePath checksum = do
  moduleDir <- getModuleDir checksum
  return $ moduleDir </> moduleFile

getLibraryModule :: ModuleAlias -> ModuleChecksum -> IO Module
getLibraryModule alias checksum@(ModuleChecksum c) = do
  moduleFilePath <- getLibraryModuleFilePath checksum
  moduleFileExists <- doesFileExist moduleFilePath
  if not moduleFileExists
    then raiseError' $ "could not find the module file for `" <> extract alias <> "` (" <> c <> ")."
    else Module.fromFilePath moduleFilePath

getModuleDir :: ModuleChecksum -> IO (Path Abs Dir)
getModuleDir (ModuleChecksum checksum) = do
  libDir <- getLibraryDirPath
  resolveDir libDir $ T.unpack checksum

download :: LogContext IO -> Path Abs File -> ModuleAlias -> ModuleURL -> IO ()
download logCtx tempFilePath alias (ModuleURL url) = do
  let curlCmd = proc "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]
  (_, _, Just curlErrorHandler, curlHandler) <-
    createProcess curlCmd {std_err = CreatePipe}
  printNote' logCtx $ "downloading `" <> extract alias <> "` from " <> url
  curlExitCode <- waitForProcess curlHandler
  raiseIfFailure "curl" curlExitCode curlErrorHandler

computeModuleChecksum :: B.ByteString -> ModuleChecksum
computeModuleChecksum fileByteString =
  ModuleChecksum $ TE.decodeUtf8 $ Base64.encode $ SHA256.hash fileByteString

extractToLibDir :: LogContext IO -> Path Abs File -> ModuleAlias -> ModuleChecksum -> IO ()
extractToLibDir logCtx tempFilePath alias c@(ModuleChecksum checksum) = do
  targetDirPath <- getModuleDir c
  ensureDir targetDirPath
  let tarCmd = proc "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]
  (_, _, Just tarErrorHandler, tarHandler) <-
    createProcess tarCmd {std_err = CreatePipe}
  printNote' logCtx $ "extracting `" <> extract alias <> "` (" <> checksum <> ")"
  tarExitCode <- waitForProcess tarHandler
  raiseIfFailure "tar" tarExitCode tarErrorHandler

addDependencyToModuleFile :: LogContext IO -> Module -> ModuleAlias -> ModuleURL -> ModuleChecksum -> IO ()
addDependencyToModuleFile logCtx targetModule alias url checksum = do
  printNote' logCtx $ "adding the dependency of `" <> extract alias <> "` to the module file"
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

showModuleChecksum :: ModuleChecksum -> T.Text
showModuleChecksum (ModuleChecksum checksum) =
  checksum
