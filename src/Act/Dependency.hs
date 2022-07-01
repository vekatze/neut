module Act.Dependency (get, tidy) where

import Context.App
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import Control.Monad
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import Data.Function
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Entity.Global
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleURL
import Path
import Path.IO
import System.IO
import System.Process
import Prelude hiding (log)

get :: Axis -> ModuleAlias -> ModuleURL -> IO ()
get axis alias url = do
  mainModule <- getMainModule (axis & throw)
  withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
    download axis tempFilePath alias url
    archive <- B.hGetContents tempFileHandle
    let checksum = computeModuleChecksum archive
    extractToLibDir axis tempFilePath alias checksum
    addDependencyToModuleFile axis mainModule alias url checksum
    getLibraryModule axis alias checksum >>= tidy' axis

tidy :: Axis -> IO ()
tidy axis = do
  getMainModule (axis & throw) >>= tidy' axis

tidy' :: Axis -> Module -> IO ()
tidy' axis targetModule = do
  (axis & log & Log.printNote') $ "context: " <> T.pack (toFilePath (moduleLocation targetModule))
  let dependency = Map.toList $ moduleDependency targetModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary axis (ModuleAlias alias) url checksum

installIfNecessary :: Axis -> ModuleAlias -> ModuleURL -> ModuleChecksum -> IO ()
installIfNecessary axis alias url checksum = do
  isInstalled <- checkIfInstalled checksum
  unless isInstalled $
    withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
      download axis tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let archiveModuleChecksum = computeModuleChecksum archive
      when (checksum /= archiveModuleChecksum) $
        axis & throw & Throw.raiseError' $
          "the checksum of the module `"
            <> extract alias
            <> "` is different from the expected one:"
            <> "\n- "
            <> showModuleChecksum checksum
            <> " (expected)"
            <> "\n- "
            <> showModuleChecksum archiveModuleChecksum
            <> " (actual)"
      extractToLibDir axis tempFilePath alias checksum
      getLibraryModule axis alias checksum >>= tidy' axis

checkIfInstalled :: ModuleChecksum -> IO Bool
checkIfInstalled checksum = do
  getLibraryModuleFilePath checksum >>= doesFileExist

getLibraryModuleFilePath :: ModuleChecksum -> IO (Path Abs File)
getLibraryModuleFilePath checksum = do
  moduleDir <- getModuleDir checksum
  return $ moduleDir </> moduleFile

getLibraryModule :: Axis -> ModuleAlias -> ModuleChecksum -> IO Module
getLibraryModule axis alias checksum@(ModuleChecksum c) = do
  moduleFilePath <- getLibraryModuleFilePath checksum
  moduleFileExists <- doesFileExist moduleFilePath
  if not moduleFileExists
    then
      axis & throw & Throw.raiseError' $
        "could not find the module file for `" <> extract alias <> "` (" <> c <> ")."
    else Module.fromFilePath (axis & throw) moduleFilePath

getModuleDir :: ModuleChecksum -> IO (Path Abs Dir)
getModuleDir (ModuleChecksum checksum) = do
  libDir <- getLibraryDirPath
  resolveDir libDir $ T.unpack checksum

download :: Axis -> Path Abs File -> ModuleAlias -> ModuleURL -> IO ()
download axis tempFilePath alias (ModuleURL url) = do
  let curlCmd = proc "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]
  (_, _, Just curlErrorHandler, curlHandler) <-
    createProcess curlCmd {std_err = CreatePipe}
  axis & log & Log.printNote' $ "downloading `" <> extract alias <> "` from " <> url
  curlExitCode <- waitForProcess curlHandler
  Throw.raiseIfProcessFailed (axis & throw) "curl" curlExitCode curlErrorHandler

computeModuleChecksum :: B.ByteString -> ModuleChecksum
computeModuleChecksum fileByteString =
  ModuleChecksum $ TE.decodeUtf8 $ Base64.encode $ SHA256.hash fileByteString

extractToLibDir :: Axis -> Path Abs File -> ModuleAlias -> ModuleChecksum -> IO ()
extractToLibDir axis tempFilePath alias c@(ModuleChecksum checksum) = do
  targetDirPath <- getModuleDir c
  ensureDir targetDirPath
  let tarCmd = proc "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]
  (_, _, Just tarErrorHandler, tarHandler) <-
    createProcess tarCmd {std_err = CreatePipe}
  axis & log & Log.printNote' $ "extracting `" <> extract alias <> "` (" <> checksum <> ")"
  tarExitCode <- waitForProcess tarHandler
  Throw.raiseIfProcessFailed (axis & throw) "tar" tarExitCode tarErrorHandler

addDependencyToModuleFile :: Axis -> Module -> ModuleAlias -> ModuleURL -> ModuleChecksum -> IO ()
addDependencyToModuleFile axis targetModule alias url checksum = do
  axis & log & Log.printNote' $ "adding the dependency of `" <> extract alias <> "` to the module file"
  let targetModule' = addDependency alias url checksum targetModule
  TIO.writeFile (toFilePath $ moduleLocation targetModule') $ ppModule targetModule'

showModuleChecksum :: ModuleChecksum -> T.Text
showModuleChecksum (ModuleChecksum checksum) =
  checksum
