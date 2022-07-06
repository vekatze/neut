module Act.Dependency
  ( get,
    tidy,
    GetConfig (..),
    TidyConfig (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Throw as Throw
import Control.Monad
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.HashMap.Strict as Map
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

data Context = Context
  { getThrowCtx :: Throw.Context,
    getLogCtx :: Log.Context
  }

data GetConfig = GetConfig
  { moduleAlias :: ModuleAlias,
    moduleURL :: ModuleURL,
    throwCfg :: Throw.Config,
    logCfg :: Log.Config
  }

get :: Mode.Mode -> GetConfig -> IO ()
get mode cfg = do
  throwCtx <- Mode.throwCtx mode (throwCfg cfg)
  logCtx <- Mode.logCtx mode (logCfg cfg)
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    let ctx = Context {getThrowCtx = throwCtx, getLogCtx = logCtx}
    let alias = moduleAlias cfg
    let url = moduleURL cfg
    withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
      download ctx tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let checksum = computeModuleChecksum archive
      extractToLibDir ctx tempFilePath alias checksum
      addDependencyToModuleFile logCtx mainModule alias url checksum
      getLibraryModule throwCtx alias checksum >>= tidy' ctx

data TidyConfig = TidyConfig
  { tidyThrowCfg :: Throw.Config,
    tidyLogCfg :: Log.Config
  }

tidy :: Mode.Mode -> TidyConfig -> IO ()
tidy mode cfg = do
  throwCtx <- Mode.throwCtx mode (tidyThrowCfg cfg)
  logCtx <- Mode.logCtx mode (tidyLogCfg cfg)
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    let ctx = Context {getThrowCtx = throwCtx, getLogCtx = logCtx}
    tidy' ctx mainModule

tidy' :: Context -> Module -> IO ()
tidy' ctx targetModule = do
  Log.printNote' (getLogCtx ctx) $ "context: " <> T.pack (toFilePath (moduleLocation targetModule))
  let dependency = Map.toList $ moduleDependency targetModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary ctx alias url checksum

installIfNecessary :: Context -> ModuleAlias -> ModuleURL -> ModuleChecksum -> IO ()
installIfNecessary ctx alias url checksum = do
  isInstalled <- checkIfInstalled checksum
  unless isInstalled $
    withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
      download ctx tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let archiveModuleChecksum = computeModuleChecksum archive
      when (checksum /= archiveModuleChecksum) $
        Throw.raiseError' (getThrowCtx ctx) $
          "the checksum of the module `"
            <> extract alias
            <> "` is different from the expected one:"
            <> "\n- "
            <> showModuleChecksum checksum
            <> " (expected)"
            <> "\n- "
            <> showModuleChecksum archiveModuleChecksum
            <> " (actual)"
      extractToLibDir ctx tempFilePath alias checksum
      getLibraryModule (getThrowCtx ctx) alias checksum >>= tidy' ctx

checkIfInstalled :: ModuleChecksum -> IO Bool
checkIfInstalled checksum = do
  getLibraryModuleFilePath checksum >>= doesFileExist

getLibraryModuleFilePath :: ModuleChecksum -> IO (Path Abs File)
getLibraryModuleFilePath checksum = do
  moduleDir <- getModuleDir checksum
  return $ moduleDir </> moduleFile

getLibraryModule :: Throw.Context -> ModuleAlias -> ModuleChecksum -> IO Module
getLibraryModule throwCtx alias checksum@(ModuleChecksum c) = do
  moduleFilePath <- getLibraryModuleFilePath checksum
  moduleFileExists <- doesFileExist moduleFilePath
  if not moduleFileExists
    then
      Throw.raiseError' throwCtx $
        "could not find the module file for `" <> extract alias <> "` (" <> c <> ")."
    else Module.fromFilePath throwCtx moduleFilePath

getModuleDir :: ModuleChecksum -> IO (Path Abs Dir)
getModuleDir (ModuleChecksum checksum) = do
  libDir <- getLibraryDirPath
  resolveDir libDir $ T.unpack checksum

download :: Context -> Path Abs File -> ModuleAlias -> ModuleURL -> IO ()
download ctx tempFilePath alias (ModuleURL url) = do
  let curlCmd = proc "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]
  (_, _, Just curlErrorHandler, curlHandler) <-
    createProcess curlCmd {std_err = CreatePipe}
  Log.printNote' (getLogCtx ctx) $ "downloading `" <> extract alias <> "` from " <> url
  curlExitCode <- waitForProcess curlHandler
  Throw.raiseIfProcessFailed (getThrowCtx ctx) "curl" curlExitCode curlErrorHandler

computeModuleChecksum :: B.ByteString -> ModuleChecksum
computeModuleChecksum fileByteString =
  ModuleChecksum $ TE.decodeUtf8 $ Base64.encode $ SHA256.hash fileByteString

extractToLibDir :: Context -> Path Abs File -> ModuleAlias -> ModuleChecksum -> IO ()
extractToLibDir ctx tempFilePath alias c@(ModuleChecksum checksum) = do
  targetDirPath <- getModuleDir c
  ensureDir targetDirPath
  let tarCmd = proc "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]
  (_, _, Just tarErrorHandler, tarHandler) <-
    createProcess tarCmd {std_err = CreatePipe}
  Log.printNote' (getLogCtx ctx) $ "extracting `" <> extract alias <> "` (" <> checksum <> ")"
  tarExitCode <- waitForProcess tarHandler
  Throw.raiseIfProcessFailed (getThrowCtx ctx) "tar" tarExitCode tarErrorHandler

addDependencyToModuleFile :: Log.Context -> Module -> ModuleAlias -> ModuleURL -> ModuleChecksum -> IO ()
addDependencyToModuleFile logCtx targetModule alias url checksum = do
  Log.printNote' logCtx $ "adding the dependency of `" <> extract alias <> "` to the module file"
  let targetModule' = addDependency alias url checksum targetModule
  TIO.writeFile (toFilePath $ moduleLocation targetModule') $ ppModule targetModule'

showModuleChecksum :: ModuleChecksum -> T.Text
showModuleChecksum (ModuleChecksum checksum) =
  checksum
