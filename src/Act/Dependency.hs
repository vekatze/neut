module Act.Dependency
  ( get,
    tidy,
    GetConfig (..),
    TidyConfig (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
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
    getLogCtx :: Log.Context,
    getPathCtx :: Path.Context
  }

data GetConfig = GetConfig
  { moduleAlias :: ModuleAlias,
    moduleURL :: ModuleURL,
    throwCfg :: Throw.Config,
    logCfg :: Log.Config,
    pathCfg :: Path.Config
  }

get :: Mode.Mode -> GetConfig -> IO ()
get mode cfg = do
  throwCtx <- Mode.throwCtx mode (throwCfg cfg)
  logCtx <- Mode.logCtx mode (logCfg cfg)
  pathCtx <- Mode.pathCtx mode (pathCfg cfg)
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    let ctx = Context {getThrowCtx = throwCtx, getLogCtx = logCtx, getPathCtx = pathCtx}
    let alias = moduleAlias cfg
    let url = moduleURL cfg
    withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
      download ctx tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let checksum = computeModuleChecksum archive
      extractToLibDir ctx tempFilePath alias checksum
      addDependencyToModuleFile logCtx mainModule alias url checksum
      getLibraryModule ctx alias checksum >>= tidy' ctx

data TidyConfig = TidyConfig
  { tidyThrowCfg :: Throw.Config,
    tidyLogCfg :: Log.Config,
    tidyPathCfg :: Path.Config
  }

tidy :: Mode.Mode -> TidyConfig -> IO ()
tidy mode cfg = do
  throwCtx <- Mode.throwCtx mode (tidyThrowCfg cfg)
  logCtx <- Mode.logCtx mode (tidyLogCfg cfg)
  pathCtx <- Mode.pathCtx mode (tidyPathCfg cfg)
  Throw.run throwCtx (Log.printLog logCtx) $ do
    mainModule <- Module.fromCurrentPath throwCtx
    let ctx = Context {getThrowCtx = throwCtx, getLogCtx = logCtx, getPathCtx = pathCtx}
    tidy' ctx mainModule

tidy' :: Context -> Module -> IO ()
tidy' ctx targetModule = do
  Log.printNote' (getLogCtx ctx) $ "context: " <> T.pack (toFilePath (moduleLocation targetModule))
  let dependency = Map.toList $ moduleDependency targetModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary ctx alias url checksum

installIfNecessary :: Context -> ModuleAlias -> ModuleURL -> ModuleChecksum -> IO ()
installIfNecessary ctx alias url checksum = do
  isInstalled <- checkIfInstalled (getPathCtx ctx) checksum
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
      getLibraryModule ctx alias checksum >>= tidy' ctx

checkIfInstalled :: Path.Context -> ModuleChecksum -> IO Bool
checkIfInstalled ctx checksum = do
  Path.getLibraryModuleFilePath ctx checksum >>= doesFileExist

getLibraryModule :: Context -> ModuleAlias -> ModuleChecksum -> IO Module
getLibraryModule ctx alias checksum@(ModuleChecksum c) = do
  moduleFilePath <- Path.getLibraryModuleFilePath (getPathCtx ctx) checksum
  moduleFileExists <- doesFileExist moduleFilePath
  if not moduleFileExists
    then
      Throw.raiseError' (getThrowCtx ctx) $
        "could not find the module file for `" <> extract alias <> "` (" <> c <> ")."
    else Module.fromFilePath (getThrowCtx ctx) moduleFilePath

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
  targetDirPath <- Path.getModuleDirPath (getPathCtx ctx) c
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
