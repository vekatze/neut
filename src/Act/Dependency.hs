module Act.Dependency
  ( get,
    tidy,
    GetConfig (..),
    TidyConfig (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Mode as Mode
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.ModuleAlias
import qualified Entity.ModuleChecksum as MC
import qualified Entity.ModuleID as MID
import Entity.ModuleURL
import Path
import Path.IO
import System.IO
import System.Process
import Prelude hiding (log)

data Context = Context
  { getThrowCtx :: Throw.Context,
    getLogCtx :: Log.Context,
    getModuleCtx :: Module.Context
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
    moduleCtx <-
      Mode.moduleCtx mode $
        Module.Config
          { Module.mainModule = mainModule,
            Module.throwCtx = throwCtx,
            Module.pathCtx = pathCtx
          }
    let ctx = Context {getThrowCtx = throwCtx, getLogCtx = logCtx, getModuleCtx = moduleCtx}
    let alias = moduleAlias cfg
    let url = moduleURL cfg
    withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
      download ctx tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let checksum = MC.fromByteString archive
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
    moduleCtx <-
      Mode.moduleCtx mode $
        Module.Config
          { Module.mainModule = mainModule,
            Module.throwCtx = throwCtx,
            Module.pathCtx = pathCtx
          }
    let ctx = Context {getThrowCtx = throwCtx, getLogCtx = logCtx, getModuleCtx = moduleCtx}
    tidy' ctx mainModule

tidy' :: Context -> Module -> IO ()
tidy' ctx targetModule = do
  Log.printNote' (getLogCtx ctx) $ "context: " <> T.pack (toFilePath (moduleLocation targetModule))
  let dependency = Map.toList $ moduleDependency targetModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary ctx alias url checksum

installIfNecessary :: Context -> ModuleAlias -> ModuleURL -> MC.ModuleChecksum -> IO ()
installIfNecessary ctx alias url checksum = do
  isInstalled <- checkIfInstalled (getModuleCtx ctx) checksum
  unless isInstalled $
    withSystemTempFile (T.unpack $ extract alias) $ \tempFilePath tempFileHandle -> do
      download ctx tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let archiveModuleChecksum = MC.fromByteString archive
      when (checksum /= archiveModuleChecksum) $
        Throw.raiseError' (getThrowCtx ctx) $
          "the checksum of the module `"
            <> extract alias
            <> "` is different from the expected one:"
            <> "\n- "
            <> MC.reify checksum
            <> " (expected)"
            <> "\n- "
            <> MC.reify archiveModuleChecksum
            <> " (actual)"
      extractToLibDir ctx tempFilePath alias checksum
      getLibraryModule ctx alias checksum >>= tidy' ctx

checkIfInstalled :: Module.Context -> MC.ModuleChecksum -> IO Bool
checkIfInstalled ctx checksum = do
  Module.getModuleFilePath ctx Nothing (MID.Library checksum) >>= doesFileExist

getLibraryModule :: Context -> ModuleAlias -> MC.ModuleChecksum -> IO Module
getLibraryModule ctx alias checksum = do
  moduleFilePath <- Module.getModuleFilePath (getModuleCtx ctx) Nothing (MID.Library checksum)
  moduleFileExists <- doesFileExist moduleFilePath
  if not moduleFileExists
    then
      Throw.raiseError' (getThrowCtx ctx) $
        "could not find the module file for `" <> extract alias <> "` (" <> MC.reify checksum <> ")."
    else Module.fromFilePath (getThrowCtx ctx) (MID.Library checksum) moduleFilePath

download :: Context -> Path Abs File -> ModuleAlias -> ModuleURL -> IO ()
download ctx tempFilePath alias (ModuleURL url) = do
  let curlCmd = proc "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]
  (_, _, Just curlErrorHandler, curlHandler) <-
    createProcess curlCmd {std_err = CreatePipe}
  Log.printNote' (getLogCtx ctx) $ "downloading `" <> extract alias <> "` from " <> url
  curlExitCode <- waitForProcess curlHandler
  Throw.raiseIfProcessFailed (getThrowCtx ctx) "curl" curlExitCode curlErrorHandler

extractToLibDir :: Context -> Path Abs File -> ModuleAlias -> MC.ModuleChecksum -> IO ()
extractToLibDir ctx tempFilePath alias checksum = do
  targetDirPath <- parent <$> Module.getModuleFilePath (getModuleCtx ctx) Nothing (MID.Library checksum)
  ensureDir targetDirPath
  let tarCmd = proc "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]
  (_, _, Just tarErrorHandler, tarHandler) <-
    createProcess tarCmd {std_err = CreatePipe}
  Log.printNote' (getLogCtx ctx) $
    "extracting `"
      <> extract alias
      <> "` ("
      <> MC.reify checksum
      <> ")"
  tarExitCode <- waitForProcess tarHandler
  Throw.raiseIfProcessFailed (getThrowCtx ctx) "tar" tarExitCode tarErrorHandler

addDependencyToModuleFile :: Log.Context -> Module -> ModuleAlias -> ModuleURL -> MC.ModuleChecksum -> IO ()
addDependencyToModuleFile logCtx targetModule alias url checksum = do
  Log.printNote' logCtx $ "adding the dependency of `" <> extract alias <> "` to the module file"
  let targetModule' = addDependency alias url checksum targetModule
  TIO.writeFile (toFilePath $ moduleLocation targetModule') $ ppModule targetModule'
