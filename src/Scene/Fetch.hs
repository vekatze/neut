module Scene.Fetch
  ( fetch,
    insertDependency,
    Context (..),
  )
where

import qualified Context.Log as Log
import qualified Context.Module as Module
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Entity.BaseName as BN
import qualified Entity.Module as M
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
  { throwCtx :: Throw.Context,
    logCtx :: Log.Context,
    moduleCtx :: Module.Context
  }

fetch :: Context -> M.Module -> IO ()
fetch ctx baseModule = do
  Log.printNote' (logCtx ctx) $ "context: " <> T.pack (toFilePath (M.moduleLocation baseModule))
  let dependency = Map.toList $ M.moduleDependency baseModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary ctx alias url checksum

insertDependency :: Context -> M.Module -> ModuleAlias -> ModuleURL -> IO ()
insertDependency ctx mainModule alias url = do
  withSystemTempFile (T.unpack $ BN.reify (extract alias)) $ \tempFilePath tempFileHandle -> do
    download ctx tempFilePath alias url
    archive <- B.hGetContents tempFileHandle
    let checksum = MC.fromByteString archive
    extractToLibDir ctx tempFilePath alias checksum
    addDependencyToModuleFile ctx mainModule alias url checksum
    getLibraryModule ctx alias checksum >>= fetch ctx

installIfNecessary :: Context -> ModuleAlias -> ModuleURL -> MC.ModuleChecksum -> IO ()
installIfNecessary ctx alias url checksum = do
  isInstalled <- checkIfInstalled (moduleCtx ctx) checksum
  unless isInstalled $
    withSystemTempFile (T.unpack $ BN.reify (extract alias)) $ \tempFilePath tempFileHandle -> do
      download ctx tempFilePath alias url
      archive <- B.hGetContents tempFileHandle
      let archiveModuleChecksum = MC.fromByteString archive
      when (checksum /= archiveModuleChecksum) $
        Throw.raiseError' (throwCtx ctx) $
          "the checksum of the module `"
            <> BN.reify (extract alias)
            <> "` is different from the expected one:"
            <> "\n- "
            <> MC.reify checksum
            <> " (expected)"
            <> "\n- "
            <> MC.reify archiveModuleChecksum
            <> " (actual)"
      extractToLibDir ctx tempFilePath alias checksum
      getLibraryModule ctx alias checksum >>= fetch ctx

checkIfInstalled :: Module.Context -> MC.ModuleChecksum -> IO Bool
checkIfInstalled ctx checksum = do
  Module.getModuleFilePath ctx Nothing (MID.Library checksum) >>= doesFileExist

getLibraryModule :: Context -> ModuleAlias -> MC.ModuleChecksum -> IO M.Module
getLibraryModule ctx alias checksum = do
  moduleFilePath <- Module.getModuleFilePath (moduleCtx ctx) Nothing (MID.Library checksum)
  moduleFileExists <- doesFileExist moduleFilePath
  if not moduleFileExists
    then
      Throw.raiseError' (throwCtx ctx) $
        "could not find the module file for `"
          <> BN.reify (extract alias)
          <> "` ("
          <> MC.reify checksum
          <> ")."
    else Module.fromFilePath (throwCtx ctx) (MID.Library checksum) moduleFilePath

download :: Context -> Path Abs File -> ModuleAlias -> ModuleURL -> IO ()
download ctx tempFilePath alias (ModuleURL url) = do
  let curlCmd = proc "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]
  (_, _, Just curlErrorHandler, curlHandler) <-
    createProcess curlCmd {std_err = CreatePipe}
  Log.printNote' (logCtx ctx) $ "downloading `" <> BN.reify (extract alias) <> "` from " <> url
  curlExitCode <- waitForProcess curlHandler
  Throw.raiseIfProcessFailed (throwCtx ctx) "curl" curlExitCode curlErrorHandler

extractToLibDir :: Context -> Path Abs File -> ModuleAlias -> MC.ModuleChecksum -> IO ()
extractToLibDir ctx tempFilePath alias checksum = do
  targetDirPath <- parent <$> Module.getModuleFilePath (moduleCtx ctx) Nothing (MID.Library checksum)
  ensureDir targetDirPath
  let tarCmd = proc "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]
  (_, _, Just tarErrorHandler, tarHandler) <-
    createProcess tarCmd {std_err = CreatePipe}
  Log.printNote' (logCtx ctx) $
    "extracting `"
      <> BN.reify (extract alias)
      <> "` ("
      <> MC.reify checksum
      <> ")"
  tarExitCode <- waitForProcess tarHandler
  Throw.raiseIfProcessFailed (throwCtx ctx) "tar" tarExitCode tarErrorHandler

addDependencyToModuleFile :: Context -> M.Module -> ModuleAlias -> ModuleURL -> MC.ModuleChecksum -> IO ()
addDependencyToModuleFile ctx targetModule alias url checksum = do
  Log.printNote' (logCtx ctx) $
    "adding the dependency of `"
      <> BN.reify (extract alias)
      <> "` to the module file"
  let targetModule' = M.addDependency alias url checksum targetModule
  TIO.writeFile (toFilePath $ M.moduleLocation targetModule') $ M.ppModule targetModule'
