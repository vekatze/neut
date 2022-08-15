module Scene.Fetch
  ( fetch,
    insertDependency,
    Context (..),
  )
where

import qualified Context.Env as Env
import qualified Context.External as External
import qualified Context.Log as Log
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import qualified Entity.Module as M
import qualified Entity.Module.Reflect as Module
import Entity.ModuleAlias
import qualified Entity.ModuleChecksum as MC
import qualified Entity.ModuleID as MID
import Entity.ModuleURL
import Path
import qualified Scene.Parse.Core as Parse
import System.IO
import Prelude hiding (log)

class
  ( Throw.Context m,
    Path.Context m,
    Log.Context m,
    Module.Context m,
    Env.Context m,
    External.Context m,
    Parse.Context m
  ) =>
  Context m
  where
  writeModule :: M.Module -> m ()
  getHandleContents :: Handle -> m B.ByteString
  withTempFile :: (Path Abs File -> Handle -> m a) -> m a

fetch :: Context m => M.Module -> m ()
fetch baseModule = do
  Log.printNote' $ "context: " <> T.pack (toFilePath (M.moduleLocation baseModule))
  let dependency = Map.toList $ M.moduleDependency baseModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary alias url checksum

insertDependency :: Context m => ModuleAlias -> ModuleURL -> m ()
insertDependency alias url = do
  mainModule <- Env.getMainModule
  -- withSystemTempFile (T.unpack $ BN.reify (extract alias)) $ \tempFilePath tempFileHandle -> do
  withTempFile $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias url
    archive <- getHandleContents tempFileHandle
    let checksum = MC.fromByteString archive
    extractToLibDir tempFilePath alias checksum
    addDependencyToModuleFile mainModule alias url checksum
    getLibraryModule alias checksum >>= fetch

installIfNecessary :: Context m => ModuleAlias -> ModuleURL -> MC.ModuleChecksum -> m ()
installIfNecessary alias url checksum = do
  isInstalled <- checkIfInstalled checksum
  unless isInstalled $
    withTempFile $ \tempFilePath tempFileHandle -> do
      download tempFilePath alias url
      archive <- getHandleContents tempFileHandle
      -- archive <- B.hGetContents tempFileHandle
      let archiveModuleChecksum = MC.fromByteString archive
      when (checksum /= archiveModuleChecksum) $
        Throw.raiseError' $
          "the checksum of the module `"
            <> BN.reify (extract alias)
            <> "` is different from the expected one:"
            <> "\n- "
            <> MC.reify checksum
            <> " (expected)"
            <> "\n- "
            <> MC.reify archiveModuleChecksum
            <> " (actual)"
      extractToLibDir tempFilePath alias checksum
      getLibraryModule alias checksum >>= fetch

checkIfInstalled :: Context m => MC.ModuleChecksum -> m Bool
checkIfInstalled checksum = do
  Module.getModuleFilePath Nothing (MID.Library checksum) >>= Path.doesFileExist

getLibraryModule :: Context m => ModuleAlias -> MC.ModuleChecksum -> m M.Module
getLibraryModule alias checksum = do
  moduleFilePath <- Module.getModuleFilePath Nothing (MID.Library checksum)
  moduleFileExists <- Path.doesFileExist moduleFilePath
  if moduleFileExists
    then Module.fromFilePath (MID.Library checksum) moduleFilePath
    else
      Throw.raiseError' $
        "could not find the module file for `"
          <> BN.reify (extract alias)
          <> "` ("
          <> MC.reify checksum
          <> ")."

download :: Context m => Path Abs File -> ModuleAlias -> ModuleURL -> m ()
download tempFilePath alias (ModuleURL url) = do
  -- let curlCmd = proc "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]
  -- (_, _, Just curlErrorHandler, curlHandler) <-
  --   createProcess curlCmd {std_err = CreatePipe}
  Log.printNote' $ "downloading `" <> BN.reify (extract alias) <> "` from " <> url
  External.run "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]

-- curlExitCode <- waitForProcess curlHandler
-- raiseIfProcessFailed "curl" curlExitCode curlErrorHandler

extractToLibDir :: Context m => Path Abs File -> ModuleAlias -> MC.ModuleChecksum -> m ()
extractToLibDir tempFilePath alias checksum = do
  targetDirPath <- parent <$> Module.getModuleFilePath Nothing (MID.Library checksum)
  Path.ensureDir targetDirPath
  -- let tarCmd = proc "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]
  -- (_, _, Just tarErrorHandler, tarHandler) <-
  --   createProcess tarCmd {std_err = CreatePipe}
  Log.printNote' $
    "extracting `"
      <> BN.reify (extract alias)
      <> "` ("
      <> MC.reify checksum
      <> ")"
  External.run "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]

-- tarExitCode <- waitForProcess tarHandler
-- raiseIfProcessFailed "tar" tarExitCode tarErrorHandler

addDependencyToModuleFile :: Context m => M.Module -> ModuleAlias -> ModuleURL -> MC.ModuleChecksum -> m ()
addDependencyToModuleFile targetModule alias url checksum = do
  Log.printNote' $
    "adding the dependency of `"
      <> BN.reify (extract alias)
      <> "` to the module file"
  let targetModule' = M.addDependency alias url checksum targetModule
  writeModule targetModule'

-- TIO.writeFile (toFilePath $ M.moduleLocation targetModule') $ M.ppModule targetModule'
