module Scene.Fetch
  ( fetch,
    insertDependency,
  )
where

import Context.App
import Context.External qualified as External
import Context.Fetch
import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Module qualified as M
import Entity.ModuleAlias
import Entity.ModuleChecksum qualified as MC
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Path
import Scene.Module.Reflect qualified as Module
import Prelude hiding (log)

fetch :: M.Module -> App ()
fetch baseModule = do
  let dependency = Map.toList $ M.moduleDependency baseModule
  forM_ dependency $ \(alias, (url, checksum)) ->
    installIfNecessary alias url checksum

insertDependency :: T.Text -> ModuleURL -> App ()
insertDependency aliasName url = do
  alias <- ModuleAlias <$> Throw.liftEither (BN.reflect' aliasName)
  mainModule <- Module.getMainModule
  withTempFile $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias url
    archive <- getHandleContents tempFileHandle
    let checksum = MC.fromByteString archive
    extractToLibDir tempFilePath alias checksum
    addDependencyToModuleFile mainModule alias url checksum
    getLibraryModule alias checksum >>= fetch

installIfNecessary :: ModuleAlias -> ModuleURL -> MC.ModuleChecksum -> App ()
installIfNecessary alias url checksum = do
  isInstalled <- checkIfInstalled checksum
  unless isInstalled $ do
    Log.printNote' $ "installing a dependency: " <> BN.reify (extract alias) <> " (" <> MC.reify checksum <> ")"
    withTempFile $ \tempFilePath tempFileHandle -> do
      download tempFilePath alias url
      archive <- getHandleContents tempFileHandle
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

checkIfInstalled :: MC.ModuleChecksum -> App Bool
checkIfInstalled checksum = do
  Module.getModuleFilePath Nothing (MID.Library checksum) >>= Path.doesFileExist

getLibraryModule :: ModuleAlias -> MC.ModuleChecksum -> App M.Module
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

download :: Path Abs File -> ModuleAlias -> ModuleURL -> App ()
download tempFilePath _ (ModuleURL url) = do
  External.run "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack url]

extractToLibDir :: Path Abs File -> ModuleAlias -> MC.ModuleChecksum -> App ()
extractToLibDir tempFilePath _ checksum = do
  targetDirPath <- parent <$> Module.getModuleFilePath Nothing (MID.Library checksum)
  Path.ensureDir targetDirPath
  External.run "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath targetDirPath, "--strip-components=1"]

addDependencyToModuleFile :: M.Module -> ModuleAlias -> ModuleURL -> MC.ModuleChecksum -> App ()
addDependencyToModuleFile targetModule alias url checksum = do
  let targetModule' = M.addDependency alias url checksum targetModule
  Module.save targetModule'
  Log.printNote' $ "added a dependency: " <> BN.reify (extract alias) <> " (" <> MC.reify checksum <> ")"
