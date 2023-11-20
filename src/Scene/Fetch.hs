module Scene.Fetch
  ( fetch,
    insertDependency,
    insertCoreDependency,
  )
where

import Context.App
import Context.External qualified as External
import Context.Fetch
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Remark qualified as Remark
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.BaseName (isCapitalized)
import Entity.BaseName qualified as BN
import Entity.Error (Error (MakeError))
import Entity.Module qualified as M
import Entity.ModuleAlias
import Entity.ModuleDigest qualified as MD
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Path
import Scene.Module.Reflect qualified as Module
import UnliftIO.Async

fetch :: M.Module -> App ()
fetch baseModule = do
  let dependency = Map.toList $ M.moduleDependency baseModule
  forConcurrently_ dependency $ \(alias, (mirrorList, digest)) ->
    installIfNecessary alias mirrorList digest

insertDependency :: T.Text -> ModuleURL -> App ()
insertDependency aliasName url = do
  aliasName' <- Throw.liftEither (BN.reflect' aliasName)
  when (isCapitalized aliasName') $ do
    Throw.raiseError' $ "module aliases must not be capitalized, but found: " <> BN.reify aliasName'
  let alias = ModuleAlias aliasName'
  mainModule <- Module.getMainModule
  withTempFile $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias [url]
    archive <- getHandleContents tempFileHandle
    let digest = MD.fromByteString archive
    extractToLibDir tempFilePath alias digest
    addDependencyToModuleFile mainModule alias [url] digest
    getLibraryModule alias digest >>= fetch

insertCoreDependency :: App ()
insertCoreDependency = do
  coreModuleURL <- Module.getCoreModuleURL
  digest <- Module.getCoreModuleDigest
  mainModule <- Module.getMainModule
  addDependencyToModuleFile mainModule coreModuleAlias [coreModuleURL] digest
  installIfNecessary coreModuleAlias [coreModuleURL] digest

installIfNecessary :: ModuleAlias -> [ModuleURL] -> MD.ModuleDigest -> App ()
installIfNecessary alias mirrorList digest = do
  isInstalled <- checkIfInstalled digest
  unless isInstalled $ do
    Remark.printNote' $ "installing a dependency: " <> BN.reify (extract alias) <> " (" <> MD.reify digest <> ")"
    withTempFile $ \tempFilePath tempFileHandle -> do
      download tempFilePath alias mirrorList
      archive <- getHandleContents tempFileHandle
      let archiveModuleDigest = MD.fromByteString archive
      when (digest /= archiveModuleDigest) $
        Throw.raiseError' $
          "the digest of the module `"
            <> BN.reify (extract alias)
            <> "` is different from the expected one:"
            <> "\n- "
            <> MD.reify digest
            <> " (expected)"
            <> "\n- "
            <> MD.reify archiveModuleDigest
            <> " (actual)"
      extractToLibDir tempFilePath alias digest
      getLibraryModule alias digest >>= fetch

checkIfInstalled :: MD.ModuleDigest -> App Bool
checkIfInstalled digest = do
  Module.getModuleFilePath Nothing (MID.Library digest) >>= Path.doesFileExist

getLibraryModule :: ModuleAlias -> MD.ModuleDigest -> App M.Module
getLibraryModule alias digest = do
  moduleFilePath <- Module.getModuleFilePath Nothing (MID.Library digest)
  moduleFileExists <- Path.doesFileExist moduleFilePath
  if moduleFileExists
    then Module.fromFilePath (MID.Library digest) moduleFilePath
    else
      Throw.raiseError' $
        "could not find the module file for `"
          <> BN.reify (extract alias)
          <> "` ("
          <> MD.reify digest
          <> ")."

download :: Path Abs File -> ModuleAlias -> [ModuleURL] -> App ()
download tempFilePath ma@(ModuleAlias alias) mirrorList = do
  case mirrorList of
    [] ->
      Throw.raiseError' $ "couldn't obtain the module `" <> BN.reify alias <> "`."
    ModuleURL mirror : rest -> do
      errOrUnit <- External.runOrFail "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack mirror]
      case errOrUnit of
        Right () ->
          return ()
        Left (MakeError errorList) -> do
          Remark.printWarning' $ "couldn't process the module at: " <> mirror
          forM_ errorList Remark.printRemark
          download tempFilePath ma rest

extractToLibDir :: Path Abs File -> ModuleAlias -> MD.ModuleDigest -> App ()
extractToLibDir tempFilePath _ digest = do
  moduleDirPath <- parent <$> Module.getModuleFilePath Nothing (MID.Library digest)
  Path.ensureDir moduleDirPath
  External.run "tar" ["xf", toFilePath tempFilePath, "-C", toFilePath moduleDirPath, "--strip-components=1"]

addDependencyToModuleFile :: M.Module -> ModuleAlias -> [ModuleURL] -> MD.ModuleDigest -> App ()
addDependencyToModuleFile targetModule alias mirrorList digest = do
  let targetModule' = M.addDependency alias mirrorList digest targetModule
  Module.save targetModule'
  Remark.printNote' $ "added a dependency: " <> BN.reify (extract alias) <> " (" <> MD.reify digest <> ")"
