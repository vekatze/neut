module Scene.Fetch
  ( fetch,
    insertDependency,
    insertCoreDependency,
  )
where

import Context.App
import Context.External qualified as External
import Context.Fetch
import Context.Module (getMainModule)
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Remark qualified as Remark
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.BaseName (isCapitalized)
import Entity.BaseName qualified as BN
import Entity.Ens qualified as E
import Entity.Error (Error (MakeError))
import Entity.Hint
import Entity.Module (keyDependency, keyDigest, keyEnablePreset, keyMirror, moduleLocation)
import Entity.Module qualified as M
import Entity.ModuleAlias
import Entity.ModuleDigest qualified as MD
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.Target
import Path
import Scene.Build qualified as Build
import Scene.Ens.Reflect qualified as Ens
import Scene.Module.Reflect qualified as Module
import UnliftIO.Async

fetch :: M.Module -> App ()
fetch baseModule = do
  let dependency = Map.toList $ M.moduleDependency baseModule
  forConcurrently_ dependency $ \(alias, dep) ->
    installIfNecessary alias (M.dependencyMirrorList dep) (M.dependencyDigest dep)

insertDependency :: T.Text -> ModuleURL -> App ()
insertDependency aliasName url = do
  aliasName' <- Throw.liftEither (BN.reflect' aliasName)
  when (isCapitalized aliasName') $ do
    Throw.raiseError' $ "module aliases must not be capitalized, but found: " <> BN.reify aliasName'
  let alias = ModuleAlias aliasName'
  withTempFile $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias [url]
    archive <- getHandleContents tempFileHandle
    let digest = MD.fromByteString archive
    mainModule <- getMainModule
    case Map.lookup alias (M.moduleDependency mainModule) of
      Just dep -> do
        if M.dependencyDigest dep == digest
          then do
            if url `elem` M.dependencyMirrorList dep
              then do
                moduleDirPath <- Module.getModuleDirByID Nothing (MID.Library digest)
                libDirExists <- Path.doesDirExist moduleDirPath
                if libDirExists
                  then do
                    Remark.printNote' $ "already installed: `" <> MD.reify digest
                  else do
                    Remark.printNote' $ "reinstalling a dependency: `" <> MD.reify digest <> "`"
                    installModule tempFilePath alias digest
              else do
                Remark.printNote' $ "adding a mirror of `" <> BN.reify (extract alias) <> "`"
                let dep' = dep {M.dependencyMirrorList = url : M.dependencyMirrorList dep}
                addDependencyToModuleFile alias dep'
          else do
            Remark.printNote' $
              "replacing a dependency: "
                <> BN.reify (extract alias)
                <> "\n- old: "
                <> MD.reify (M.dependencyDigest dep)
                <> "\n- new: "
                <> MD.reify digest
            let dep' = dep {M.dependencyDigest = digest, M.dependencyMirrorList = [url]}
            updateDependencyInModuleFile (moduleLocation mainModule) alias dep'
            installModule tempFilePath alias digest
      Nothing -> do
        Remark.printNote' $ "adding a dependency: " <> BN.reify (extract alias) <> " (" <> MD.reify digest <> ")"
        addDependencyToModuleFile alias $
          M.Dependency
            { dependencyMirrorList = [url],
              dependencyDigest = digest,
              dependencyPresetEnabled = False
            }
        installModule tempFilePath alias digest

installModule :: Path Abs File -> ModuleAlias -> MD.ModuleDigest -> App ()
installModule archivePath alias digest = do
  extractToLibDir archivePath alias digest
  libModule <- getLibraryModule alias digest
  fetch libModule
  Build.buildTarget Build.abstractAxis libModule (Abstract Foundation)

insertCoreDependency :: App ()
insertCoreDependency = do
  coreModuleURL <- Module.getCoreModuleURL
  digest <- Module.getCoreModuleDigest
  addDependencyToModuleFile coreModuleAlias $
    M.Dependency
      { dependencyMirrorList = [coreModuleURL],
        dependencyDigest = digest,
        dependencyPresetEnabled = True
      }
  installIfNecessary coreModuleAlias [coreModuleURL] digest

installIfNecessary :: ModuleAlias -> [ModuleURL] -> MD.ModuleDigest -> App ()
installIfNecessary alias mirrorList digest = do
  isInstalled <- checkIfInstalled digest
  unless isInstalled $ do
    Remark.printNote' $ "adding a dependency: " <> BN.reify (extract alias) <> " (" <> MD.reify digest <> ")"
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
      libModule <- getLibraryModule alias digest
      fetch libModule
      Build.buildTarget Build.abstractAxis libModule (Abstract Foundation)

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
extractToLibDir archivePath _ digest = do
  moduleDirPath <- Module.getModuleDirByID Nothing (MID.Library digest)
  Path.ensureDir moduleDirPath
  External.run "tar" ["xf", toFilePath archivePath, "-C", toFilePath moduleDirPath, "--strip-components=1"]

addDependencyToModuleFile :: ModuleAlias -> M.Dependency -> App ()
addDependencyToModuleFile alias dep = do
  mainModule <- getMainModule
  (c1, (baseEns@(m :< _), c2)) <- Ens.fromFilePath (moduleLocation mainModule)
  let depEns = makeDependencyEns m alias dep
  mergedEns <- Throw.liftEither $ E.merge baseEns depEns
  Module.saveEns (M.moduleLocation mainModule) (c1, (mergedEns, c2))

makeDependencyEns :: Hint -> ModuleAlias -> M.Dependency -> E.Ens
makeDependencyEns m alias dep = do
  let digest = M.dependencyDigest dep
  let mirrorList = M.dependencyMirrorList dep
  let enablePreset = M.dependencyPresetEnabled dep
  m
    :< E.Dictionary
      []
      [ ( keyDependency,
          E.inject $
            m
              :< E.Dictionary
                []
                [ ( BN.reify $ extract alias,
                    E.inject $
                      m
                        :< E.Dictionary
                          []
                          [ (keyDigest, E.inject $ m :< E.String (MD.reify digest)),
                            ( keyMirror,
                              E.inject $
                                m :< E.List [] (map (\(ModuleURL mirror) -> (m :< E.String mirror, [])) mirrorList)
                            ),
                            (keyEnablePreset, E.inject $ m :< E.Bool enablePreset)
                          ]
                  )
                ]
        )
      ]

updateDependencyInModuleFile :: Path Abs File -> ModuleAlias -> M.Dependency -> App ()
updateDependencyInModuleFile mainModuleFileLoc alias dep = do
  (c1, (baseEns@(m :< _), c2)) <- Ens.fromFilePath mainModuleFileLoc
  let depEns = makeDependencyEns' m dep
  mergedEns <- Throw.liftEither $ E.conservativeUpdate [keyDependency, BN.reify (extract alias)] depEns baseEns
  Module.saveEns mainModuleFileLoc (c1, (mergedEns, c2))

makeDependencyEns' :: Hint -> M.Dependency -> E.Ens
makeDependencyEns' m dep = do
  let digest = M.dependencyDigest dep
  let mirrorList = M.dependencyMirrorList dep
  let enablePreset = M.dependencyPresetEnabled dep
  m
    :< E.Dictionary
      []
      [ (keyDigest, E.inject $ m :< E.String (MD.reify digest)),
        ( keyMirror,
          E.inject $
            m :< E.List [] (map (\(ModuleURL mirror) -> (m :< E.String mirror, [])) mirrorList)
        ),
        (keyEnablePreset, E.inject $ m :< E.Bool enablePreset)
      ]
