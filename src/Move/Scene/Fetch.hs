module Move.Scene.Fetch
  ( fetch,
    insertDependency,
    insertCoreDependency,
  )
where

import Move.Context.App
import Move.Context.Env (getMainModule)
import Move.Context.External qualified as External
import Move.Context.Fetch
import Move.Context.Module qualified as Module
import Move.Context.Path qualified as Path
import Move.Context.Remark qualified as Remark
import Move.Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.Containers.ListUtils (nubOrdOn)
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Rule.BaseName (isCapitalized)
import Rule.BaseName qualified as BN
import Rule.Ens qualified as E
import Rule.Ens qualified as SE
import Rule.Error (Error (MakeError))
import Rule.Hint
import Rule.Module (keyDependency, keyDigest, keyEnablePreset, keyMirror, moduleLocation)
import Rule.Module qualified as M
import Rule.ModuleAlias
import Rule.ModuleDigest qualified as MD
import Rule.ModuleID qualified as MID
import Rule.ModuleURL
import Rule.Syntax.Series (Series (hasOptionalSeparator))
import Rule.Syntax.Series qualified as SE
import Path
import Move.Scene.Ens.Reflect qualified as Ens
import Move.Scene.Module.Reflect qualified as Module
import UnliftIO.Async

fetch :: M.Module -> App ()
fetch baseModule = do
  fetchDeps $ collectDependency baseModule

fetchDeps :: [(ModuleAlias, M.Dependency)] -> App ()
fetchDeps deps = do
  deps' <- tidy deps
  if null deps'
    then return ()
    else do
      next <- fmap concat $ pooledForConcurrently deps' $ \(alias, dep) -> do
        installModule alias (M.dependencyMirrorList dep) (M.dependencyDigest dep)
      fetchDeps next

tidy :: [(ModuleAlias, M.Dependency)] -> App [(ModuleAlias, M.Dependency)]
tidy deps = do
  let deps' = nubOrdOn (M.dependencyDigest . snd) deps
  filterM (fmap not . checkIfInstalled . M.dependencyDigest . snd) deps'

insertDependency :: T.Text -> ModuleURL -> App ()
insertDependency aliasName url = do
  aliasName' <- Throw.liftEither (BN.reflect' aliasName)
  when (isCapitalized aliasName') $ do
    Throw.raiseError' $ "Module aliases must not be capitalized, but found: " <> BN.reify aliasName'
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
                dependencyDirExists <- Path.doesDirExist moduleDirPath
                if dependencyDirExists
                  then do
                    Remark.printNote' $ "Already installed: " <> MD.reify digest
                  else do
                    printInstallationRemark alias digest
                    installModule' tempFilePath alias digest >>= fetchDeps
              else do
                Remark.printNote' $ "Adding a mirror of `" <> BN.reify (extract alias) <> "`"
                let dep' = dep {M.dependencyMirrorList = url : M.dependencyMirrorList dep}
                addDependencyToModuleFile alias dep'
          else do
            Remark.printNote' $
              "Replacing a dependency: "
                <> BN.reify (extract alias)
                <> "\n- old: "
                <> MD.reify (M.dependencyDigest dep)
                <> "\n- new: "
                <> MD.reify digest
            installModule' tempFilePath alias digest >>= fetchDeps
            let dep' = dep {M.dependencyDigest = digest, M.dependencyMirrorList = [url]}
            updateDependencyInModuleFile (moduleLocation mainModule) alias dep'
      Nothing -> do
        printInstallationRemark alias digest
        installModule' tempFilePath alias digest >>= fetchDeps
        addDependencyToModuleFile alias $
          M.Dependency
            { dependencyMirrorList = [url],
              dependencyDigest = digest,
              dependencyPresetEnabled = False
            }

insertCoreDependency :: App ()
insertCoreDependency = do
  coreModuleURL <- Module.getCoreModuleURL
  digest <- Module.getCoreModuleDigest
  _ <- installModule coreModuleAlias [coreModuleURL] digest
  addDependencyToModuleFile coreModuleAlias $
    M.Dependency
      { dependencyMirrorList = [coreModuleURL],
        dependencyDigest = digest,
        dependencyPresetEnabled = True
      }

installModule :: ModuleAlias -> [ModuleURL] -> MD.ModuleDigest -> App [(ModuleAlias, M.Dependency)]
installModule alias mirrorList digest = do
  printInstallationRemark alias digest
  withTempFile $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias mirrorList
    archive <- getHandleContents tempFileHandle
    let archiveModuleDigest = MD.fromByteString archive
    when (digest /= archiveModuleDigest) $
      Throw.raiseError' $
        "The digest of the module `"
          <> BN.reify (extract alias)
          <> "` is different from the expected one:"
          <> "\n- "
          <> MD.reify digest
          <> " (expected)"
          <> "\n- "
          <> MD.reify archiveModuleDigest
          <> " (actual)"
    installModule' tempFilePath alias digest

installModule' :: Path Abs File -> ModuleAlias -> MD.ModuleDigest -> App [(ModuleAlias, M.Dependency)]
installModule' archivePath alias digest = do
  extractToDependencyDir archivePath alias digest
  libModule <- getLibraryModule alias digest
  return $ collectDependency libModule

printInstallationRemark :: ModuleAlias -> MD.ModuleDigest -> App ()
printInstallationRemark alias digest = do
  Remark.printNote' $ "Install: " <> BN.reify (extract alias) <> " (" <> MD.reify digest <> ")"

collectDependency :: M.Module -> [(ModuleAlias, M.Dependency)]
collectDependency baseModule = do
  Map.toList $ M.moduleDependency baseModule

checkIfInstalled :: MD.ModuleDigest -> App Bool
checkIfInstalled digest = do
  Module.getModuleFilePath Nothing (MID.Library digest) >>= Path.doesFileExist

getLibraryModule :: ModuleAlias -> MD.ModuleDigest -> App M.Module
getLibraryModule alias digest = do
  moduleFilePath <- Module.getModuleFilePath Nothing (MID.Library digest)
  moduleFileExists <- Path.doesFileExist moduleFilePath
  if moduleFileExists
    then Module.fromFilePath moduleFilePath
    else
      Throw.raiseError' $
        "Could not find the module file for `"
          <> BN.reify (extract alias)
          <> "` ("
          <> MD.reify digest
          <> ")."

download :: Path Abs File -> ModuleAlias -> [ModuleURL] -> App ()
download tempFilePath ma@(ModuleAlias alias) mirrorList = do
  case mirrorList of
    [] ->
      Throw.raiseError' $ "Could not obtain the module `" <> BN.reify alias <> "`."
    ModuleURL mirror : rest -> do
      errOrUnit <- External.runOrFail "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack mirror]
      case errOrUnit of
        Right () ->
          return ()
        Left (MakeError errorList) -> do
          Remark.printWarning' $ "Could not process the module at: " <> mirror
          forM_ errorList Remark.printRemark
          download tempFilePath ma rest

extractToDependencyDir :: Path Abs File -> ModuleAlias -> MD.ModuleDigest -> App ()
extractToDependencyDir archivePath _ digest = do
  moduleDirPath <- Module.getModuleDirByID Nothing (MID.Library digest)
  Path.ensureDir moduleDirPath
  External.run "tar" ["xf", toFilePath archivePath, "-C", toFilePath moduleDirPath]

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
  let preset = if enablePreset then Just (keyEnablePreset, m :< E.Bool enablePreset) else Nothing
  let mirrorList' = SE.fromList SE.Bracket SE.Comma $ map (\(ModuleURL mirror) -> m :< E.String mirror) mirrorList
  SE.dictFromList
    m
    [ ( keyDependency,
        SE.dictFromList
          m
          [ ( BN.reify $ extract alias,
              SE.dictFromList
                m
                ( [ (keyDigest, m :< E.String (MD.reify digest)),
                    (keyMirror, m :< E.List (mirrorList' {hasOptionalSeparator = True}))
                  ]
                    ++ maybeToList preset
                )
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
  let mirrorList' = SE.fromList SE.Bracket SE.Comma $ map (\(ModuleURL mirror) -> m :< E.String mirror) mirrorList
  let enablePreset = M.dependencyPresetEnabled dep
  let enablePresetField =
        if enablePreset
          then Just (keyEnablePreset, m :< E.Bool enablePreset)
          else Nothing
  SE.dictFromList
    m
    $ [ (keyDigest, m :< E.String (MD.reify digest)),
        (keyMirror, m :< E.List (mirrorList' {hasOptionalSeparator = True}))
      ]
      ++ catMaybes [enablePresetField]
