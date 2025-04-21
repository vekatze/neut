module Move.Scene.Fetch
  ( fetch,
    insertDependency,
    insertCoreDependency,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Reader (asks)
import Data.Containers.ListUtils (nubOrdOn)
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (toApp)
import Move.Context.Env (getMainModule)
import Move.Context.External qualified as External
import Move.Context.Fetch qualified as Fetch
import Move.Context.Module qualified as Module
import Move.Context.Remark qualified as Remark
import Move.Context.Throw qualified as Throw
import Move.Scene.Ens.Reflect (Handle (Handle))
import Move.Scene.Ens.Reflect qualified as Ens
import Move.Scene.Module.Reflect qualified as Module
import Path
import Path.IO
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
import UnliftIO.Async

fetch :: M.MainModule -> App ()
fetch (M.MainModule baseModule) = do
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
  Fetch.withTempFile $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias [url]
    archive <- Fetch.getHandleContents tempFileHandle
    let digest = MD.fromByteString archive
    mainModule <- getMainModule
    case Map.lookup alias (M.moduleDependency $ M.extractModule mainModule) of
      Just dep -> do
        if M.dependencyDigest dep == digest
          then do
            if url `elem` M.dependencyMirrorList dep
              then do
                moduleDirPath <- toApp $ Module.getModuleDirByID mainModule Nothing (MID.Library digest)
                dependencyDirExists <- doesDirExist moduleDirPath
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
            updateDependencyInModuleFile (moduleLocation $ M.extractModule mainModule) alias dep'
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
  Fetch.withTempFile $ \tempFilePath tempFileHandle -> do
    download tempFilePath alias mirrorList
    archive <- Fetch.getHandleContents tempFileHandle
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
  mainModule <- getMainModule
  toApp (Module.getModuleFilePath mainModule Nothing (MID.Library digest)) >>= doesFileExist

getLibraryModule :: ModuleAlias -> MD.ModuleDigest -> App M.Module
getLibraryModule alias digest = do
  mainModule <- getMainModule
  moduleFilePath <- toApp $ Module.getModuleFilePath mainModule Nothing (MID.Library digest)
  moduleFileExists <- doesFileExist moduleFilePath
  if moduleFileExists
    then do
      counter <- asks App.counter
      let h = Module.Handle {counter}
      toApp $ Module.fromFilePath h moduleFilePath
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
  mainModule <- getMainModule
  moduleDirPath <- toApp $ Module.getModuleDirByID mainModule Nothing (MID.Library digest)
  ensureDir moduleDirPath
  External.run "tar" ["xf", toFilePath archivePath, "-C", toFilePath moduleDirPath]

addDependencyToModuleFile :: ModuleAlias -> M.Dependency -> App ()
addDependencyToModuleFile alias dep = do
  M.MainModule mainModule <- getMainModule
  counter <- asks App.counter
  let h = Handle {counter}
  (c1, (baseEns@(m :< _), c2)) <- toApp $ Ens.fromFilePath h (moduleLocation mainModule)
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
  counter <- asks App.counter
  let h = Handle {counter}
  (c1, (baseEns@(m :< _), c2)) <- toApp $ Ens.fromFilePath h mainModuleFileLoc
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
