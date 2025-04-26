module Move.Scene.Fetch
  ( Handle,
    new,
    fetch,
    insertDependency,
    insertCoreDependency,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils (nubOrdOn)
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Move.Console.Report
import Move.Context.App
import Move.Context.EIO (EIO, raiseError', toApp)
import Move.Context.Env (getMainModule)
import Move.Context.External qualified as External
import Move.Context.Fetch qualified as Fetch
import Move.Context.Module qualified as Module
import Move.Context.Remark qualified as Remark
import Move.Context.Throw qualified as Throw
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.Module.Reflect qualified as Module
import Move.Scene.Module.Save qualified as ModuleSave
import Path
import Path.IO
import Rule.BaseName (isCapitalized)
import Rule.BaseName qualified as BN
import Rule.Ens qualified as E
import Rule.Ens qualified as SE
import Rule.Error (Error (MakeError))
import Rule.Hint
import Rule.Log (ColorSpec)
import Rule.Module (keyDependency, keyDigest, keyEnablePreset, keyMirror, moduleLocation)
import Rule.Module qualified as M
import Rule.ModuleAlias
import Rule.ModuleDigest qualified as MD
import Rule.ModuleID qualified as MID
import Rule.ModuleURL
import Rule.Syntax.Series (Series (hasOptionalSeparator))
import Rule.Syntax.Series qualified as SE
import UnliftIO.Async

data Handle
  = Handle
  { ensReflectHandle :: EnsReflect.Handle,
    moduleSaveHandle :: ModuleSave.Handle,
    externalHandle :: External.Handle,
    moduleHandle :: Module.Handle,
    mainModule :: M.MainModule,
    stdOutColorSpec :: ColorSpec
  }

new :: App Handle
new = do
  ensReflectHandle <- EnsReflect.new
  moduleSaveHandle <- ModuleSave.new
  externalHandle <- External.new
  moduleHandle <- Module.new
  mainModule <- getMainModule
  stdOutColorSpec <- getColorSpecStdOut
  return $ Handle {..}

fetch :: M.MainModule -> App ()
fetch (M.MainModule baseModule) = do
  fetchDeps $ collectDependency baseModule

fetchDeps :: [(ModuleAlias, M.Dependency)] -> App ()
fetchDeps deps = do
  deps' <- tidy deps
  if null deps'
    then return ()
    else do
      h <- new
      next <- fmap concat $ pooledForConcurrently deps' $ \(alias, dep) -> do
        installModule h alias (M.dependencyMirrorList dep) (M.dependencyDigest dep)
      fetchDeps next

tidy :: [(ModuleAlias, M.Dependency)] -> App [(ModuleAlias, M.Dependency)]
tidy deps = do
  let deps' = nubOrdOn (M.dependencyDigest . snd) deps
  h <- new
  toApp $ filterM (fmap not . checkIfInstalled h . M.dependencyDigest . snd) deps'

insertDependency :: Handle -> T.Text -> ModuleURL -> App ()
insertDependency h aliasName url = do
  aliasName' <- Throw.liftEither (BN.reflect' aliasName)
  when (isCapitalized aliasName') $ do
    Throw.raiseError' $ "Module aliases must not be capitalized, but found: " <> BN.reify aliasName'
  let alias = ModuleAlias aliasName'
  withSystemTempFile "fetch" $ \tempFilePath tempFileHandle -> do
    toApp $ download h tempFilePath alias [url]
    archive <- liftIO $ Fetch.getHandleContents tempFileHandle
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
                    liftIO $ printInstallationRemark h alias digest
                    toApp (installModule' h tempFilePath alias digest) >>= fetchDeps
              else do
                Remark.printNote' $ "Adding a mirror of `" <> BN.reify (extract alias) <> "`"
                let dep' = dep {M.dependencyMirrorList = url : M.dependencyMirrorList dep}
                toApp $ addDependencyToModuleFile h alias dep'
          else do
            Remark.printNote' $
              "Replacing a dependency: "
                <> BN.reify (extract alias)
                <> "\n- old: "
                <> MD.reify (M.dependencyDigest dep)
                <> "\n- new: "
                <> MD.reify digest
            toApp (installModule' h tempFilePath alias digest) >>= fetchDeps
            let dep' = dep {M.dependencyDigest = digest, M.dependencyMirrorList = [url]}
            toApp $ updateDependencyInModuleFile h (moduleLocation $ M.extractModule mainModule) alias dep'
      Nothing -> do
        liftIO $ printInstallationRemark h alias digest
        toApp (installModule' h tempFilePath alias digest) >>= fetchDeps
        toApp $
          addDependencyToModuleFile h alias $
            M.Dependency
              { dependencyMirrorList = [url],
                dependencyDigest = digest,
                dependencyPresetEnabled = False
              }

insertCoreDependency :: App ()
insertCoreDependency = do
  coreModuleURL <- Module.getCoreModuleURL
  digest <- Module.getCoreModuleDigest
  h <- new
  _ <- installModule h coreModuleAlias [coreModuleURL] digest
  toApp $
    addDependencyToModuleFile h coreModuleAlias $
      M.Dependency
        { dependencyMirrorList = [coreModuleURL],
          dependencyDigest = digest,
          dependencyPresetEnabled = True
        }

installModule :: Handle -> ModuleAlias -> [ModuleURL] -> MD.ModuleDigest -> App [(ModuleAlias, M.Dependency)]
installModule h alias mirrorList digest = do
  liftIO $ printInstallationRemark h alias digest
  withSystemTempFile "fetch" $ \tempFilePath tempFileHandle -> do
    toApp $ download h tempFilePath alias mirrorList
    archive <- liftIO $ Fetch.getHandleContents tempFileHandle
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
    toApp $ installModule' h tempFilePath alias digest

installModule' :: Handle -> Path Abs File -> ModuleAlias -> MD.ModuleDigest -> EIO [(ModuleAlias, M.Dependency)]
installModule' h archivePath alias digest = do
  extractToDependencyDir h archivePath alias digest
  libModule <- getLibraryModule h alias digest
  return $ collectDependency libModule

printInstallationRemark :: Handle -> ModuleAlias -> MD.ModuleDigest -> IO ()
printInstallationRemark h alias digest = do
  printNote' (stdOutColorSpec h) $ "Install: " <> BN.reify (extract alias) <> " (" <> MD.reify digest <> ")"

collectDependency :: M.Module -> [(ModuleAlias, M.Dependency)]
collectDependency baseModule = do
  Map.toList $ M.moduleDependency baseModule

checkIfInstalled :: Handle -> MD.ModuleDigest -> EIO Bool
checkIfInstalled h digest = do
  Module.getModuleFilePath (mainModule h) Nothing (MID.Library digest) >>= doesFileExist

getLibraryModule :: Handle -> ModuleAlias -> MD.ModuleDigest -> EIO M.Module
getLibraryModule h alias digest = do
  moduleFilePath <- Module.getModuleFilePath (mainModule h) Nothing (MID.Library digest)
  moduleFileExists <- doesFileExist moduleFilePath
  if moduleFileExists
    then do
      Module.fromFilePath (moduleHandle h) moduleFilePath
    else
      raiseError' $
        "Could not find the module file for `"
          <> BN.reify (extract alias)
          <> "` ("
          <> MD.reify digest
          <> ")."

download :: Handle -> Path Abs File -> ModuleAlias -> [ModuleURL] -> EIO ()
download h tempFilePath ma@(ModuleAlias alias) mirrorList = do
  case mirrorList of
    [] ->
      raiseError' $ "Could not obtain the module `" <> BN.reify alias <> "`."
    ModuleURL mirror : rest -> do
      errOrUnit <- External.runOrFail (externalHandle h) "curl" ["-s", "-S", "-L", "-o", toFilePath tempFilePath, T.unpack mirror]
      case errOrUnit of
        Right () ->
          return ()
        Left (MakeError errorList) -> do
          liftIO $ printWarning' (stdOutColorSpec h) $ "Could not process the module at: " <> mirror
          liftIO $ forM_ errorList $ printRemark (stdOutColorSpec h)
          download h tempFilePath ma rest

extractToDependencyDir :: Handle -> Path Abs File -> ModuleAlias -> MD.ModuleDigest -> EIO ()
extractToDependencyDir h archivePath _ digest = do
  moduleDirPath <- Module.getModuleDirByID (mainModule h) Nothing (MID.Library digest)
  ensureDir moduleDirPath
  External.run (externalHandle h) "tar" ["xf", toFilePath archivePath, "-C", toFilePath moduleDirPath]

addDependencyToModuleFile :: Handle -> ModuleAlias -> M.Dependency -> EIO ()
addDependencyToModuleFile h alias dep = do
  let mm = M.extractModule (mainModule h)
  (c1, (baseEns@(m :< _), c2)) <- EnsReflect.fromFilePath (ensReflectHandle h) (moduleLocation mm)
  let depEns = makeDependencyEns m alias dep
  mergedEns <- liftEither $ E.merge baseEns depEns
  ModuleSave.save (moduleSaveHandle h) (M.moduleLocation mm) (c1, (mergedEns, c2))

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

updateDependencyInModuleFile :: Handle -> Path Abs File -> ModuleAlias -> M.Dependency -> EIO ()
updateDependencyInModuleFile h mainModuleFileLoc alias dep = do
  (c1, (baseEns@(m :< _), c2)) <- EnsReflect.fromFilePath (ensReflectHandle h) mainModuleFileLoc
  let depEns = makeDependencyEns' m dep
  mergedEns <- liftEither $ E.conservativeUpdate [keyDependency, BN.reify (extract alias)] depEns baseEns
  ModuleSave.save (moduleSaveHandle h) mainModuleFileLoc (c1, (mergedEns, c2))

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
