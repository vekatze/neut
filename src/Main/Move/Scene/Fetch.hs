module Main.Move.Scene.Fetch
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
import Data.ByteString qualified as B
import Data.Containers.ListUtils (nubOrdOn)
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Ens.Rule.Ens qualified as E
import Ens.Rule.Ens qualified as SE
import Error.Move.Run (forP)
import Error.Rule.EIO (EIO)
import Language.Common.Move.Raise (raiseError')
import Language.Common.Rule.BaseName (isCapitalized)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.Error (Error (MakeError))
import Language.Common.Rule.Hint
import Language.Common.Rule.ModuleAlias
import Language.Common.Rule.ModuleDigest qualified as MD
import Language.Common.Rule.ModuleID qualified as MID
import Logger.Move.Log qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.External qualified as External
import Main.Move.Context.Module qualified as Module
import Main.Move.Scene.Ens.Reflect qualified as EnsReflect
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Module.Reflect qualified as ModuleReflect
import Main.Move.Scene.Module.Save qualified as ModuleSave
import Main.Rule.Module (keyDependency, keyDigest, keyEnablePreset, keyMirror, moduleLocation)
import Main.Rule.Module qualified as M
import Main.Rule.ModuleURL
import Path
import Path.IO
import SyntaxTree.Rule.Series (Series (hasOptionalSeparator))
import SyntaxTree.Rule.Series qualified as SE

data Handle = Handle
  { ensReflectHandle :: EnsReflect.Handle,
    moduleSaveHandle :: ModuleSave.Handle,
    externalHandle :: External.Handle,
    loggerHandle :: Logger.Handle,
    envHandle :: Env.Handle
  }

new ::
  Base.Handle ->
  Handle
new (Base.Handle {..}) = do
  let ensReflectHandle = EnsReflect.new gensymHandle
  let moduleSaveHandle = ModuleSave.new loggerHandle
  let externalHandle = External.new loggerHandle
  Handle {..}

fetch :: Handle -> M.MainModule -> EIO ()
fetch h (M.MainModule baseModule) = do
  fetchDeps h $ collectDependency baseModule

fetchDeps :: Handle -> [(ModuleAlias, M.Dependency)] -> EIO ()
fetchDeps h deps = do
  deps' <- tidy h deps
  if null deps'
    then return ()
    else do
      next <- fmap concat $ forP deps' $ \(alias, dep) -> do
        installModule h alias (M.dependencyMirrorList dep) (M.dependencyDigest dep)
      fetchDeps h next

tidy :: Handle -> [(ModuleAlias, M.Dependency)] -> EIO [(ModuleAlias, M.Dependency)]
tidy h deps = do
  let deps' = nubOrdOn (M.dependencyDigest . snd) deps
  filterM (fmap not . checkIfInstalled h . M.dependencyDigest . snd) deps'

insertDependency :: Handle -> T.Text -> ModuleURL -> EIO ()
insertDependency h aliasName url = do
  aliasName' <- liftEither (BN.reflect' aliasName)
  when (isCapitalized aliasName') $ do
    raiseError' $ "Module aliases must not be capitalized, but found: " <> BN.reify aliasName'
  let alias = ModuleAlias aliasName'
  withSystemTempFile "fetch" $ \tempFilePath tempFileHandle -> do
    download h tempFilePath alias [url]
    archive <- liftIO $ B.hGetContents tempFileHandle
    let digest = MD.fromByteString archive
    let mainModule = Env.getMainModule (envHandle h)
    case Map.lookup alias (M.moduleDependency $ M.extractModule mainModule) of
      Just dep -> do
        if M.dependencyDigest dep == digest
          then do
            if url `elem` M.dependencyMirrorList dep
              then do
                moduleDirPath <- Module.getModuleDirByID mainModule Nothing (MID.Library digest)
                dependencyDirExists <- doesDirExist moduleDirPath
                if dependencyDirExists
                  then do
                    liftIO $ Logger.printNote' (loggerHandle h) $ "Already installed: " <> MD.reify digest
                  else do
                    liftIO $ printInstallationRemark h alias digest
                    installModule' h tempFilePath alias digest >>= fetchDeps h
              else do
                liftIO $ Logger.printNote' (loggerHandle h) $ "Adding a mirror of `" <> BN.reify (extract alias) <> "`"
                let dep' = dep {M.dependencyMirrorList = url : M.dependencyMirrorList dep}
                addDependencyToModuleFile h alias dep'
          else do
            liftIO $
              Logger.printNote' (loggerHandle h) $
                "Replacing a dependency: "
                  <> BN.reify (extract alias)
                  <> "\n- old: "
                  <> MD.reify (M.dependencyDigest dep)
                  <> "\n- new: "
                  <> MD.reify digest
            installModule' h tempFilePath alias digest >>= fetchDeps h
            let dep' = dep {M.dependencyDigest = digest, M.dependencyMirrorList = [url]}
            updateDependencyInModuleFile h (moduleLocation $ M.extractModule mainModule) alias dep'
      Nothing -> do
        liftIO $ printInstallationRemark h alias digest
        installModule' h tempFilePath alias digest >>= fetchDeps h
        addDependencyToModuleFile h alias $
          M.Dependency
            { dependencyMirrorList = [url],
              dependencyDigest = digest,
              dependencyPresetEnabled = False
            }

insertCoreDependency :: Handle -> EIO ()
insertCoreDependency h = do
  coreModuleURL <- Module.getCoreModuleURL
  digest <- Module.getCoreModuleDigest
  _ <- installModule h coreModuleAlias [coreModuleURL] digest
  addDependencyToModuleFile h coreModuleAlias $
    M.Dependency
      { dependencyMirrorList = [coreModuleURL],
        dependencyDigest = digest,
        dependencyPresetEnabled = True
      }

installModule :: Handle -> ModuleAlias -> [ModuleURL] -> MD.ModuleDigest -> EIO [(ModuleAlias, M.Dependency)]
installModule h alias mirrorList digest = do
  liftIO $ printInstallationRemark h alias digest
  withSystemTempFile "fetch" $ \tempFilePath tempFileHandle -> do
    download h tempFilePath alias mirrorList
    archive <- liftIO $ B.hGetContents tempFileHandle
    let archiveModuleDigest = MD.fromByteString archive
    when (digest /= archiveModuleDigest) $
      raiseError' $
        "The digest of the module `"
          <> BN.reify (extract alias)
          <> "` is different from the expected one:"
          <> "\n- "
          <> MD.reify digest
          <> " (expected)"
          <> "\n- "
          <> MD.reify archiveModuleDigest
          <> " (actual)"
    installModule' h tempFilePath alias digest

installModule' :: Handle -> Path Abs File -> ModuleAlias -> MD.ModuleDigest -> EIO [(ModuleAlias, M.Dependency)]
installModule' h archivePath alias digest = do
  extractToDependencyDir h archivePath alias digest
  libModule <- getLibraryModule h alias digest
  return $ collectDependency libModule

printInstallationRemark :: Handle -> ModuleAlias -> MD.ModuleDigest -> IO ()
printInstallationRemark h alias digest = do
  Logger.printNote' (loggerHandle h) $ "Install: " <> BN.reify (extract alias) <> " (" <> MD.reify digest <> ")"

collectDependency :: M.Module -> [(ModuleAlias, M.Dependency)]
collectDependency baseModule = do
  Map.toList $ M.moduleDependency baseModule

checkIfInstalled :: Handle -> MD.ModuleDigest -> EIO Bool
checkIfInstalled h digest = do
  let mainModule = Env.getMainModule (envHandle h)
  Module.getModuleFilePath mainModule Nothing (MID.Library digest) >>= doesFileExist

getLibraryModule :: Handle -> ModuleAlias -> MD.ModuleDigest -> EIO M.Module
getLibraryModule h alias digest = do
  let mainModule = Env.getMainModule (envHandle h)
  moduleFilePath <- Module.getModuleFilePath mainModule Nothing (MID.Library digest)
  moduleFileExists <- doesFileExist moduleFilePath
  if moduleFileExists
    then ModuleReflect.fromFilePath moduleFilePath
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
          liftIO $ Logger.printWarning' (loggerHandle h) $ "Could not process the module at: " <> mirror
          liftIO $ forM_ errorList $ Logger.printLog (loggerHandle h)
          download h tempFilePath ma rest

extractToDependencyDir :: Handle -> Path Abs File -> ModuleAlias -> MD.ModuleDigest -> EIO ()
extractToDependencyDir h archivePath _ digest = do
  let mainModule = Env.getMainModule (envHandle h)
  moduleDirPath <- Module.getModuleDirByID mainModule Nothing (MID.Library digest)
  ensureDir moduleDirPath
  External.run (externalHandle h) "tar" ["xf", toFilePath archivePath, "-C", toFilePath moduleDirPath]

addDependencyToModuleFile :: Handle -> ModuleAlias -> M.Dependency -> EIO ()
addDependencyToModuleFile h alias dep = do
  let mainModule = Env.getMainModule (envHandle h)
  let mm = M.extractModule mainModule
  (c1, (baseEns@(m :< _), c2)) <- EnsReflect.fromFilePath (moduleLocation mm)
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
  (c1, (baseEns@(m :< _), c2)) <- EnsReflect.fromFilePath mainModuleFileLoc
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
