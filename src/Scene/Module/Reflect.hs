module Scene.Module.Reflect
  ( getModule,
    fromFilePath,
    fromCurrentPath,
    findModuleFile,
    getAllDependencies,
  )
where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.BaseName (isCapitalized)
import Entity.BaseName qualified as BN
import Entity.C
import Entity.Const (archiveRelDir, buildRelDir, moduleFile, sourceRelDir)
import Entity.Ens qualified as E
import Entity.Error
import Entity.GlobalLocator qualified as GL
import Entity.Hint qualified as H
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleDigest
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.SourceLocator qualified as SL
import Entity.Target
import Path
import Path.IO
import Scene.Ens.Reflect qualified as Ens

getModule ::
  H.Hint ->
  MID.ModuleID ->
  T.Text ->
  App Module
getModule m moduleID locatorText = do
  nextModuleFilePath <- Module.getModuleFilePath (Just m) moduleID
  mcm <- Module.getModuleCacheMap
  case Map.lookup nextModuleFilePath mcm of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        raiseError m $
          T.pack "could not find the module file for `"
            <> locatorText
            <> "`"
      nextModule <- fromFilePath moduleID nextModuleFilePath
      Module.insertToModuleCacheMap nextModuleFilePath nextModule
      return nextModule

fromFilePath :: MID.ModuleID -> Path Abs File -> App Module
fromFilePath moduleID moduleFilePath = do
  (_, (ens, _)) <- Ens.fromFilePath moduleFilePath
  (_, _, targetEns) <- liftEither $ E.access keyTarget ens >>= E.toDictionary . E.strip
  target <- mapM (interpretRelFilePath . E.strip) $ Map.fromList targetEns
  dependencyEns <- liftEither $ E.access' keyDependency E.emptyDict ens >>= E.toDictionary . E.strip
  dependency <- interpretDependencyDict dependencyEns
  (_, _, extraContentsEns) <- liftEither $ E.access' keyExtraContent E.emptyList ens >>= E.toList . E.strip
  extraContents <- mapM (interpretExtraPath $ parent moduleFilePath) extraContentsEns
  (_, _, antecedentsEns) <- liftEither $ E.access' keyAntecedent E.emptyList ens >>= E.toList . E.strip
  antecedents <- mapM (interpretAntecedent . fst) antecedentsEns
  archiveDirEns <- liftEither $ E.access' keyArchive (E.ensPath archiveRelDir) ens
  archiveDir <- interpretDirPath $ E.strip archiveDirEns
  buildDirEns <- liftEither $ E.access' keyBuild (E.ensPath buildRelDir) ens
  buildDir <- interpretDirPath $ E.strip buildDirEns
  sourceDirEns <- liftEither $ E.access' keySource (E.ensPath sourceRelDir) ens
  sourceDir <- interpretDirPath $ E.strip sourceDirEns
  (_, _, foreignDirListEns) <- liftEither $ E.access' keyForeign E.emptyList ens >>= E.toList . E.strip
  foreignDirList <- mapM (interpretDirPath . fst) foreignDirListEns
  (mPrefix, _, prefixEns) <- liftEither $ E.access' keyPrefix E.emptyDict ens >>= E.toDictionary . E.strip
  prefixMap <- liftEither $ interpretPrefixMap mPrefix prefixEns
  let mInlineLimit = interpretInlineLimit $ E.access keyInlineLimit ens
  (mPreset, _, presetEns) <- liftEither $ E.access' keyPreset E.emptyDict ens >>= E.toDictionary . E.strip
  presetMap <- liftEither $ interpretPresetMap mPreset presetEns
  return
    Module
      { moduleID = moduleID,
        moduleArchiveDir = archiveDir,
        moduleBuildDir = buildDir,
        moduleSourceDir = sourceDir,
        moduleTarget = Map.mapKeys Target target,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleAntecedents = antecedents,
        moduleLocation = moduleFilePath,
        moduleForeignDirList = foreignDirList,
        modulePrefixMap = prefixMap,
        moduleInlineLimit = mInlineLimit,
        modulePresetMap = presetMap
      }

getAllDependencies :: Module -> App [(ModuleAlias, Module)]
getAllDependencies baseModule =
  forM (Map.toList $ moduleDependency baseModule) $ \(alias, dependency) -> do
    let moduleID = MID.Library $ dependencyDigest dependency
    moduleFilePath <- Module.getModuleFilePath Nothing moduleID
    let m = H.newSourceHint moduleFilePath
    dep <- getModule m moduleID (MID.reify moduleID)
    return (alias, dep)

fromCurrentPath :: App Module
fromCurrentPath = do
  libraryDir <- Path.getLibraryDirPath
  moduleFilePath <- getCurrentModuleFilePath
  if isProperPrefixOf libraryDir moduleFilePath
    then do
      let moduleID = getDigestFromModulePath moduleFilePath
      getCurrentModuleFilePath >>= fromFilePath moduleID
    else getCurrentModuleFilePath >>= fromFilePath MID.Main

interpretPrefixMap ::
  H.Hint ->
  [(T.Text, (C, (E.Ens, C)))] ->
  Either Error (Map.HashMap BN.BaseName (ModuleAlias, SL.SourceLocator))
interpretPrefixMap m ens = do
  kvs' <- forM ens $ \(k, (_, (v, _))) -> do
    k' <- BN.reflect m k
    unless (isCapitalized k') $ do
      Left $ newError m $ "prefixes must be capitalized, but found: " <> BN.reify k'
    v' <- E.toString v >>= uncurry GL.reflectLocator
    return (k', v')
  return $ Map.fromList kvs'

interpretPresetMap ::
  H.Hint ->
  [(T.Text, (C, (E.Ens, C)))] ->
  Either Error (Map.HashMap LocatorName [BN.BaseName])
interpretPresetMap _ ens = do
  let (ks, cvcs) = unzip ens
  vs' <- forM (map (fst . snd) cvcs) $ \v -> do
    (_, _, presetList) <- E.toList v
    mapM (E.toString . fst >=> uncurry BN.reflect) presetList
  return $ Map.fromList $ zip ks vs'

interpretRelFilePath :: E.Ens -> App SL.SourceLocator
interpretRelFilePath ens = do
  (m, pathString) <- liftEither $ E.toString ens
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return $ SL.SourceLocator relPath
    Nothing ->
      raiseError m $ "invalid file path: " <> pathString

interpretDependencyDict ::
  (H.Hint, C, [(T.Text, (C, (E.Ens, C)))]) ->
  App (Map.HashMap ModuleAlias Dependency)
interpretDependencyDict (m, _, dep) = do
  items <- forM dep $ \(k, (_, (ens, _))) -> do
    k' <- liftEither $ BN.reflect m k
    when (BN.isCapitalized k') $ do
      raiseError m $ "module aliases can't be capitalized, but found: " <> BN.reify k'
    when (S.member k' BN.reservedAlias) $
      raiseError m $
        "the reserved name `"
          <> BN.reify k'
          <> "` cannot be used as an alias of a module"
    (_, _, urlEnsList) <- liftEither $ E.access keyMirror ens >>= E.toList . E.strip
    urlList <- liftEither $ mapM (E.toString . fst >=> return . snd) urlEnsList
    (_, digest) <- liftEither $ E.access keyDigest ens >>= E.toString . E.strip
    let mirrorList = map ModuleURL urlList
    let digest' = ModuleDigest digest
    return (ModuleAlias k', Dependency {dependencyMirrorList = mirrorList, dependencyDigest = digest'})
  return $ Map.fromList items

interpretExtraPath :: Path Abs Dir -> (E.Ens, a) -> App (SomePath Rel)
interpretExtraPath moduleRootDir (entity, _) = do
  (m, itemPathText) <- liftEither $ E.toString entity
  if T.last itemPathText == '/'
    then do
      dirPath <- parseRelDir $ T.unpack itemPathText
      ensureExistence m moduleRootDir dirPath Path.doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- parseRelFile $ T.unpack itemPathText
      ensureExistence m moduleRootDir filePath Path.doesFileExist "file"
      return $ Right filePath

interpretAntecedent :: E.Ens -> App ModuleDigest
interpretAntecedent ens = do
  (_, digestText) <- liftEither $ E.toString ens
  return $ ModuleDigest digestText

interpretDirPath :: E.Ens -> App (Path Rel Dir)
interpretDirPath ens = do
  (_, pathText) <- liftEither $ E.toString ens
  parseRelDir $ T.unpack pathText

interpretInlineLimit :: Either Error (C, (E.Ens, C)) -> Maybe Int
interpretInlineLimit errOrEns = do
  (_, (ens, _)) <- rightToMaybe errOrEns
  rightToMaybe $ E.toInt ens

ensureExistence ::
  H.Hint ->
  Path Abs Dir ->
  Path Rel t ->
  (Path Abs t -> App Bool) ->
  T.Text ->
  App ()
ensureExistence m moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker (moduleRootDir </> path)
  unless b $ do
    raiseError m $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath path)

findModuleFile :: Path Abs Dir -> Path Abs Dir -> App (Path Abs File)
findModuleFile baseDir moduleRootDirCandidate = do
  let moduleFileCandidate = moduleRootDirCandidate </> moduleFile
  moduleFileExists <- Path.doesFileExist moduleFileCandidate
  case (moduleFileExists, moduleRootDirCandidate /= parent moduleRootDirCandidate) of
    (True, _) ->
      return moduleFileCandidate
    (_, True) ->
      findModuleFile baseDir $ parent moduleRootDirCandidate
    _ ->
      raiseError' $ "couldn't find a module file (context: " <> T.pack (toFilePath baseDir) <> ")"

getCurrentModuleFilePath :: App (Path Abs File)
getCurrentModuleFilePath = do
  baseDir <- Path.getCurrentDir
  findModuleFile baseDir baseDir

rightToMaybe :: Either a b -> Maybe b
rightToMaybe errOrVal =
  case errOrVal of
    Left _ ->
      Nothing
    Right val ->
      Just val
