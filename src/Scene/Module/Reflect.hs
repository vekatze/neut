module Scene.Module.Reflect
  ( getModule,
    fromFilePath,
    fromCurrentPath,
    findModuleFile,
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
import Entity.BaseName qualified as BN
import Entity.Const (archiveRelDir, buildRelDir, moduleFile, sourceRelDir)
import Entity.Ens qualified as E
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
  ens <- Ens.fromFilePath moduleFilePath
  (_, targetEns) <- liftEither $ E.access keyTarget ens >>= E.toDictionary
  target <- mapM interpretRelFilePath targetEns
  dependencyEns <- liftEither $ E.access' keyDependency E.emptyDict ens >>= E.toDictionary
  dependency <- interpretDependencyDict dependencyEns
  extraContentsEns <- liftEither $ E.access' keyExtraContent E.emptyList ens >>= E.toList
  extraContents <- mapM (interpretExtraPath $ parent moduleFilePath) extraContentsEns
  antecedentsEns <- liftEither $ E.access' keyAntecedent E.emptyList ens >>= E.toList
  antecedents <- mapM interpretAntecedent antecedentsEns
  archiveDirEns <- liftEither $ E.access' keyArchive (E.ensPath archiveRelDir) ens
  archiveDir <- interpretDirPath archiveDirEns
  buildDirEns <- liftEither $ E.access' keyBuild (E.ensPath buildRelDir) ens
  buildDir <- interpretDirPath buildDirEns
  sourceDirEns <- liftEither $ E.access' keySource (E.ensPath sourceRelDir) ens
  sourceDir <- interpretDirPath sourceDirEns
  foreignDirListEns <- liftEither $ E.access' keyForeign E.emptyList ens >>= E.toList
  foreignDirList <- mapM interpretDirPath foreignDirListEns
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
        moduleForeignDirList = foreignDirList
      }

fromCurrentPath :: App Module
fromCurrentPath =
  getCurrentModuleFilePath >>= fromFilePath MID.Main

interpretRelFilePath :: E.Ens -> App SL.SourceLocator
interpretRelFilePath ens = do
  (m, pathString) <- liftEither $ E.toString ens
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return $ SL.SourceLocator relPath
    Nothing ->
      raiseError m $ "invalid file path: " <> pathString

interpretDependencyDict ::
  (H.Hint, Map.HashMap T.Text E.Ens) ->
  App (Map.HashMap ModuleAlias ([ModuleURL], ModuleDigest))
interpretDependencyDict (m, dep) = do
  items <- forM (Map.toList dep) $ \(k, ens) -> do
    k' <- liftEither $ BN.reflect m k
    when (S.member k' BN.reservedAlias) $
      raiseError m $
        "the reserved name `"
          <> BN.reify k'
          <> "` cannot be used as an alias of a module"
    urlEnsList <- liftEither $ E.access "mirror" ens >>= E.toList
    urlList <- liftEither $ mapM (E.toString >=> return . snd) urlEnsList
    (_, digest) <- liftEither $ E.access "digest" ens >>= E.toString
    return (ModuleAlias k', (map ModuleURL urlList, ModuleDigest digest))
  return $ Map.fromList items

interpretExtraPath :: Path Abs Dir -> E.Ens -> App (SomePath Rel)
interpretExtraPath moduleRootDir entity = do
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
