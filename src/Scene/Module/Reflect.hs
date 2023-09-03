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
import Entity.Const (moduleFile)
import Entity.Hint qualified as H
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleDigest
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.SourceLocator qualified as SL
import Entity.Target
import Entity.Tree qualified as Tree
import Path
import Path.IO
import Scene.Tree.Reflect qualified as Tree

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
  (m, treeList) <- Tree.reflect moduleFilePath
  sourceDirTree <- liftEither $ Tree.accessOrEmpty m keySource treeList >>= Tree.extract
  archiveDirTree <- liftEither $ Tree.accessOrEmpty m keyArchive treeList >>= Tree.extract
  (_, entryPointTree) <- liftEither $ Tree.accessOrEmpty m keyTarget treeList >>= mapM Tree.toDictionary
  dependencyTree <- liftEither $ Tree.accessOrEmpty m keyDependency treeList >>= mapM Tree.toDictionary
  (_, extraContentTree) <- liftEither $ Tree.accessOrEmpty m keyExtraContent treeList
  (_, antecedentTree) <- liftEither $ Tree.accessOrEmpty m keyAntecedent treeList
  (_, foreignDirListTree) <- liftEither $ Tree.accessOrEmpty m keyForeign treeList
  let moduleRootDir = parent moduleFilePath
  archiveDir <- interpretDirPath archiveDirTree
  sourceDir <- interpretDirPath sourceDirTree
  target <- mapM interpretRelFilePath entryPointTree
  dependency <- interpretDependencyDict dependencyTree
  extraContents <- mapM (interpretExtraPath moduleRootDir) extraContentTree
  antecedents <- mapM interpretAntecedent antecedentTree
  foreignDirList <- mapM interpretDirPath foreignDirListTree
  return
    Module
      { moduleID = moduleID,
        moduleArchiveDir = archiveDir,
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

interpretRelFilePath :: (H.Hint, [Tree.Tree]) -> App SL.SourceLocator
interpretRelFilePath pathInfo = do
  (m, pathString) <- liftEither $ Tree.extract pathInfo >>= Tree.toString
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return $ SL.SourceLocator relPath
    Nothing ->
      raiseError m $ "invalid file path: " <> pathString

interpretDependencyDict ::
  (H.Hint, Map.HashMap T.Text (H.Hint, [Tree.Tree])) ->
  App (Map.HashMap ModuleAlias ([ModuleURL], ModuleDigest))
interpretDependencyDict (m, dep) = do
  items <- forM (Map.toList dep) $ \(k, (mDep, depTree)) -> do
    k' <- liftEither $ BN.reflect m k
    when (S.member k' BN.reservedAlias) $
      raiseError m $
        "the reserved name `"
          <> BN.reify k'
          <> "` cannot be used as an alias of a module"
    (_, urlTreeList) <- liftEither $ Tree.access mDep "mirror" depTree
    urlList <- liftEither $ mapM (Tree.toString >=> return . snd) urlTreeList
    (_, digest) <- liftEither $ Tree.access mDep "digest" depTree >>= Tree.extract >>= Tree.toString
    return (ModuleAlias k', (map ModuleURL urlList, ModuleDigest digest))
  return $ Map.fromList items

interpretExtraPath :: Path Abs Dir -> Tree.Tree -> App (SomePath Rel)
interpretExtraPath moduleRootDir entity = do
  (m, itemPathText) <- liftEither $ Tree.toString entity
  if T.last itemPathText == '/'
    then do
      dirPath <- parseRelDir $ T.unpack itemPathText
      ensureExistence m moduleRootDir dirPath Path.doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- parseRelFile $ T.unpack itemPathText
      ensureExistence m moduleRootDir filePath Path.doesFileExist "file"
      return $ Right filePath

interpretAntecedent :: Tree.Tree -> App ModuleDigest
interpretAntecedent ens = do
  (_, digestText) <- liftEither $ Tree.toString ens
  return $ ModuleDigest digestText

interpretDirPath :: Tree.Tree -> App (Path Rel Dir)
interpretDirPath ens = do
  (_, pathText) <- liftEither $ Tree.toString ens
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
