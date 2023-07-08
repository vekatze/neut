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
import Context.Remark (printNote')
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
import Entity.StrictGlobalLocator qualified as SGL
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
  (_, entryPointTree) <- liftEither $ Tree.accessOrEmpty m "target" treeList >>= mapM Tree.toDictionary
  dependencyTree <- liftEither $ Tree.accessOrEmpty m "dependency" treeList >>= mapM Tree.toDictionary
  (_, extraContentTree) <- liftEither $ Tree.accessOrEmpty m "extra-content" treeList
  (_, antecedentTree) <- liftEither $ Tree.accessOrEmpty m "antecedent" treeList
  target <- mapM (interpretRelFilePath moduleID) entryPointTree
  dependency <- interpretDependencyDict dependencyTree
  extraContents <- mapM (interpretExtraPath $ parent moduleFilePath) extraContentTree
  antecedents <- mapM interpretAntecedent antecedentTree
  return
    Module
      { moduleID = moduleID,
        moduleTarget = Map.mapKeys Target target,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleAntecedents = antecedents,
        moduleLocation = moduleFilePath
      }

fromCurrentPath :: App Module
fromCurrentPath =
  getCurrentModuleFilePath >>= fromFilePath MID.Main

interpretRelFilePath :: MID.ModuleID -> (H.Hint, [Tree.Tree]) -> App SGL.StrictGlobalLocator
interpretRelFilePath moduleID pathInfo = do
  (m, pathString) <- liftEither $ Tree.extract pathInfo >>= Tree.toString
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = SL.SourceLocator relPath
          }
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

interpretExtraPath :: Path Abs Dir -> Tree.Tree -> App SomePath
interpretExtraPath moduleRootDir entity = do
  (m, itemPathText) <- liftEither $ Tree.toString entity
  if T.last itemPathText == '/'
    then do
      dirPath <- Path.resolveDir moduleRootDir $ T.unpack itemPathText
      ensureExistence m moduleRootDir dirPath Path.doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- Path.resolveFile moduleRootDir $ T.unpack itemPathText
      ensureExistence m moduleRootDir filePath Path.doesFileExist "file"
      return $ Right filePath

interpretAntecedent :: Tree.Tree -> App ModuleDigest
interpretAntecedent ens = do
  (_, digestText) <- liftEither $ Tree.toString ens
  return $ ModuleDigest digestText

ensureExistence ::
  H.Hint ->
  Path Abs Dir ->
  Path Abs t ->
  (Path Abs t -> App Bool) ->
  T.Text ->
  App ()
ensureExistence m moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker path
  unless b $ do
    relPathFromModuleRoot <- Path.stripPrefix moduleRootDir path
    raiseError m $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath relPathFromModuleRoot)

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
