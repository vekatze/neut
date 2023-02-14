module Scene.Module.Reflect
  ( fromFilePath,
    fromCurrentPath,
  )
where

import Context.Path qualified as Path
import Context.Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const (moduleFile)
import Entity.Ens
import Entity.Hint qualified as H
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Path
import Scene.Ens.Reflect qualified as Ens

fromFilePath :: (Path.Context m, Context m) => MID.ModuleID -> Path Abs File -> m Module
fromFilePath moduleID moduleFilePath = do
  entity <- Ens.fromFilePath moduleFilePath
  (_, entryPointEns) <- liftEither $ access "target" entity >>= toDictionary
  dependencyEns <- liftEither $ access "dependency" entity >>= toDictionary
  extraContentsEns <- liftEither $ access "extra-content" entity >>= toList
  target <- mapM (interpretRelFilePath moduleID) entryPointEns
  dependency <- interpretDependencyDict dependencyEns
  extraContents <- mapM (interpretExtraPath $ parent moduleFilePath) extraContentsEns
  return
    Module
      { moduleTarget = Map.mapKeys Target target,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleLocation = moduleFilePath
      }

fromCurrentPath :: (Path.Context m, Context m) => m Module
fromCurrentPath =
  getCurrentModuleFilePath >>= fromFilePath MID.Main

interpretRelFilePath :: Context m => MID.ModuleID -> Ens -> m SGL.StrictGlobalLocator
interpretRelFilePath moduleID ens = do
  (m, pathString) <- liftEither $ toString ens
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
  (Context m) =>
  (H.Hint, Map.HashMap T.Text Ens) ->
  m (Map.HashMap ModuleAlias (ModuleURL, ModuleChecksum))
interpretDependencyDict (m, dep) = do
  items <- forM (Map.toList dep) $ \(k, ens) -> do
    k' <- liftEither $ BN.reflect m k
    when (S.member k' BN.reservedAlias) $
      raiseError m $
        "the reserved name `"
          <> BN.reify k'
          <> "` cannot be used as an alias of a module"
    (_, url) <- liftEither $ access "URL" ens >>= toString
    (_, checksum) <- liftEither $ access "checksum" ens >>= toString
    return (ModuleAlias k', (ModuleURL url, ModuleChecksum checksum))
  return $ Map.fromList items

interpretExtraPath :: (Path.Context m, Context m) => Path Abs Dir -> Ens -> m SomePath
interpretExtraPath moduleRootDir entity = do
  (m, itemPathText) <- liftEither $ toString entity
  if T.last itemPathText == '/'
    then do
      dirPath <- Path.resolveDir moduleRootDir $ T.unpack itemPathText
      ensureExistence m moduleRootDir dirPath Path.doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- Path.resolveFile moduleRootDir $ T.unpack itemPathText
      ensureExistence m moduleRootDir filePath Path.doesFileExist "file"
      return $ Right filePath

ensureExistence ::
  (Context m, Path.Context m) =>
  H.Hint ->
  Path Abs Dir ->
  Path Abs t ->
  (Path Abs t -> m Bool) ->
  T.Text ->
  m ()
ensureExistence m moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker path
  unless b $ do
    relPathFromModuleRoot <- Path.stripPrefix moduleRootDir path
    raiseError m $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath relPathFromModuleRoot)

findModuleFile :: (Path.Context m, Context m) => Path Abs Dir -> m (Path Abs File)
findModuleFile moduleRootDirCandidate = do
  let moduleFileCandidate = moduleRootDirCandidate </> moduleFile
  moduleFileExists <- Path.doesFileExist moduleFileCandidate
  case (moduleFileExists, moduleRootDirCandidate /= parent moduleRootDirCandidate) of
    (True, _) ->
      return moduleFileCandidate
    (_, True) ->
      findModuleFile $ parent moduleRootDirCandidate
    _ ->
      raiseError' "couldn't find a module file."

getCurrentModuleFilePath :: (Path.Context m, Context m) => m (Path Abs File)
getCurrentModuleFilePath =
  Path.getCurrentDir >>= findModuleFile
