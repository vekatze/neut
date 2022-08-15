module Entity.Module.Reflect
  ( fromFilePath,
    fromCurrentPath,
  )
where

import qualified Context.Path as Path
import Context.Throw
import Control.Monad
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import Entity.Ens
import qualified Entity.Ens.Reflect as Ens
import qualified Entity.Hint as H
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import qualified Entity.ModuleID as MID
import Entity.ModuleURL
import qualified Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import Entity.Target
import Path
import qualified Scene.Parse.Core as Parse

fromFilePath :: (Parse.Context m, Path.Context m, Context m) => MID.ModuleID -> Path Abs File -> m Module
fromFilePath moduleID moduleFilePath = do
  entity <- Ens.fromFilePath moduleFilePath
  (_, entryPointEns) <- access "target" entity >>= toDictionary
  dependencyEns <- access "dependency" entity >>= toDictionary
  extraContentsEns <- access "extra-content" entity >>= toList
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

fromCurrentPath :: (Parse.Context m, Path.Context m, Context m) => m Module
fromCurrentPath =
  getCurrentModuleFilePath >>= fromFilePath MID.Main

interpretRelFilePath :: Context m => MID.ModuleID -> Ens -> m SGL.StrictGlobalLocator
interpretRelFilePath moduleID ens = do
  (m, pathString) <- toString ens
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
    k' <- BN.reflect m k
    (_, url) <- access "URL" ens >>= toString
    (_, checksum) <- access "checksum" ens >>= toString
    return (ModuleAlias k', (ModuleURL url, ModuleChecksum checksum))
  return $ Map.fromList items

interpretExtraPath :: (Parse.Context m, Path.Context m, Context m) => Path Abs Dir -> Ens -> m SomePath
interpretExtraPath moduleRootDir entity = do
  (m, itemPathText) <- toString entity
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
