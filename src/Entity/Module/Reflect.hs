module Entity.Module.Reflect
  ( fromFilePath,
    fromCurrentPath,
  )
where

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
import Path.IO

fromFilePath :: Context -> MID.ModuleID -> Path Abs File -> IO Module
fromFilePath ctx moduleID moduleFilePath = do
  entity <- Ens.fromFilePath ctx moduleFilePath
  (_, entryPointEns) <- access ctx "target" entity >>= toDictionary ctx
  dependencyEns <- access ctx "dependency" entity >>= toDictionary ctx
  extraContentsEns <- access ctx "extra-content" entity >>= toList ctx
  target <- mapM (interpretRelFilePath ctx moduleID) entryPointEns
  dependency <- interpretDependencyDict ctx dependencyEns
  extraContents <- mapM (interpretExtraPath ctx $ parent moduleFilePath) extraContentsEns
  return
    Module
      { moduleTarget = Map.mapKeys Target target,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleLocation = moduleFilePath
      }

fromCurrentPath :: Context -> IO Module
fromCurrentPath ctx =
  getCurrentModuleFilePath ctx >>= fromFilePath ctx MID.Main

interpretRelFilePath :: Context -> MID.ModuleID -> Ens -> IO SGL.StrictGlobalLocator
interpretRelFilePath ctx moduleID ens = do
  (m, pathString) <- toString ctx ens
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return
        SGL.StrictGlobalLocator
          { SGL.moduleID = moduleID,
            SGL.sourceLocator = SL.SourceLocator relPath
          }
    Nothing ->
      raiseError ctx m $ "invalid file path: " <> pathString

interpretDependencyDict ::
  Context ->
  (H.Hint, Map.HashMap T.Text Ens) ->
  IO (Map.HashMap ModuleAlias (ModuleURL, ModuleChecksum))
interpretDependencyDict ctx (m, dep) = do
  items <- forM (Map.toList dep) $ \(k, ens) -> do
    k' <- BN.reflect ctx m k
    (_, url) <- access ctx "URL" ens >>= toString ctx
    (_, checksum) <- access ctx "checksum" ens >>= toString ctx
    return (ModuleAlias k', (ModuleURL url, ModuleChecksum checksum))
  return $ Map.fromList items

interpretExtraPath :: Context -> Path Abs Dir -> Ens -> IO SomePath
interpretExtraPath ctx moduleRootDir entity = do
  (m, itemPathText) <- toString ctx entity
  if T.last itemPathText == '/'
    then do
      dirPath <- resolveDir moduleRootDir $ T.unpack itemPathText
      ensureExistence ctx m moduleRootDir dirPath doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- resolveFile moduleRootDir $ T.unpack itemPathText
      ensureExistence ctx m moduleRootDir filePath doesFileExist "file"
      return $ Right filePath

ensureExistence ::
  Context ->
  H.Hint ->
  Path Abs Dir ->
  Path Abs t ->
  (Path Abs t -> IO Bool) ->
  T.Text ->
  IO ()
ensureExistence ctx m moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker path
  unless b $ do
    relPathFromModuleRoot <- stripProperPrefix moduleRootDir path
    raiseError ctx m $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath relPathFromModuleRoot)
