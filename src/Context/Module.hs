module Context.Module
  ( getModuleFilePath,
    getModule,
    getSourcePath,
    fromFilePath,
    fromCurrentPath,
  )
where

import Context.App
import Context.App.Internal
import Context.Ens qualified as Ens
import Context.Env qualified as Env
import Context.Path qualified as Path
import Context.Throw
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const (moduleFile)
import Entity.Ens
import Entity.Hint
import Entity.Hint qualified as H
import Entity.Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleChecksum qualified as MC
import Entity.ModuleID
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Path
import Path.IO

getModuleFilePath :: Maybe H.Hint -> MID.ModuleID -> App (Path Abs File)
getModuleFilePath mHint moduleID = do
  moduleDir <- getModuleDirByID mHint moduleID
  return $ moduleDir </> moduleFile

getModule ::
  H.Hint ->
  MID.ModuleID ->
  T.Text ->
  App Module
getModule m moduleID locatorText = do
  nextModuleFilePath <- getModuleFilePath (Just m) moduleID
  mcm <- readRef' moduleCacheMap
  case Map.lookup nextModuleFilePath mcm of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        Throw.raiseError m $
          T.pack "could not find the module file for `"
            <> locatorText
            <> "`"
      nextModule <- fromFilePath moduleID nextModuleFilePath
      modifyRef' moduleCacheMap $ Map.insert nextModuleFilePath nextModule
      return nextModule

getSourcePath :: SGL.StrictGlobalLocator -> App (Path Abs File)
getSourcePath sgl = do
  moduleDir <- getModuleDirByID Nothing $ SGL.moduleID sgl
  let relPath = SL.reify $ SGL.sourceLocator sgl
  return $ moduleDir </> sourceRelDir </> relPath

getModuleDirByID :: Maybe H.Hint -> MID.ModuleID -> App (Path Abs Dir)
getModuleDirByID mHint moduleID = do
  mainModule <- readRef' mainModule
  case moduleID of
    MID.Base -> do
      let message = "the base module can't be used here"
      case mHint of
        Just hint ->
          Throw.raiseError hint message
        Nothing ->
          Throw.raiseError' message
    MID.Main ->
      return $ getModuleRootDir mainModule
    MID.Library (MC.ModuleChecksum checksum) -> do
      libraryDir <- Path.getLibraryDirPath
      resolveDir libraryDir $ T.unpack checksum

fromFilePath :: MID.ModuleID -> Path Abs File -> App Module
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

fromCurrentPath :: App Module
fromCurrentPath =
  getCurrentModuleFilePath >>= fromFilePath MID.Main

interpretRelFilePath :: MID.ModuleID -> Ens -> App SGL.StrictGlobalLocator
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

interpretDependencyDict :: (H.Hint, Map.HashMap T.Text Ens) -> App (Map.HashMap ModuleAlias (ModuleURL, ModuleChecksum))
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

interpretExtraPath :: Path Abs Dir -> Ens -> App SomePath
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
  H.Hint ->
  Path Abs Dir ->
  Path Abs t ->
  (Path Abs t -> m Bool) ->
  T.Text ->
  App ()
ensureExistence m moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker path
  unless b $ do
    relPathFromModuleRoot <- Path.stripPrefix moduleRootDir path
    raiseError m $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath relPathFromModuleRoot)

findModuleFile :: Path Abs Dir -> App (Path Abs File)
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

getCurrentModuleFilePath :: App (Path Abs File)
getCurrentModuleFilePath =
  Path.getCurrentDir >>= findModuleFile
