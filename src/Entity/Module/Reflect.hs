module Entity.Module.Reflect
  ( initializeMainModule,
    fromFilePath,
  )
where

import Context.App
import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.Text as T
import Entity.Ens
import qualified Entity.Ens.Reflect as Ens
import Entity.Module
import Entity.ModuleChecksum
import Entity.ModuleURL
import Path
import Path.IO

fromFilePath :: Axis -> Path Abs File -> IO Module
fromFilePath axis moduleFilePath = do
  entity <- Ens.fromFilePath moduleFilePath
  entryPointEns <- access axis "target" entity >>= toDictionary axis
  dependencyEns <- access axis "dependency" entity >>= toDictionary axis
  extraContentsEns <- access axis "extra-content" entity >>= toList axis
  target <- mapM (interpretRelFilePath axis) entryPointEns
  dependency <- mapM (interpretDependency axis) dependencyEns
  extraContents <- mapM (interpretExtraPath axis $ parent moduleFilePath) extraContentsEns
  return
    Module
      { moduleTarget = target,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleLocation = moduleFilePath
      }

interpretRelFilePath :: Axis -> Ens -> IO (Path Rel File)
interpretRelFilePath axis =
  toString axis >=> parseRelFile . T.unpack

interpretDependency :: Axis -> Ens -> IO (ModuleURL, ModuleChecksum)
interpretDependency axis dependencyValue = do
  url <- access axis "URL" dependencyValue >>= toString axis
  checksum <- access axis "checksum" dependencyValue >>= toString axis
  return (ModuleURL url, ModuleChecksum checksum)

interpretExtraPath :: Axis -> Path Abs Dir -> Ens -> IO SomePath
interpretExtraPath axis moduleRootDir entity = do
  itemPathText <- toString axis entity
  if T.last itemPathText == '/'
    then do
      dirPath <- resolveDir moduleRootDir $ T.unpack itemPathText
      ensureExistence axis moduleRootDir dirPath doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- resolveFile moduleRootDir $ T.unpack itemPathText
      ensureExistence axis moduleRootDir filePath doesFileExist "file"
      return $ Right filePath

ensureExistence :: Axis -> Path Abs Dir -> Path Abs t -> (Path Abs t -> IO Bool) -> T.Text -> IO ()
ensureExistence axis moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker path
  unless b $ do
    relPathFromModuleRoot <- stripProperPrefix moduleRootDir path
    axis & throw & Throw.raiseError' $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath relPathFromModuleRoot)

initializeMainModule :: Axis -> IO ()
initializeMainModule axis = do
  getMainModuleFilePath axis >>= fromFilePath axis >>= setMainModule axis
