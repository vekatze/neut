module Entity.Module.Reflect
  ( initializeMainModule,
    fromFilePath,
  )
where

import Context.Throw
import Control.Monad
import qualified Data.Text as T
import Entity.Ens
import qualified Entity.Ens.Reflect as Ens
import Entity.Module
import Entity.ModuleChecksum
import Entity.ModuleURL
import Path
import Path.IO

fromFilePath :: Context -> Path Abs File -> IO Module
fromFilePath context moduleFilePath = do
  entity <- Ens.fromFilePath context moduleFilePath
  entryPointEns <- access context "target" entity >>= toDictionary context
  dependencyEns <- access context "dependency" entity >>= toDictionary context
  extraContentsEns <- access context "extra-content" entity >>= toList context
  target <- mapM (interpretRelFilePath context) entryPointEns
  dependency <- mapM (interpretDependency context) dependencyEns
  extraContents <- mapM (interpretExtraPath context $ parent moduleFilePath) extraContentsEns
  return
    Module
      { moduleTarget = target,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleLocation = moduleFilePath
      }

interpretRelFilePath :: Context -> Ens -> IO (Path Rel File)
interpretRelFilePath context =
  toString context >=> parseRelFile . T.unpack

interpretDependency :: Context -> Ens -> IO (ModuleURL, ModuleChecksum)
interpretDependency context dependencyValue = do
  url <- access context "URL" dependencyValue >>= toString context
  checksum <- access context "checksum" dependencyValue >>= toString context
  return (ModuleURL url, ModuleChecksum checksum)

interpretExtraPath :: Context -> Path Abs Dir -> Ens -> IO SomePath
interpretExtraPath context moduleRootDir entity = do
  itemPathText <- toString context entity
  if T.last itemPathText == '/'
    then do
      dirPath <- resolveDir moduleRootDir $ T.unpack itemPathText
      ensureExistence context moduleRootDir dirPath doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- resolveFile moduleRootDir $ T.unpack itemPathText
      ensureExistence context moduleRootDir filePath doesFileExist "file"
      return $ Right filePath

ensureExistence :: Context -> Path Abs Dir -> Path Abs t -> (Path Abs t -> IO Bool) -> T.Text -> IO ()
ensureExistence context moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker path
  unless b $ do
    relPathFromModuleRoot <- stripProperPrefix moduleRootDir path
    raiseError' context $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath relPathFromModuleRoot)

initializeMainModule :: Context -> IO ()
initializeMainModule context = do
  getMainModuleFilePath context >>= fromFilePath context >>= setMainModule context
