module Entity.Module.Reflect
  ( initializeMainModule,
    fromFilePath,
  )
where

import Control.Monad
import qualified Data.Text as T
import Entity.Ens
import qualified Entity.Ens.Reflect as Ens
import Entity.Log
import Entity.Module
import Entity.ModuleChecksum
import Entity.ModuleURL
import Path
import Path.IO

fromFilePath :: Path Abs File -> IO Module
fromFilePath moduleFilePath = do
  entity <- Ens.fromFilePath moduleFilePath
  entryPointEns <- access "target" entity >>= toDictionary
  dependencyEns <- access "dependency" entity >>= toDictionary
  extraContentsEns <- access "extra-content" entity >>= toList
  target <- mapM interpretRelFilePath entryPointEns
  dependency <- mapM interpretDependency dependencyEns
  extraContents <- mapM (interpretExtraPath $ parent moduleFilePath) extraContentsEns
  return
    Module
      { moduleTarget = target,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleLocation = moduleFilePath
      }

interpretRelFilePath :: Ens -> IO (Path Rel File)
interpretRelFilePath =
  toString >=> parseRelFile . T.unpack

interpretDependency :: Ens -> IO (ModuleURL, ModuleChecksum)
interpretDependency dependencyValue = do
  url <- access "URL" dependencyValue >>= toString
  checksum <- access "checksum" dependencyValue >>= toString
  return (ModuleURL url, ModuleChecksum checksum)

interpretExtraPath :: Path Abs Dir -> Ens -> IO SomePath
interpretExtraPath moduleRootDir entity = do
  itemPathText <- toString entity
  if T.last itemPathText == '/'
    then do
      dirPath <- resolveDir moduleRootDir $ T.unpack itemPathText
      ensureExistence moduleRootDir dirPath doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- resolveFile moduleRootDir $ T.unpack itemPathText
      ensureExistence moduleRootDir filePath doesFileExist "file"
      return $ Right filePath

ensureExistence :: Path Abs Dir -> Path Abs t -> (Path Abs t -> IO Bool) -> T.Text -> IO ()
ensureExistence moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker path
  unless b $ do
    relPathFromModuleRoot <- stripProperPrefix moduleRootDir path
    raiseError' $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath relPathFromModuleRoot)

initializeMainModule :: IO ()
initializeMainModule = do
  getMainModuleFilePath >>= fromFilePath >>= setMainModule
