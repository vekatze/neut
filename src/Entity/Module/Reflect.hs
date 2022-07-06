module Entity.Module.Reflect
  ( fromFilePath,
    fromCurrentPath,
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
fromFilePath ctx moduleFilePath = do
  entity <- Ens.fromFilePath ctx moduleFilePath
  entryPointEns <- access ctx "target" entity >>= toDictionary ctx
  dependencyEns <- access ctx "dependency" entity >>= toDictionary ctx
  extraContentsEns <- access ctx "extra-content" entity >>= toList ctx
  target <- mapM (interpretRelFilePath ctx) entryPointEns
  dependency <- mapM (interpretDependency ctx) dependencyEns
  extraContents <- mapM (interpretExtraPath ctx $ parent moduleFilePath) extraContentsEns
  return
    Module
      { moduleTarget = target,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleLocation = moduleFilePath
      }

fromCurrentPath :: Context -> IO Module
fromCurrentPath ctx =
  getCurrentModuleFilePath ctx >>= fromFilePath ctx

interpretRelFilePath :: Context -> Ens -> IO (Path Rel File)
interpretRelFilePath ctx =
  toString ctx >=> parseRelFile . T.unpack

interpretDependency :: Context -> Ens -> IO (ModuleURL, ModuleChecksum)
interpretDependency ctx dependencyValue = do
  url <- access ctx "URL" dependencyValue >>= toString ctx
  checksum <- access ctx "checksum" dependencyValue >>= toString ctx
  return (ModuleURL url, ModuleChecksum checksum)

interpretExtraPath :: Context -> Path Abs Dir -> Ens -> IO SomePath
interpretExtraPath ctx moduleRootDir entity = do
  itemPathText <- toString ctx entity
  if T.last itemPathText == '/'
    then do
      dirPath <- resolveDir moduleRootDir $ T.unpack itemPathText
      ensureExistence ctx moduleRootDir dirPath doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- resolveFile moduleRootDir $ T.unpack itemPathText
      ensureExistence ctx moduleRootDir filePath doesFileExist "file"
      return $ Right filePath

ensureExistence :: Context -> Path Abs Dir -> Path Abs t -> (Path Abs t -> IO Bool) -> T.Text -> IO ()
ensureExistence ctx moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker path
  unless b $ do
    relPathFromModuleRoot <- stripProperPrefix moduleRootDir path
    raiseError' ctx $ "no such " <> kindText <> " exists: " <> T.pack (toFilePath relPathFromModuleRoot)
