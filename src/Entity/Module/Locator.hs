module Entity.Module.Locator (getNextModule) where

import Context.Throw
import Control.Monad
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Global
import Entity.Hint
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Path
import Path.IO
import System.IO.Unsafe

getNextModule :: Context -> Hint -> Module -> ModuleAlias -> IO Module
getNextModule ctx m currentModule nextModuleAlias = do
  nextModuleFilePath <- getNextModuleFilePath ctx m currentModule nextModuleAlias
  moduleCacheMap <- readIORef moduleCacheMapRef
  case Map.lookup nextModuleFilePath moduleCacheMap of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        raiseError ctx m $
          T.pack "could not find the module file for `"
            <> extract nextModuleAlias
            <> "`"
      nextModule <- Module.fromFilePath ctx nextModuleFilePath
      modifyIORef' moduleCacheMapRef $ Map.insert nextModuleFilePath nextModule
      return nextModule

getNextModuleFilePath :: Context -> Hint -> Module -> ModuleAlias -> IO (Path Abs File)
getNextModuleFilePath ctx m currentModule nextModuleAlias = do
  moduleDirPath <- getNextModuleDirPath ctx m currentModule nextModuleAlias
  return $ moduleDirPath </> moduleFile

getNextModuleDirPath :: Context -> Hint -> Module -> ModuleAlias -> IO (Path Abs Dir)
getNextModuleDirPath ctx m currentModule nextModuleAlias =
  if nextModuleAlias == ModuleAlias defaultModulePrefix
    then getCurrentFilePath ctx >>= filePathToModuleFileDir ctx
    else do
      ModuleChecksum checksum <- resolveModuleAliasIntoModuleName ctx m currentModule nextModuleAlias
      libraryDir <- getLibraryDirPath
      resolveDir libraryDir $ T.unpack checksum

{-# NOINLINE moduleCacheMapRef #-}
moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module)
moduleCacheMapRef =
  unsafePerformIO (newIORef Map.empty)

filePathToModuleFilePath :: Context -> Path Abs File -> IO (Path Abs File)
filePathToModuleFilePath ctx filePath = do
  findModuleFile ctx $ parent filePath

filePathToModuleFileDir :: Context -> Path Abs File -> IO (Path Abs Dir)
filePathToModuleFileDir ctx filePath =
  parent <$> filePathToModuleFilePath ctx filePath

resolveModuleAliasIntoModuleName :: Context -> Hint -> Module -> ModuleAlias -> IO ModuleChecksum
resolveModuleAliasIntoModuleName ctx m currentModule (ModuleAlias nextModuleAlias) =
  case Map.lookup nextModuleAlias (moduleDependency currentModule) of
    Just (_, checksum) ->
      return checksum
    Nothing ->
      raiseError ctx m $ "no such module alias is defined: " <> nextModuleAlias

-- getNextSource :: Hint -> Module -> ModuleAlias -> IO Source
-- getNextSource m currentModule nextModuleAlias = do
--   -- sig@(nextModuleName, _, _) <- parseModuleInfo m sigText
--   newModule <- getNextModule m currentModule nextModuleAlias
--   filePath <- getSourceFilePath newModule sig
--   return $
--     Source
--       { sourceModule = newModule,
--         sourceFilePath = filePath
--       }

-- getSourceFilePath :: Module -> SourceSignature -> IO (Path Abs File)
-- getSourceFilePath baseModule (_, locator, name) = do
--   resolveFile (getSourceDir baseModule) (sectionToPath $ locator ++ [name])

-- sourceSignatureは、(ModuleAlias, [DirPath], FileName) になってるのか。
-- sectionToPath :: [T.Text] -> FilePath
-- sectionToPath sectionPath =
--   T.unpack $ T.intercalate (T.singleton FP.pathSeparator) sectionPath <> "." <> sourceFileExtension
