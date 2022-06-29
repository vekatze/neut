module Entity.Module.Locator (getNextModule) where

import Context.App
import qualified Context.Throw as Throw
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

getNextModule :: Axis -> Hint -> Module -> ModuleAlias -> IO Module
getNextModule axis m currentModule nextModuleAlias = do
  nextModuleFilePath <- getNextModuleFilePath axis m currentModule nextModuleAlias
  moduleCacheMap <- readIORef moduleCacheMapRef
  case Map.lookup nextModuleFilePath moduleCacheMap of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        (axis & throw & Throw.raiseError) m $
          T.pack "could not find the module file for `"
            <> extract nextModuleAlias
            <> "`"
      nextModule <- Module.fromFilePath axis nextModuleFilePath
      modifyIORef' moduleCacheMapRef $ Map.insert nextModuleFilePath nextModule
      return nextModule

getNextModuleFilePath :: Axis -> Hint -> Module -> ModuleAlias -> IO (Path Abs File)
getNextModuleFilePath axis m currentModule nextModuleAlias = do
  moduleDirPath <- getNextModuleDirPath axis m currentModule nextModuleAlias
  return $ moduleDirPath </> moduleFile

getNextModuleDirPath :: Axis -> Hint -> Module -> ModuleAlias -> IO (Path Abs Dir)
getNextModuleDirPath axis m currentModule nextModuleAlias =
  if nextModuleAlias == ModuleAlias defaultModulePrefix
    then getCurrentFilePath axis >>= filePathToModuleFileDir axis
    else do
      ModuleChecksum checksum <- resolveModuleAliasIntoModuleName axis m currentModule nextModuleAlias
      libraryDir <- getLibraryDirPath
      resolveDir libraryDir $ T.unpack checksum

{-# NOINLINE moduleCacheMapRef #-}
moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module)
moduleCacheMapRef =
  unsafePerformIO (newIORef Map.empty)

filePathToModuleFilePath :: Axis -> Path Abs File -> IO (Path Abs File)
filePathToModuleFilePath axis filePath = do
  findModuleFile axis $ parent filePath

filePathToModuleFileDir :: Axis -> Path Abs File -> IO (Path Abs Dir)
filePathToModuleFileDir axis filePath =
  parent <$> filePathToModuleFilePath axis filePath

resolveModuleAliasIntoModuleName :: Axis -> Hint -> Module -> ModuleAlias -> IO ModuleChecksum
resolveModuleAliasIntoModuleName axis m currentModule (ModuleAlias nextModuleAlias) =
  case Map.lookup nextModuleAlias (moduleDependency currentModule) of
    Just (_, checksum) ->
      return checksum
    Nothing ->
      (axis & throw & Throw.raiseError) m $ "no such module alias is defined: " <> nextModuleAlias

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
