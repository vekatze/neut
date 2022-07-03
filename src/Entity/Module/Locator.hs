module Entity.Module.Locator (getNextModule) where

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

getNextModule :: Throw.Context -> Hint -> Module -> ModuleAlias -> IO Module
getNextModule ctx m currentModule nextModuleAlias = do
  nextModuleFilePath <- getNextModuleFilePath ctx m currentModule nextModuleAlias
  moduleCacheMap <- readIORef moduleCacheMapRef
  case Map.lookup nextModuleFilePath moduleCacheMap of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        Throw.raiseError ctx m $
          T.pack "could not find the module file for `"
            <> extract nextModuleAlias
            <> "`"
      nextModule <- Module.fromFilePath ctx nextModuleFilePath
      modifyIORef' moduleCacheMapRef $ Map.insert nextModuleFilePath nextModule
      return nextModule

getNextModuleFilePath :: Throw.Context -> Hint -> Module -> ModuleAlias -> IO (Path Abs File)
getNextModuleFilePath ctx m currentModule nextModuleAlias = do
  moduleDirPath <- getNextModuleDirPath ctx m currentModule nextModuleAlias
  return $ moduleDirPath </> moduleFile

getNextModuleDirPath :: Throw.Context -> Hint -> Module -> ModuleAlias -> IO (Path Abs Dir)
getNextModuleDirPath ctx m currentModule nextModuleAlias = do
  if nextModuleAlias == ModuleAlias defaultModulePrefix
    then return $ getModuleFileDir currentModule
    else do
      ModuleChecksum checksum <- resolveModuleAliasIntoModuleName ctx m currentModule nextModuleAlias
      libraryDir <- getLibraryDirPath
      resolveDir libraryDir $ T.unpack checksum

{-# NOINLINE moduleCacheMapRef #-}
moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module)
moduleCacheMapRef =
  unsafePerformIO (newIORef Map.empty)

getModuleFileDir :: Module -> Path Abs Dir
getModuleFileDir currentModule =
  parent (moduleLocation currentModule)

resolveModuleAliasIntoModuleName :: Throw.Context -> Hint -> Module -> ModuleAlias -> IO ModuleChecksum
resolveModuleAliasIntoModuleName ctx m currentModule (ModuleAlias nextModuleAlias) =
  case Map.lookup nextModuleAlias (moduleDependency currentModule) of
    Just (_, checksum) ->
      return checksum
    Nothing ->
      Throw.raiseError ctx m $ "no such module alias is defined: " <> nextModuleAlias
