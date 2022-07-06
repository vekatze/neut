module Entity.Module.Locator (getNextModule) where

import qualified Context.Throw as Throw
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Global
import Entity.Hint
import Entity.Module
import qualified Entity.Module.Reflect as Module
import Entity.ModuleAlias
import Entity.ModuleChecksum
import qualified Entity.ModuleID as MID
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
  moduleID <- resolveAlias ctx m currentModule nextModuleAlias
  case moduleID of
    MID.This ->
      return $ moduleLocation currentModule
    MID.That (ModuleChecksum checksum) -> do
      libraryDir <- getLibraryDirPath
      moduleDir <- resolveDir libraryDir $ T.unpack checksum
      return $ moduleDir </> moduleFile

{-# NOINLINE moduleCacheMapRef #-}
moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module)
moduleCacheMapRef =
  unsafePerformIO (newIORef Map.empty)

resolveAlias :: Throw.Context -> Hint -> Module -> ModuleAlias -> IO MID.ModuleID
resolveAlias ctx m currentModule nextModuleAlias =
  if nextModuleAlias == ModuleAlias defaultModulePrefix
    then return MID.This
    else do
      case Map.lookup nextModuleAlias (moduleDependency currentModule) of
        Just (_, checksum) ->
          return $ MID.That checksum
        Nothing ->
          Throw.raiseError ctx m $ "no such module alias is defined: " <> extract nextModuleAlias
