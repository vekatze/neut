{-# LANGUAGE TemplateHaskell #-}

module Context.Path.Main (new) where

import qualified Context.Path as Path
import qualified Data.Text as T
import Entity.Module
import Entity.ModuleChecksum
import Path
import Path.IO

new :: Path.Config -> IO Path.Context
new _ =
  return $
    Path.Context
      { Path.getLibraryDirPath =
          getLibraryDirPath,
        Path.getModuleDirPath =
          getModuleDirPath,
        Path.getLibraryModuleFilePath =
          getLibraryModuleFilePath
      }

getLibraryDirPath :: IO (Path Abs Dir)
getLibraryDirPath = do
  basePath <- getCacheDirPath
  returnDirectory $ basePath </> $(mkRelDir "library")

getCacheDirPath :: IO (Path Abs Dir)
getCacheDirPath = do
  getXdgDir XdgCache (Just $(mkRelDir "neut")) >>= returnDirectory

returnDirectory :: Path Abs Dir -> IO (Path Abs Dir)
returnDirectory path =
  ensureDir path >> return path

getModuleDirPath :: ModuleChecksum -> IO (Path Abs Dir)
getModuleDirPath (ModuleChecksum checksum) = do
  libDir <- getLibraryDirPath
  resolveDir libDir $ T.unpack checksum

getLibraryModuleFilePath :: ModuleChecksum -> IO (Path Abs File)
getLibraryModuleFilePath checksum = do
  moduleDir <- getModuleDirPath checksum
  return $ moduleDir </> moduleFile
