{-# LANGUAGE TemplateHaskell #-}

module Context.Path.Main (new) where

import qualified Context.Path as Path
import Path
import Path.IO

new :: Path.Config -> IO Path.Context
new _ =
  return $
    Path.Context
      { Path.getLibraryDirPath =
          getLibraryDirPath
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
