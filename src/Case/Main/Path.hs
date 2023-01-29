{-# LANGUAGE TemplateHaskell #-}

module Case.Main.Path
  ( getLibraryDirPath,
    ensureNotInLibDir,
    Context,
  )
where

import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Entity.Const
import Path
import Path.IO
import System.Environment

class
  ( Throw.Context m,
    MonadThrow m,
    MonadIO m
  ) =>
  Context m

getLibraryDirPath :: Context m => m (Path Abs Dir)
getLibraryDirPath = do
  cacheDirPath <- getCacheDirPath
  returnDirectory $ cacheDirPath </> $(mkRelDir "library")

getCacheDirPath :: Context m => m (Path Abs Dir)
getCacheDirPath = do
  mCacheDirPathString <- liftIO $ lookupEnv envVarCacheDir
  case mCacheDirPathString of
    Just cacheDirPathString -> do
      parseAbsDir cacheDirPathString >>= returnDirectory
    Nothing ->
      getXdgDir XdgCache (Just $(mkRelDir "neut")) >>= returnDirectory

returnDirectory :: Context m => Path Abs Dir -> m (Path Abs Dir)
returnDirectory path =
  ensureDir path >> return path

ensureNotInLibDir :: Context m => m ()
ensureNotInLibDir = do
  currentDir <- getCurrentDir
  libDir <- getLibraryDirPath
  when (isProperPrefixOf libDir currentDir) $
    Throw.raiseError'
      "this command cannot be used under the library directory"
