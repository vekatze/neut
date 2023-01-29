{-# LANGUAGE TemplateHaskell #-}

module Case.Main.Path
  ( getLibraryDirPath,
    ensureNotInLibDir,
    Context,
  )
where

import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Path
import Path.IO

class
  ( Throw.Context m,
    MonadIO m
  ) =>
  Context m

getLibraryDirPath :: Context m => m (Path Abs Dir)
getLibraryDirPath = do
  basePath <- getCacheDirPath
  returnDirectory $ basePath </> $(mkRelDir "library")

getCacheDirPath :: Context m => m (Path Abs Dir)
getCacheDirPath = do
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
