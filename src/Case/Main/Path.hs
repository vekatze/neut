{-# LANGUAGE TemplateHaskell #-}

module Case.Main.Path
  ( getLibraryDirPath,
    getCacheDirPath,
    ensureNotInLibDir,
    Context,
  )
where

import qualified Context.Throw as Throw
import Control.Monad
import Control.Monad.IO.Class
import Path
import Path.IO

class
  ( Throw.Context m,
    MonadIO m
  ) =>
  Context m

-- class Monad m => Context m where
--   getLibraryDirPath :: m (Path Abs Dir)
--   getCurrentDir :: m (Path Abs Dir)
--   ensureNotInLibDir :: m ()
--   resolveDir :: Path Abs Dir -> FilePath -> m (Path Abs Dir)
--   resolveFile :: Path Abs Dir -> FilePath -> m (Path Abs File)
--   doesDirExist :: Path Abs Dir -> m Bool
--   doesFileExist :: Path Abs File -> m Bool
--   ensureDir :: Path Abs Dir -> m ()
--   stripPrefix :: Path b Dir -> Path b t -> m (Path Rel t)
--   writeByteString :: Path Abs File -> L.ByteString -> m ()
--   parseRelFile :: FilePath -> m (Path Rel File)
--   removeDirRecur :: Path Abs Dir -> m ()

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
