{-# LANGUAGE TemplateHaskell #-}

module Context.Case.Main.Path
  ( getLibraryDirPath,
    ensureNotInLibDir,
    Context,
  )
where

import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Version qualified as V
import Entity.Const
import Entity.TargetPlatform as TP
import Path
import Path.IO
import Paths_neut
import System.Environment

class
  ( Throw.Context m,
    MonadThrow m,
    Env.Context m,
    MonadIO m
  ) =>
  Context m

getLibraryDirPath :: Context m => m (Path Abs Dir)
getLibraryDirPath = do
  cacheDirPath <- getCacheDirPath
  relLibDirPath <- getLibDirRelPath
  returnDirectory $ cacheDirPath </> relLibDirPath

getLibDirRelPath :: Context m => m (Path Rel Dir)
getLibDirRelPath = do
  prefix <- getCacheDirPrefix
  return $ prefix </> $(mkRelDir "library")

getCacheDirPrefix :: Context m => m (Path Rel Dir)
getCacheDirPrefix = do
  tp <- Env.getTargetPlatform
  platformDir <- parseRelDir $ TP.platform tp
  versionDir <- parseRelDir $ V.showVersion version
  return $ platformDir </> versionDir

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
