{-# LANGUAGE TemplateHaskell #-}

module Context.Path
  ( getLibraryDirPath,
    getCurrentDir,
    ensureNotInLibDir,
    resolveDir,
    resolveFile,
    doesDirExist,
    doesFileExist,
    getModificationTime,
    ensureDir,
    stripPrefix,
    writeByteString,
    writeText,
    getBaseName,
    parseRelFile,
    removeDirRecur,
    getExecutableOutputPath,
    getTargetDir,
    getArtifactDir,
    sourceToOutputPath,
    getSourceCachePath,
    attachOutputPath,
  )
where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time
import Data.Version qualified as V
import Entity.Const
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Src
import Entity.Target qualified as Target
import Entity.TargetPlatform as TP
import Path (Abs, Dir, File, Path, Rel, (</>))
import Path qualified as P
import Path.IO qualified as P
import Paths_neut
import System.Environment

getLibraryDirPath :: App (Path Abs Dir)
getLibraryDirPath = do
  cacheDirPath <- getCacheDirPath
  relLibDirPath <- getLibDirRelPath
  returnDirectory $ cacheDirPath </> relLibDirPath

getCurrentDir :: App (Path Abs Dir)
getCurrentDir =
  P.getCurrentDir

ensureNotInLibDir :: App ()
ensureNotInLibDir = do
  currentDir <- getCurrentDir
  libDir <- getLibraryDirPath
  when (P.isProperPrefixOf libDir currentDir) $
    Throw.raiseError'
      "this command cannot be used under the library directory"

resolveDir :: Path Abs Dir -> FilePath -> App (Path Abs Dir)
resolveDir =
  P.resolveDir

resolveFile :: Path Abs Dir -> FilePath -> App (Path Abs File)
resolveFile =
  P.resolveFile

doesDirExist :: Path Abs Dir -> App Bool
doesDirExist =
  P.doesDirExist

doesFileExist :: Path Abs File -> App Bool
doesFileExist =
  P.doesFileExist

getModificationTime :: Path Abs File -> App UTCTime
getModificationTime =
  P.getModificationTime

ensureDir :: Path Abs Dir -> App ()
ensureDir =
  P.ensureDir

getBaseName :: Path Abs File -> App T.Text
getBaseName path = do
  let dirPath = P.parent path
  filename <- P.stripProperPrefix dirPath path
  return $ T.replace packageFileExtension "" $ T.pack $ P.toFilePath filename

stripPrefix :: Path b Dir -> Path b t -> App (Path Rel t)
stripPrefix =
  P.stripProperPrefix

writeByteString :: Path Abs File -> L.ByteString -> App ()
writeByteString path content =
  liftIO $ L.writeFile (P.toFilePath path) content

writeText :: Path Abs File -> T.Text -> App ()
writeText path text =
  liftIO $ TIO.writeFile (P.toFilePath path) text

parseRelFile :: FilePath -> App (Path Rel File)
parseRelFile =
  P.parseRelFile

removeDirRecur :: Path Abs Dir -> App ()
removeDirRecur =
  P.removeDirRecur

getCacheDirPath :: App (Path Abs Dir)
getCacheDirPath = do
  mCacheDirPathString <- liftIO $ lookupEnv envVarCacheDir
  case mCacheDirPathString of
    Just cacheDirPathString -> do
      P.parseAbsDir cacheDirPathString >>= returnDirectory
    Nothing ->
      P.getXdgDir P.XdgCache (Just $(P.mkRelDir "neut")) >>= returnDirectory

getLibDirRelPath :: App (Path Rel Dir)
getLibDirRelPath = do
  prefix <- getPlatformPrefix
  return $ prefix </> $(P.mkRelDir "library")

returnDirectory :: Path Abs Dir -> App (Path Abs Dir)
returnDirectory path =
  P.ensureDir path >> return path

getPlatformPrefix :: App (Path Rel Dir)
getPlatformPrefix = do
  tp <- readRef "targetPlatform" targetPlatform
  platformDir <- P.parseRelDir $ TP.platform tp
  versionDir <- P.parseRelDir $ V.showVersion version
  return $ platformDir </> versionDir

getExecutableOutputPath :: Target.Target -> Module -> App (Path Abs File)
getExecutableOutputPath target mainModule = do
  executableDir <- getExecutableDir mainModule
  resolveFile executableDir $ T.unpack $ Target.extract target

getTargetDir :: Module -> App (Path Abs Dir)
getTargetDir baseModule = do
  prefix <- getPlatformPrefix
  return $ getModuleRootDir baseModule </> targetRelDir </> prefix

getArtifactDir :: Module -> App (Path Abs Dir)
getArtifactDir baseModule = do
  targetDir <- getTargetDir baseModule
  return $ targetDir </> artifactRelDir

getExecutableDir :: Module -> App (Path Abs Dir)
getExecutableDir baseModule = do
  targetDir <- getTargetDir baseModule
  return $ targetDir </> executableRelDir

sourceToOutputPath :: OK.OutputKind -> Src.Source -> App (Path Abs File)
sourceToOutputPath kind source = do
  artifactDir <- getArtifactDir $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  Src.attachExtension (artifactDir </> relPathWithoutExtension) kind

getSourceCachePath :: Src.Source -> App (Path Abs File)
getSourceCachePath source = do
  artifactDir <- getArtifactDir $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  P.addExtension ".i" (artifactDir </> relPathWithoutExtension)

attachOutputPath :: OK.OutputKind -> Src.Source -> App (OK.OutputKind, Path Abs File)
attachOutputPath outputKind source = do
  outputPath <- sourceToOutputPath outputKind source
  return (outputKind, outputPath)
