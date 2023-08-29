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
    setModificationTime,
    ensureDir,
    stripPrefix,
    writeByteString,
    writeText,
    getBaseName,
    parseRelFile,
    removeDirRecur,
    getExecutableOutputPath,
    getBaseBuildDir,
    getBuildDir,
    getInstallDir,
    getArtifactDir,
    getPlatformPrefix,
    sourceToOutputPath,
    getSourceCachePath,
    attachOutputPath,
  )
where

import Context.App
import Context.App.Internal
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as L
import Data.ByteString.UTF8 qualified as B
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time
import Data.Version qualified as V
import Entity.BuildMode qualified as BM
import Entity.Const
import Entity.Digest
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Platform as TP
import Entity.Source qualified as Src
import Entity.Target qualified as Target
import Path (Abs, Dir, File, Path, Rel, (</>))
import Path qualified as P
import Path.IO qualified as P
import Paths_neut
import System.Environment

getLibraryDirPath :: App (Path Abs Dir)
getLibraryDirPath = do
  cacheDirPath <- getCacheDirPath
  returnDirectory $ cacheDirPath </> $(P.mkRelDir "library")

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

setModificationTime :: Path Abs File -> UTCTime -> App ()
setModificationTime =
  P.setModificationTime

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

returnDirectory :: Path Abs Dir -> App (Path Abs Dir)
returnDirectory path =
  P.ensureDir path >> return path

getPlatformPrefix :: App (Path Rel Dir)
getPlatformPrefix = do
  P.parseRelDir $ T.unpack $ TP.reify platform

getExecutableOutputPath :: Target.Target -> Module -> App (Path Abs File)
getExecutableOutputPath target mainModule = do
  executableDir <- getExecutableDir mainModule
  resolveFile executableDir $ T.unpack $ Target.extract target

getBaseBuildDir :: Module -> App (Path Abs Dir)
getBaseBuildDir baseModule = do
  platformPrefix <- getPlatformPrefix
  versionDir <- P.parseRelDir $ "compiler-" ++ V.showVersion version
  return $ getModuleRootDir baseModule </> buildRelDir </> platformPrefix </> versionDir

getBuildDir :: Module -> App (Path Abs Dir)
getBuildDir baseModule = do
  baseBuildDir <- getBaseBuildDir baseModule
  optString <- readRef' clangOptString
  buildMode <- Env.getBuildMode
  let configString = B.fromString $ T.unpack (BM.reify buildMode) ++ " " ++ optString
  buildOptionPrefix <- P.parseRelDir $ "build-option-" ++ B.toString (hashAndEncode configString)
  return $ baseBuildDir </> buildOptionPrefix

getArtifactDir :: Module -> App (Path Abs Dir)
getArtifactDir baseModule = do
  buildDir <- getBuildDir baseModule
  return $ buildDir </> artifactRelDir

getExecutableDir :: Module -> App (Path Abs Dir)
getExecutableDir baseModule = do
  buildDir <- getBuildDir baseModule
  return $ buildDir </> executableRelDir

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

getInstallDir :: FilePath -> App (Path Abs Dir)
getInstallDir filePath = do
  path <- P.resolveDir' filePath
  ensureDir path
  return path
