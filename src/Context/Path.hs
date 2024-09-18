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
    getForeignDir,
    getInstallDir,
    getPlatformPrefix,
    sourceToOutputPath,
    getZenExecutableDir,
    getSourceCachePath,
    getSourceCompletionCachePath,
    attachOutputPath,
    getOutputPathForEntryPoint,
    getLastModifiedSup,
    getLastModifiedInf,
    unrollPath,
  )
where

import Context.App
import Context.App.Internal
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.UTF8 qualified as B
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time
import Data.Version qualified as V
import Entity.Const
import Entity.Digest
import Entity.Module
import Entity.Module qualified as M
import Entity.ModuleID qualified as MID
import Entity.OutputKind qualified as OK
import Entity.Platform as TP
import Entity.Source qualified as Src
import Entity.Target qualified as Target
import Path (Abs, Dir, File, Path, Rel, (</>))
import Path qualified as P
import Path.IO qualified as P
import Paths_neut

getCurrentDir :: App (Path Abs Dir)
getCurrentDir =
  P.getCurrentDir

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
  liftIO $ B.writeFile (P.toFilePath path) $ encodeUtf8 text

parseRelFile :: FilePath -> App (Path Rel File)
parseRelFile =
  P.parseRelFile

removeDirRecur :: Path Abs Dir -> App ()
removeDirRecur =
  P.removeDirRecur

returnDirectory :: Path Abs Dir -> App (Path Abs Dir)
returnDirectory path =
  ensureDir path >> return path

getLibraryDirPath :: App (Path Abs Dir)
getLibraryDirPath = do
  mainModule <- Env.getMainModule
  let moduleRootDir = getModuleRootDir mainModule
  returnDirectory $ moduleRootDir </> moduleCacheDir mainModule </> $(P.mkRelDir "library")

ensureNotInLibDir :: App ()
ensureNotInLibDir = do
  mainModule <- Env.getMainModule
  case moduleID mainModule of
    MID.Library _ ->
      Throw.raiseError'
        "This command cannot be used under the library directory"
    _ ->
      return ()

getPlatformPrefix :: App (Path Rel Dir)
getPlatformPrefix = do
  P.parseRelDir $ T.unpack $ TP.reify platform

getExecutableOutputPath :: Target.MainTarget -> Module -> App (Path Abs File)
getExecutableOutputPath targetOrZen mainModule = do
  case targetOrZen of
    Target.Named target _ -> do
      executableDir <- getExecutableDir mainModule
      resolveFile executableDir $ T.unpack target
    Target.Zen path _ _ -> do
      zenExecutableDir <- getZenExecutableDir mainModule
      relPath <- getRelPathFromSourceDir mainModule path
      (relPathWithoutExtension, _) <- P.splitExtension relPath
      return $ zenExecutableDir </> relPathWithoutExtension

getBaseBuildDir :: Module -> App (Path Abs Dir)
getBaseBuildDir baseModule = do
  platformPrefix <- getPlatformPrefix
  versionDir <- P.parseRelDir $ "compiler-" ++ V.showVersion version
  let moduleRootDir = getModuleRootDir baseModule
  return $ moduleRootDir </> moduleCacheDir baseModule </> platformPrefix </> versionDir

getBuildDir :: Module -> App (Path Abs Dir)
getBuildDir baseModule = do
  baseBuildDir <- getBaseBuildDir baseModule
  buildSignature <- getBuildSignature
  buildPrefix <- P.parseRelDir $ "build-" ++ buildSignature
  return $ baseBuildDir </> buildPrefix

getBuildSignature :: App String
getBuildSignature = do
  sigCache <- readRef' buildSignatureCache
  case sigCache of
    Just sig -> do
      return sig
    Nothing -> do
      mainModule <- Env.getMainModule
      sig <- fmap (B.toString . hashAndEncode) $ liftIO $ B.readFile $ P.toFilePath $ moduleLocation mainModule
      modifyRef' buildSignatureCache $ const $ Just sig
      return sig

getArtifactDir :: Module -> App (Path Abs Dir)
getArtifactDir baseModule = do
  buildDir <- getBuildDir baseModule
  return $ buildDir </> artifactRelDir

getForeignDir :: Module -> App (Path Abs Dir)
getForeignDir baseModule = do
  buildDir <- getBuildDir baseModule
  let foreignDir = buildDir </> foreignRelDir
  ensureDir foreignDir
  return foreignDir

getEntryDir :: Module -> App (Path Abs Dir)
getEntryDir baseModule = do
  buildDir <- getBuildDir baseModule
  return $ buildDir </> entryRelDir

getExecutableDir :: Module -> App (Path Abs Dir)
getExecutableDir baseModule = do
  buildDir <- getBuildDir baseModule
  return $ buildDir </> executableRelDir

getZenExecutableDir :: Module -> App (Path Abs Dir)
getZenExecutableDir baseModule = do
  buildDir <- getBuildDir baseModule
  return $ buildDir </> zenRelDir </> executableRelDir

getZenEntryDir :: Module -> App (Path Abs Dir)
getZenEntryDir baseModule = do
  buildDir <- getBuildDir baseModule
  return $ buildDir </> zenRelDir </> entryRelDir

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

getSourceCompletionCachePath :: Src.Source -> App (Path Abs File)
getSourceCompletionCachePath source = do
  artifactDir <- getArtifactDir $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  P.addExtension ".ic" (artifactDir </> relPathWithoutExtension)

attachOutputPath :: OK.OutputKind -> Src.Source -> App (OK.OutputKind, Path Abs File)
attachOutputPath outputKind source = do
  outputPath <- sourceToOutputPath outputKind source
  return (outputKind, outputPath)

getOutputPathForEntryPoint :: Module -> OK.OutputKind -> Target.MainTarget -> App (OK.OutputKind, Path Abs File)
getOutputPathForEntryPoint baseModule kind mainTarget = do
  case mainTarget of
    Target.Named target _ -> do
      entryDir <- getEntryDir baseModule
      relPath <- parseRelFile $ T.unpack target
      outputPath <- Src.attachExtension (entryDir </> relPath) kind
      return (kind, outputPath)
    Target.Zen path _ _ -> do
      zenEntryDir <- getZenEntryDir baseModule
      relPath <- getRelPathFromSourceDir baseModule path
      (relPathWithoutExtension, _) <- P.splitExtension relPath
      outputPath <- Src.attachExtension (zenEntryDir </> relPathWithoutExtension) kind
      return (kind, outputPath)

getInstallDir :: FilePath -> App (Path Abs Dir)
getInstallDir filePath = do
  path <- P.resolveDir' filePath
  ensureDir path
  return path

getLastModifiedSup :: [Path Abs File] -> App (Maybe UTCTime)
getLastModifiedSup pathList =
  case pathList of
    [] ->
      return Nothing
    [path] -> do
      b <- doesFileExist path
      if b
        then Just <$> getModificationTime path
        else return Nothing
    path : pathList' -> do
      b <- doesFileExist path
      if b
        then do
          t1 <- getModificationTime path
          t2 <- getLastModifiedSup pathList'
          if Just t1 > t2
            then return $ Just t1
            else return t2
        else do
          return Nothing

getLastModifiedInf :: [Path Abs File] -> App (Maybe UTCTime)
getLastModifiedInf pathList =
  case pathList of
    [] ->
      return Nothing
    [path] -> do
      b <- doesFileExist path
      if b
        then Just <$> getModificationTime path
        else return Nothing
    path : pathList' -> do
      b <- doesFileExist path
      if b
        then do
          t1 <- getModificationTime path
          t2 <- getLastModifiedInf pathList'
          if Just t1 < t2
            then return $ Just t1
            else return t2
        else do
          return Nothing

unrollPath :: M.SomePath Abs -> App [Path Abs File]
unrollPath path =
  case path of
    Left dirPath -> do
      (_, filePathList) <- P.listDirRecur dirPath
      return filePathList
    Right filePath ->
      return [filePath]
