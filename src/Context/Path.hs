{-# LANGUAGE TemplateHaskell #-}

module Context.Path
  ( getLibraryDirPath,
    getCurrentDir,
    ensureNotInLibDir,
    inLibDir,
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

import Context.Antecedent qualified as Antecedent
import Context.App
import Context.App.Internal
import Context.Env qualified as Env
import Context.External (getClangDigest)
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.UTF8 qualified as B
import Data.HashMap.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time
import Data.Version qualified as V
import Entity.BuildMode qualified as BM
import Entity.Const
import Entity.Digest
import Entity.Ens qualified as E
import Entity.Ens.Reify qualified as E
import Entity.Module
import Entity.Module qualified as M
import Entity.ModuleAlias qualified as MA
import Entity.ModuleID qualified as MID
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
  b <- inLibDir
  when b $
    Throw.raiseError'
      "This command cannot be used under the library directory"

inLibDir :: App Bool
inLibDir = do
  currentDir <- getCurrentDir
  libDir <- getLibraryDirPath
  return $ P.isProperPrefixOf libDir currentDir

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

getExecutableOutputPath :: Target.ConcreteTarget -> Module -> App (Path Abs File)
getExecutableOutputPath targetOrZen mainModule = do
  case targetOrZen of
    Target.Named target _ -> do
      executableDir <- getExecutableDir (Target.Concrete targetOrZen) mainModule
      resolveFile executableDir $ T.unpack target
    Target.Zen path _ _ -> do
      zenExecutableDir <- getZenExecutableDir (Target.Concrete targetOrZen) mainModule
      relPath <- getRelPathFromSourceDir mainModule path
      (relPathWithoutExtension, _) <- P.splitExtension relPath
      return $ zenExecutableDir </> relPathWithoutExtension

getBaseBuildDir :: Module -> App (Path Abs Dir)
getBaseBuildDir baseModule = do
  platformPrefix <- getPlatformPrefix
  versionDir <- P.parseRelDir $ "compiler-" ++ V.showVersion version
  let moduleRootDir = getModuleRootDir baseModule
  return $ moduleRootDir </> moduleBuildDir baseModule </> platformPrefix </> versionDir

getBuildDir :: Target.Target -> Module -> App (Path Abs Dir)
getBuildDir target baseModule = do
  baseBuildDir <- getBaseBuildDir baseModule
  buildSignature <- getBuildSignature target baseModule
  buildPrefix <- P.parseRelDir $ "build-" ++ buildSignature
  return $ baseBuildDir </> buildPrefix

getBuildSignature :: Target.Target -> Module -> App String
getBuildSignature target baseModule = do
  sigMap <- readRef' buildSignatureMap
  case Map.lookup (moduleID baseModule) sigMap of
    Just sig -> do
      return sig
    Nothing -> do
      buildMode <- Env.getBuildMode
      let depList = map (second dependencyDigest) $ Map.toList $ moduleDependency baseModule
      depList' <- fmap catMaybes $ forM depList $ \(alias, digest) -> do
        shiftedDigestOrNone <- Antecedent.lookup digest
        case shiftedDigestOrNone of
          Nothing ->
            return Nothing
          Just shiftedModule ->
            return $ Just (MA.reify alias, _m :< E.String (MID.reify $ moduleID shiftedModule))
      clangDigest <- getClangDigest
      let ens =
            E.dictFromList
              _m
              [ ("build-mode", _m :< E.String (BM.reify buildMode)),
                ("clang-digest", _m :< E.String clangDigest),
                ("compatible-shift", E.dictFromList _m depList'),
                ("compile-option", _m :< E.String (T.pack $ unwords $ Target.getCompileOption target)),
                ("link-option", _m :< E.String (T.pack $ unwords $ Target.getLinkOption target))
              ]
      let sig = B.toString $ hashAndEncode $ B.fromString $ T.unpack $ E.pp $ E.inject ens
      modifyRef' buildSignatureMap $ Map.insert (moduleID baseModule) sig
      return sig

getArtifactDir :: Target.Target -> Module -> App (Path Abs Dir)
getArtifactDir target baseModule = do
  buildDir <- getBuildDir target baseModule
  return $ buildDir </> artifactRelDir

getForeignDir :: Target.Target -> Module -> App (Path Abs Dir)
getForeignDir target baseModule = do
  buildDir <- getBuildDir target baseModule
  let foreignDir = buildDir </> foreignRelDir
  ensureDir foreignDir
  return foreignDir

getEntryDir :: Target.Target -> Module -> App (Path Abs Dir)
getEntryDir target baseModule = do
  buildDir <- getBuildDir target baseModule
  return $ buildDir </> entryRelDir

getExecutableDir :: Target.Target -> Module -> App (Path Abs Dir)
getExecutableDir target baseModule = do
  buildDir <- getBuildDir target baseModule
  return $ buildDir </> executableRelDir

getZenExecutableDir :: Target.Target -> Module -> App (Path Abs Dir)
getZenExecutableDir target baseModule = do
  buildDir <- getBuildDir target baseModule
  return $ buildDir </> zenRelDir </> executableRelDir

getZenEntryDir :: Target.Target -> Module -> App (Path Abs Dir)
getZenEntryDir target baseModule = do
  buildDir <- getBuildDir target baseModule
  return $ buildDir </> zenRelDir </> entryRelDir

sourceToOutputPath :: Target.Target -> OK.OutputKind -> Src.Source -> App (Path Abs File)
sourceToOutputPath target kind source = do
  artifactDir <- getArtifactDir target $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  Src.attachExtension (artifactDir </> relPathWithoutExtension) kind

getSourceCachePath :: Target.Target -> Src.Source -> App (Path Abs File)
getSourceCachePath target source = do
  artifactDir <- getArtifactDir target $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  P.addExtension ".i" (artifactDir </> relPathWithoutExtension)

getSourceCompletionCachePath :: Target.Target -> Src.Source -> App (Path Abs File)
getSourceCompletionCachePath target source = do
  artifactDir <- getArtifactDir target $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  P.addExtension ".ic" (artifactDir </> relPathWithoutExtension)

attachOutputPath :: Target.Target -> OK.OutputKind -> Src.Source -> App (OK.OutputKind, Path Abs File)
attachOutputPath target outputKind source = do
  outputPath <- sourceToOutputPath target outputKind source
  return (outputKind, outputPath)

getOutputPathForEntryPoint :: Module -> OK.OutputKind -> Target.ConcreteTarget -> App (OK.OutputKind, Path Abs File)
getOutputPathForEntryPoint baseModule kind targetOrZen = do
  case targetOrZen of
    Target.Named target _ -> do
      entryDir <- getEntryDir (Target.Concrete targetOrZen) baseModule
      relPath <- parseRelFile $ T.unpack target
      outputPath <- Src.attachExtension (entryDir </> relPath) kind
      return (kind, outputPath)
    Target.Zen path _ _ -> do
      zenEntryDir <- getZenEntryDir (Target.Concrete targetOrZen) baseModule
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
