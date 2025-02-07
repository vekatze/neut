{-# LANGUAGE TemplateHaskell #-}

module Context.Path
  ( getDependencyDirPath,
    getCurrentDir,
    ensureNotInDependencyDir,
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
    getSourceLocationCachePath,
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
import Context.External (getClangDigest)
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.UTF8 qualified as B
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time
import Data.Version qualified as V
import Entity.ClangOption qualified as CL
import Entity.Const
import Entity.Digest
import Entity.Ens qualified as E
import Entity.Ens.Reify qualified as E
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

getDependencyDirPath :: App (Path Abs Dir)
getDependencyDirPath = do
  mainModule <- Env.getMainModule
  let moduleRootDir = getModuleRootDir mainModule
  case moduleID mainModule of
    MID.Library _ ->
      returnDirectory $ P.parent moduleRootDir
    _ -> do
      returnDirectory $ moduleRootDir </> moduleCacheDir mainModule </> $(P.mkRelDir "dependency")

ensureNotInDependencyDir :: App ()
ensureNotInDependencyDir = do
  mainModule <- Env.getMainModule
  case moduleID mainModule of
    MID.Library _ ->
      Throw.raiseError'
        "This command cannot be used under a dependency directory"
    _ ->
      return ()

getPlatformPrefix :: App (Path Rel Dir)
getPlatformPrefix = do
  p <- Env.getPlatform Nothing
  P.parseRelDir $ T.unpack $ TP.reify p

getExecutableOutputPath :: Target.MainTarget -> Module -> App (Path Abs File)
getExecutableOutputPath targetOrZen mainModule = do
  case targetOrZen of
    Target.Named target _ -> do
      executableDir <- getExecutableDir (Target.Main targetOrZen) mainModule
      resolveFile executableDir $ T.unpack target
    Target.Zen path _ -> do
      zenExecutableDir <- getZenExecutableDir (Target.Main targetOrZen) mainModule
      relPath <- getRelPathFromSourceDir mainModule path
      (relPathWithoutExtension, _) <- P.splitExtension relPath
      return $ zenExecutableDir </> relPathWithoutExtension

getBaseBuildDir :: Module -> App (Path Abs Dir)
getBaseBuildDir baseModule = do
  platformPrefix <- getPlatformPrefix
  versionDir <- P.parseRelDir $ "compiler-" ++ V.showVersion version
  let moduleRootDir = getModuleRootDir baseModule
  return $ moduleRootDir </> moduleCacheDir baseModule </> $(P.mkRelDir "build") </> platformPrefix </> versionDir

getBuildDir :: Target.Target -> Module -> App (Path Abs Dir)
getBuildDir t baseModule = do
  baseBuildDir <- getBaseBuildDir baseModule
  buildSignature <- getBuildSignature t
  buildPrefix <- P.parseRelDir $ "build-" ++ buildSignature
  return $ baseBuildDir </> buildPrefix

getBuildSignature :: Target.Target -> App String
getBuildSignature t = do
  sigCache <- readRef' buildSignatureCache
  case sigCache of
    Just sig -> do
      return sig
    Nothing -> do
      clangDigest <- getClangDigest
      mainModule <- Env.getMainModule
      clangOption <- getClangOption t mainModule
      moduleEns <- liftIO $ B.readFile $ P.toFilePath $ moduleLocation mainModule
      let moduleEns' = decodeUtf8 moduleEns
      let ens =
            E.dictFromList
              _m
              [ ("clang-digest", _m :< E.String clangDigest),
                ("compile-option", _m :< E.String (T.unwords $ CL.compileOption clangOption)),
                ("link-option", _m :< E.String (T.unwords $ CL.linkOption clangOption)),
                ("module-configuration", _m :< E.String moduleEns')
              ]
      let sig = B.toString $ hashAndEncode $ B.fromString $ T.unpack $ E.pp $ E.inject ens
      modifyRef' buildSignatureCache $ const $ Just sig
      return sig

getClangOption :: Target.Target -> Module -> App CL.ClangOption
getClangOption t baseModule =
  case t of
    Target.Main mainModule ->
      case mainModule of
        Target.Named name _ ->
          case Map.lookup name (moduleTarget baseModule) of
            Just (Target.TargetSummary {clangOption}) ->
              return clangOption
            Nothing ->
              Throw.raiseError' $ "No such target is defined: `" <> name <> "`"
        Target.Zen _ clangOption ->
          return clangOption
    Target.Peripheral ->
      return CL.empty
    Target.PeripheralSingle _ ->
      return CL.empty

getArtifactDir :: Target.Target -> Module -> App (Path Abs Dir)
getArtifactDir t baseModule = do
  buildDir <- getBuildDir t baseModule
  return $ buildDir </> artifactRelDir

getForeignDir :: Target.Target -> Module -> App (Path Abs Dir)
getForeignDir t baseModule = do
  buildDir <- getBuildDir t baseModule
  let foreignDir = buildDir </> foreignRelDir
  ensureDir foreignDir
  return foreignDir

getEntryDir :: Target.Target -> Module -> App (Path Abs Dir)
getEntryDir t baseModule = do
  buildDir <- getBuildDir t baseModule
  return $ buildDir </> entryRelDir

getExecutableDir :: Target.Target -> Module -> App (Path Abs Dir)
getExecutableDir t baseModule = do
  buildDir <- getBuildDir t baseModule
  return $ buildDir </> executableRelDir

getZenExecutableDir :: Target.Target -> Module -> App (Path Abs Dir)
getZenExecutableDir t baseModule = do
  buildDir <- getBuildDir t baseModule
  return $ buildDir </> zenRelDir </> executableRelDir

getZenEntryDir :: Target.Target -> Module -> App (Path Abs Dir)
getZenEntryDir t baseModule = do
  buildDir <- getBuildDir t baseModule
  return $ buildDir </> zenRelDir </> entryRelDir

sourceToOutputPath :: Target.Target -> OK.OutputKind -> Src.Source -> App (Path Abs File)
sourceToOutputPath t kind source = do
  artifactDir <- getArtifactDir t $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  Src.attachExtension (artifactDir </> relPathWithoutExtension) kind

getSourceCachePath :: Target.Target -> Src.Source -> App (Path Abs File)
getSourceCachePath t =
  getCachePath t ".def"

getSourceCompletionCachePath :: Target.Target -> Src.Source -> App (Path Abs File)
getSourceCompletionCachePath t =
  getCachePath t ".cmp"

getSourceLocationCachePath :: Target.Target -> Src.Source -> App (Path Abs File)
getSourceLocationCachePath t =
  getCachePath t ".loc"

getCachePath :: Target.Target -> String -> Src.Source -> App (Path Abs File)
getCachePath t extension source = do
  artifactDir <- getArtifactDir t $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  P.addExtension extension (artifactDir </> relPathWithoutExtension)

attachOutputPath :: Target.Target -> OK.OutputKind -> Src.Source -> App (OK.OutputKind, Path Abs File)
attachOutputPath t outputKind source = do
  outputPath <- sourceToOutputPath t outputKind source
  return (outputKind, outputPath)

getOutputPathForEntryPoint :: Module -> OK.OutputKind -> Target.MainTarget -> App (OK.OutputKind, Path Abs File)
getOutputPathForEntryPoint baseModule kind mainTarget = do
  case mainTarget of
    Target.Named target _ -> do
      entryDir <- getEntryDir (Target.Main mainTarget) baseModule
      relPath <- parseRelFile $ T.unpack target
      outputPath <- Src.attachExtension (entryDir </> relPath) kind
      return (kind, outputPath)
    Target.Zen path _ -> do
      zenEntryDir <- getZenEntryDir (Target.Main mainTarget) baseModule
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
