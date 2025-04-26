module Move.Context.Path
  ( Handle (..),
    new,
    getDependencyDirPath,
    ensureNotInDependencyDir,
    writeByteString,
    writeText,
    getBaseName,
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

import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.UTF8 qualified as B
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time
import Data.Version qualified as V
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Clang qualified as Clang
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, raiseError')
import Move.Context.Env qualified as Env
import Path (Abs, Dir, File, Path, Rel, (</>))
import Path qualified as P
import Path.IO qualified as P
import Paths_neut
import Rule.ClangOption qualified as CL
import Rule.Const
import Rule.Digest
import Rule.Ens qualified as E
import Rule.Ens.Reify qualified as E
import Rule.Module
import Rule.Module qualified as M
import Rule.ModuleID qualified as MID
import Rule.OutputKind qualified as OK
import Rule.Platform as TP
import Rule.Source qualified as Src
import Rule.Target qualified as Target

data Handle
  = Handle
  { cacheRef :: IORef (Maybe String),
    clangHandle :: Clang.Handle,
    debugHandle :: Debug.Handle,
    mainModule :: M.MainModule
  }

-- temporary
new :: App Handle
new = do
  cacheRef <- asks App.buildSignatureCache
  clangHandle <- Clang.new
  envHandle <- Env.new
  mainModule <- liftIO $ Env.getMainModule envHandle
  debugHandle <- Debug.new
  return $ Handle {..}

getBaseName :: Path Abs File -> EIO T.Text
getBaseName path = do
  let dirPath = P.parent path
  filename <- P.stripProperPrefix dirPath path
  return $ T.replace packageFileExtension "" $ T.pack $ P.toFilePath filename

writeByteString :: Path Abs File -> L.ByteString -> IO ()
writeByteString path =
  L.writeFile (P.toFilePath path)

writeText :: Path Abs File -> T.Text -> IO ()
writeText path text =
  B.writeFile (P.toFilePath path) $ encodeUtf8 text

returnDirectory :: Path Abs Dir -> EIO (Path Abs Dir)
returnDirectory path =
  P.ensureDir path >> return path

getDependencyDirPath :: Module -> EIO (Path Abs Dir)
getDependencyDirPath baseModule = do
  let moduleRootDir = getModuleRootDir baseModule
  case moduleID baseModule of
    MID.Library _ ->
      returnDirectory $ P.parent moduleRootDir
    _ -> do
      returnDirectory $ moduleRootDir </> moduleCacheDir baseModule </> $(P.mkRelDir "dependency")

ensureNotInDependencyDir :: MainModule -> EIO ()
ensureNotInDependencyDir mainModule = do
  case moduleID (extractModule mainModule) of
    MID.Library _ ->
      raiseError' "This command cannot be used under a dependency directory"
    _ ->
      return ()

getPlatformPrefix :: EIO (Path Rel Dir)
getPlatformPrefix = do
  p <- Env.getPlatform Nothing
  P.parseRelDir $ T.unpack $ TP.reify p

getExecutableOutputPath :: Handle -> Target.MainTarget -> Module -> EIO (Path Abs File)
getExecutableOutputPath h targetOrZen mainModule = do
  case targetOrZen of
    Target.Named target _ -> do
      executableDir <- getExecutableDir h (Target.Main targetOrZen) mainModule
      P.resolveFile executableDir $ T.unpack target
    Target.Zen path _ -> do
      zenExecutableDir <- getZenExecutableDir h (Target.Main targetOrZen) mainModule
      relPath <- getRelPathFromSourceDir mainModule path
      (relPathWithoutExtension, _) <- P.splitExtension relPath
      return $ zenExecutableDir </> relPathWithoutExtension

getBaseBuildDir :: Module -> EIO (Path Abs Dir)
getBaseBuildDir baseModule = do
  platformPrefix <- getPlatformPrefix
  versionDir <- P.parseRelDir $ "compiler-" ++ V.showVersion version
  let moduleRootDir = getModuleRootDir baseModule
  return $ moduleRootDir </> moduleCacheDir baseModule </> $(P.mkRelDir "build") </> platformPrefix </> versionDir

getBuildDir :: Handle -> Target.Target -> Module -> EIO (Path Abs Dir)
getBuildDir h t baseModule = do
  baseBuildDir <- getBaseBuildDir baseModule
  buildSignature <- getBuildSignature h t
  buildPrefix <- P.parseRelDir $ "build-" ++ buildSignature
  return $ baseBuildDir </> buildPrefix

getBuildSignature :: Handle -> Target.Target -> EIO String
getBuildSignature h t = do
  sigCache <- liftIO $ readIORef (cacheRef h)
  case sigCache of
    Just sig -> do
      return sig
    Nothing -> do
      clangDigest <- Clang.getClangDigest (clangHandle h)
      let MainModule m = mainModule h
      clangOption <- getClangOption t m
      moduleEns <- liftIO $ B.readFile $ P.toFilePath $ moduleLocation m
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
      liftIO $ writeIORef (cacheRef h) $ Just sig
      return sig

getClangOption :: Target.Target -> Module -> EIO CL.ClangOption
getClangOption t baseModule =
  case t of
    Target.Main mainModule ->
      case mainModule of
        Target.Named name _ ->
          case Map.lookup name (moduleTarget baseModule) of
            Just (Target.TargetSummary {clangOption}) ->
              return clangOption
            Nothing ->
              raiseError' $ "No such target is defined: `" <> name <> "`"
        Target.Zen _ clangOption ->
          return clangOption
    Target.Peripheral ->
      return CL.empty
    Target.PeripheralSingle _ ->
      return CL.empty

getArtifactDir :: Handle -> Target.Target -> Module -> EIO (Path Abs Dir)
getArtifactDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> artifactRelDir

getForeignDir :: Handle -> Target.Target -> Module -> EIO (Path Abs Dir)
getForeignDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  let foreignDir = buildDir </> foreignRelDir
  P.ensureDir foreignDir
  return foreignDir

getEntryDir :: Handle -> Target.Target -> Module -> EIO (Path Abs Dir)
getEntryDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> entryRelDir

getExecutableDir :: Handle -> Target.Target -> Module -> EIO (Path Abs Dir)
getExecutableDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> executableRelDir

getZenExecutableDir :: Handle -> Target.Target -> Module -> EIO (Path Abs Dir)
getZenExecutableDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> zenRelDir </> executableRelDir

getZenEntryDir :: Handle -> Target.Target -> Module -> EIO (Path Abs Dir)
getZenEntryDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> zenRelDir </> entryRelDir

sourceToOutputPath :: Handle -> Target.Target -> OK.OutputKind -> Src.Source -> EIO (Path Abs File)
sourceToOutputPath h t kind source = do
  artifactDir <- getArtifactDir h t $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  Src.attachExtension (artifactDir </> relPathWithoutExtension) kind

getSourceCachePath :: Handle -> Target.Target -> Src.Source -> EIO (Path Abs File)
getSourceCachePath h t =
  getCachePath h t ".def"

getSourceCompletionCachePath :: Handle -> Target.Target -> Src.Source -> EIO (Path Abs File)
getSourceCompletionCachePath h t =
  getCachePath h t ".cmp"

getSourceLocationCachePath :: Handle -> Target.Target -> Src.Source -> EIO (Path Abs File)
getSourceLocationCachePath h t =
  getCachePath h t ".loc"

getCachePath :: Handle -> Target.Target -> String -> Src.Source -> EIO (Path Abs File)
getCachePath h t extension source = do
  artifactDir <- getArtifactDir h t $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  P.addExtension extension (artifactDir </> relPathWithoutExtension)

attachOutputPath :: Handle -> Target.Target -> OK.OutputKind -> Src.Source -> EIO (OK.OutputKind, Path Abs File)
attachOutputPath h t outputKind source = do
  outputPath <- sourceToOutputPath h t outputKind source
  return (outputKind, outputPath)

getOutputPathForEntryPoint ::
  Handle ->
  Module ->
  OK.OutputKind ->
  Target.MainTarget ->
  EIO (OK.OutputKind, Path Abs File)
getOutputPathForEntryPoint h baseModule kind mainTarget = do
  case mainTarget of
    Target.Named target _ -> do
      entryDir <- getEntryDir h (Target.Main mainTarget) baseModule
      relPath <- P.parseRelFile $ T.unpack target
      outputPath <- Src.attachExtension (entryDir </> relPath) kind
      return (kind, outputPath)
    Target.Zen path _ -> do
      zenEntryDir <- getZenEntryDir h (Target.Main mainTarget) baseModule
      relPath <- getRelPathFromSourceDir baseModule path
      (relPathWithoutExtension, _) <- P.splitExtension relPath
      outputPath <- Src.attachExtension (zenEntryDir </> relPathWithoutExtension) kind
      return (kind, outputPath)

getInstallDir :: FilePath -> EIO (Path Abs Dir)
getInstallDir filePath = do
  path <- P.resolveDir' filePath
  P.ensureDir path
  return path

getLastModifiedSup :: [Path Abs File] -> EIO (Maybe UTCTime)
getLastModifiedSup pathList =
  case pathList of
    [] ->
      return Nothing
    [path] -> do
      b <- P.doesFileExist path
      if b
        then Just <$> P.getModificationTime path
        else return Nothing
    path : pathList' -> do
      b <- P.doesFileExist path
      if b
        then do
          t1 <- P.getModificationTime path
          t2 <- getLastModifiedSup pathList'
          if Just t1 > t2
            then return $ Just t1
            else return t2
        else do
          return Nothing

getLastModifiedInf :: [Path Abs File] -> EIO (Maybe UTCTime)
getLastModifiedInf pathList =
  case pathList of
    [] ->
      return Nothing
    [path] -> do
      b <- P.doesFileExist path
      if b
        then Just <$> P.getModificationTime path
        else return Nothing
    path : pathList' -> do
      b <- P.doesFileExist path
      if b
        then do
          t1 <- P.getModificationTime path
          t2 <- getLastModifiedInf pathList'
          if Just t1 < t2
            then return $ Just t1
            else return t2
        else do
          return Nothing

unrollPath :: M.SomePath Abs -> EIO [Path Abs File]
unrollPath path = do
  case path of
    Left dirPath -> do
      (_, filePathList) <- P.listDirRecur dirPath
      return filePathList
    Right filePath ->
      return [filePath]
