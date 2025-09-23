module Kernel.Common.Handle.Global.Path
  ( Handle (..),
    new,
    ensureNotInDependencyDir,
    getBaseName,
    getExecutableOutputPath,
    getForeignDir,
    getInstallDir,
    sourceToOutputPath,
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

import App.App (App)
import App.Run (raiseError')
import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Data.ByteString.UTF8 qualified as B
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Data.Time
import Ens.Ens qualified as E
import Ens.ToDoc qualified as E
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.Const
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Module
import Kernel.Common.Module qualified as M
import Kernel.Common.OutputKind qualified as OK
import Kernel.Common.Source qualified as Src
import Kernel.Common.Target qualified as Target
import Language.Common.Digest
import Language.Common.ModuleID qualified as MID
import Logger.Handle qualified as Logger
import Path (Abs, Dir, File, Path, (</>))
import Path qualified as P
import Path.IO qualified as P
import Path.Read (readTextFromPath)

data Handle = Handle
  { _mainModule :: MainModule,
    _cacheRef :: IORef (Maybe String),
    _loggerHandle :: Logger.Handle,
    _platformHandle :: Platform.Handle
  }

new :: MainModule -> Platform.Handle -> Logger.Handle -> IO Handle
new _mainModule _platformHandle _loggerHandle = do
  _cacheRef <- newIORef Nothing
  return $ Handle {..}

getBaseName :: Path Abs File -> App T.Text
getBaseName path = do
  let dirPath = P.parent path
  filename <- P.stripProperPrefix dirPath path
  return $ T.replace packageFileExtension "" $ T.pack $ P.toFilePath filename

ensureNotInDependencyDir :: MainModule -> App ()
ensureNotInDependencyDir mainModule = do
  case moduleID (extractModule mainModule) of
    MID.Library _ ->
      raiseError' "This command cannot be used under a dependency directory"
    _ ->
      return ()

getExecutableOutputPath :: Handle -> Target.MainTarget -> App (Path Abs File)
getExecutableOutputPath h targetOrZen = do
  let m = extractModule $ _mainModule h
  case targetOrZen of
    Target.Named target _ -> do
      executableDir <- getExecutableDir h (Target.Main targetOrZen) m
      P.resolveFile executableDir $ T.unpack target
    Target.Zen path _ -> do
      zenExecutableDir <- getZenExecutableDir h (Target.Main targetOrZen) m
      relPath <- getRelPathFromSourceDir m path
      (relPathWithoutExtension, _) <- P.splitExtension relPath
      zenPath <- P.addExtension ".zen" relPathWithoutExtension
      return $ zenExecutableDir </> zenPath

getBuildDir :: Handle -> Target.Target -> Module -> App (Path Abs Dir)
getBuildDir h t baseModule = do
  baseBuildDir <- Platform.getBaseBuildDir (_platformHandle h) baseModule
  buildSignature <- getBuildSignature h t
  buildPrefix <- P.parseRelDir $ "build-" ++ buildSignature
  return $ baseBuildDir </> buildPrefix

getBuildSignature :: Handle -> Target.Target -> App String
getBuildSignature h t = do
  sigCache <- liftIO $ readIORef (_cacheRef h)
  case sigCache of
    Just sig -> do
      return sig
    Nothing -> do
      let clangDigest = Platform.getClangDigest (_platformHandle h)
      let MainModule m = _mainModule h
      clangOption <- getClangOption t m
      moduleEns' <- readTextFromPath (moduleLocation m)
      let ens =
            E.dictFromList
              _m
              [ ("clang-digest", _m :< E.String clangDigest),
                ("compile-option", _m :< E.String (T.unwords $ CL.compileOption clangOption)),
                ("link-option", _m :< E.String (T.unwords $ CL.linkOption clangOption)),
                ("module-configuration", _m :< E.String moduleEns')
              ]
      let sig = B.toString $ hashAndEncode $ B.fromString $ T.unpack $ E.pp $ E.inject ens
      liftIO $ writeIORef (_cacheRef h) $ Just sig
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
              raiseError' $ "No such target is defined: `" <> name <> "`"
        Target.Zen _ clangOption ->
          return clangOption
    Target.Peripheral ->
      return CL.empty
    Target.PeripheralSingle _ ->
      return CL.empty

getArtifactDir :: Handle -> Target.Target -> Module -> App (Path Abs Dir)
getArtifactDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> artifactRelDir

getForeignDir :: Handle -> Target.Target -> Module -> App (Path Abs Dir)
getForeignDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  let foreignDir = buildDir </> foreignRelDir
  P.ensureDir foreignDir
  return foreignDir

getEntryDir :: Handle -> Target.Target -> Module -> App (Path Abs Dir)
getEntryDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> entryRelDir

getExecutableDir :: Handle -> Target.Target -> Module -> App (Path Abs Dir)
getExecutableDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> executableRelDir

getZenExecutableDir :: Handle -> Target.Target -> Module -> App (Path Abs Dir)
getZenExecutableDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> zenRelDir </> executableRelDir

getZenEntryDir :: Handle -> Target.Target -> Module -> App (Path Abs Dir)
getZenEntryDir h t baseModule = do
  buildDir <- getBuildDir h t baseModule
  return $ buildDir </> zenRelDir </> entryRelDir

sourceToOutputPath :: Handle -> Target.Target -> OK.OutputKind -> Src.Source -> App (Path Abs File)
sourceToOutputPath h t kind source = do
  artifactDir <- getArtifactDir h t $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  attachExtension (artifactDir </> relPathWithoutExtension) kind

getSourceCachePath :: Handle -> Target.Target -> Src.Source -> App (Path Abs File)
getSourceCachePath h t =
  getCachePath h t ".def"

getSourceCompletionCachePath :: Handle -> Target.Target -> Src.Source -> App (Path Abs File)
getSourceCompletionCachePath h t =
  getCachePath h t ".cmp"

getSourceLocationCachePath :: Handle -> Target.Target -> Src.Source -> App (Path Abs File)
getSourceLocationCachePath h t =
  getCachePath h t ".loc"

getCachePath :: Handle -> Target.Target -> String -> Src.Source -> App (Path Abs File)
getCachePath h t extension source = do
  artifactDir <- getArtifactDir h t $ Src.sourceModule source
  relPath <- Src.getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- P.splitExtension relPath
  P.addExtension extension (artifactDir </> relPathWithoutExtension)

attachOutputPath :: Handle -> Target.Target -> OK.OutputKind -> Src.Source -> App (OK.OutputKind, Path Abs File)
attachOutputPath h t outputKind source = do
  outputPath <- sourceToOutputPath h t outputKind source
  return (outputKind, outputPath)

getOutputPathForEntryPoint ::
  Handle ->
  OK.OutputKind ->
  Target.MainTarget ->
  App (OK.OutputKind, Path Abs File)
getOutputPathForEntryPoint h kind mainTarget = do
  let baseModule = extractModule $ _mainModule h
  case mainTarget of
    Target.Named target _ -> do
      entryDir <- getEntryDir h (Target.Main mainTarget) baseModule
      relPath <- P.parseRelFile $ T.unpack target
      outputPath <- attachExtension (entryDir </> relPath) kind
      return (kind, outputPath)
    Target.Zen path _ -> do
      zenEntryDir <- getZenEntryDir h (Target.Main mainTarget) baseModule
      relPath <- getRelPathFromSourceDir baseModule path
      (relPathWithoutExtension, _) <- P.splitExtension relPath
      outputPath <- attachExtension (zenEntryDir </> relPathWithoutExtension) kind
      return (kind, outputPath)

getInstallDir :: FilePath -> App (Path Abs Dir)
getInstallDir filePath = do
  path <- P.resolveDir' filePath
  P.ensureDir path
  return path

getLastModifiedSup :: [Path Abs File] -> App (Maybe UTCTime)
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

getLastModifiedInf :: [Path Abs File] -> App (Maybe UTCTime)
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

unrollPath :: M.SomePath Abs -> App [Path Abs File]
unrollPath path = do
  case path of
    Left dirPath -> do
      (_, filePathList) <- P.listDirRecur dirPath
      return filePathList
    Right filePath ->
      return [filePath]

attachExtension :: Path Abs File -> OK.OutputKind -> App (Path Abs File)
attachExtension file kind =
  case kind of
    OK.LLVM -> do
      P.addExtension ".ll" file
    OK.Object -> do
      P.addExtension ".o" file
