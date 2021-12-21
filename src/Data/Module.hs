{-# LANGUAGE TemplateHaskell #-}

module Data.Module where

import Data.Global
  ( getCurrentFilePath,
    getLibraryDirPath,
    moduleFileName,
  )
import Data.Log (raiseError')
import qualified Data.Text as T
import Path (Abs, Dir, File, Path, Rel, addExtension, mkRelDir, parent, splitExtension, stripProperPrefix, (</>))
import Path.IO
  ( doesFileExist,
    getCurrentDir,
    resolveDir,
    resolveFile,
  )

type Alias =
  T.Text

newtype Checksum
  = Checksum T.Text
  deriving (Show, Ord, Eq)

showChecksum :: Checksum -> T.Text
showChecksum (Checksum checksum) =
  checksum

data OutputKind
  = OutputKindObject
  | OutputKindLLVM
  | OutputKindExecutable
  | OutputKindAsm
  deriving (Show)

data ModuleSignature
  = ModuleThis
  | ModuleThat Alias Checksum
  deriving (Show, Ord, Eq)

data Module = Module
  { moduleSignature :: ModuleSignature,
    moduleFilePath :: Path Abs File
  }
  deriving (Show, Ord, Eq)

data Source = Source
  { sourceModule :: Module,
    sourceFilePath :: Path Abs File
  }
  deriving (Show, Ord)

instance Eq Source where
  s1 == s2 =
    sourceFilePath s1 == sourceFilePath s2

getModuleName :: Module -> T.Text
getModuleName mo =
  case moduleSignature mo of
    ModuleThis ->
      "this"
    ModuleThat name _ ->
      name

filePathToModuleFilePath :: Path Abs File -> IO (Path Abs File)
filePathToModuleFilePath filePath = do
  dirPathToModuleFilePath $ parent filePath

dirPathToModuleFilePath :: Path Abs Dir -> IO (Path Abs File)
dirPathToModuleFilePath dirPath = do
  absModuleFile <- resolveFile dirPath moduleFileName
  moduleFileExists <- doesFileExist absModuleFile
  case (moduleFileExists, dirPath /= parent dirPath) of
    (True, _) ->
      return absModuleFile
    (_, True) ->
      dirPathToModuleFilePath $ parent dirPath
    _ ->
      raiseError' "couldn't find a module file."

filePathToModuleFileDir :: Path Abs File -> IO (Path Abs Dir)
filePathToModuleFileDir filePath =
  parent <$> filePathToModuleFilePath filePath

dirPathToModuleFileDir :: Path Abs Dir -> IO (Path Abs Dir)
dirPathToModuleFileDir dirPath =
  parent <$> dirPathToModuleFilePath dirPath

getMainModule :: IO Module
getMainModule = do
  filePath <- getCurrentDir >>= dirPathToModuleFilePath
  return
    Module
      { moduleSignature = ModuleThis,
        moduleFilePath = filePath
      }

constructLibraryModule :: Alias -> Checksum -> IO Module
constructLibraryModule alias checksum = do
  path <- getLibraryModuleFilePath checksum
  return
    Module
      { moduleSignature = ModuleThat alias checksum,
        moduleFilePath = path
      }

getLibraryModuleFilePath :: Checksum -> IO (Path Abs File)
getLibraryModuleFilePath checksum = do
  moduleDir <- getModuleDir checksum
  resolveFile moduleDir moduleFileName

getModuleDir :: Checksum -> IO (Path Abs Dir)
getModuleDir (Checksum checksum) = do
  libDir <- getLibraryDirPath
  resolveDir libDir $ T.unpack checksum

getModuleRootDir :: Module -> Path Abs Dir
getModuleRootDir mo =
  parent $ moduleFilePath mo

signatureToModuleFilePath :: ModuleSignature -> IO (Path Abs File)
signatureToModuleFilePath sig = do
  moduleDirPath <- signatureToModuleDirPath sig
  resolveFile moduleDirPath moduleFileName

signatureToModuleDirPath :: ModuleSignature -> IO (Path Abs Dir)
signatureToModuleDirPath sig =
  case sig of
    ModuleThis ->
      getCurrentFilePath >>= filePathToModuleFileDir
    ModuleThat _ (Checksum checksum) -> do
      libraryDir <- getLibraryDirPath
      resolveDir libraryDir (T.unpack checksum)

signatureToModule :: ModuleSignature -> IO Module
signatureToModule sig = do
  path <- signatureToModuleFilePath sig
  return $
    Module
      { moduleSignature = sig,
        moduleFilePath = path
      }

getModuleSourceDir :: Module -> Path Abs Dir
getModuleSourceDir mo =
  getModuleRootDir mo </> $(mkRelDir "source")

getModuleTargetDir :: Module -> Path Abs Dir
getModuleTargetDir mo =
  getModuleRootDir mo </> $(mkRelDir "target")

getModuleArtifactDir :: Module -> Path Abs Dir
getModuleArtifactDir mo = do
  getModuleRootDir mo </> $(mkRelDir "target/artifact")

getModuleExecutableDir :: Module -> Path Abs Dir
getModuleExecutableDir mo = do
  getModuleRootDir mo </> $(mkRelDir "target/executable")

getRelPathFromSourceDir :: Source -> IO (Path Rel File)
getRelPathFromSourceDir source = do
  let sourceDir = getModuleSourceDir $ sourceModule source
  stripProperPrefix sourceDir (sourceFilePath source)

sourceToOutputPath :: Source -> OutputKind -> IO (Path Abs File)
sourceToOutputPath source kind = do
  let artifactDir = getModuleArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  attachExtension (artifactDir </> relPathWithoutExtension) kind

sourceToCachePath :: Source -> IO (Path Abs File)
sourceToCachePath source = do
  let artifactDir = getModuleArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  addExtension ".cache" (artifactDir </> relPathWithoutExtension)

attachExtension :: Path Abs File -> OutputKind -> IO (Path Abs File)
attachExtension file kind =
  case kind of
    OutputKindLLVM -> do
      addExtension ".ll" file
    OutputKindAsm -> do
      addExtension ".s" file
    OutputKindObject -> do
      addExtension ".o" file
    OutputKindExecutable -> do
      return file
