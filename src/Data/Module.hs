module Data.Module where

import Data.Global
  ( getCurrentFilePath,
    getLibraryDirPath,
    moduleFileName,
  )
import Data.Log (raiseError')
import qualified Data.Text as T
import Path (Abs, Dir, File, Path, parent)
import Path.IO
  ( doesFileExist,
    getCurrentDir,
    resolveDir,
    resolveFile,
  )

type AliasName = T.Text

newtype Checksum
  = Checksum T.Text
  deriving (Eq)

data ModuleSignature
  = ModuleThis
  | ModuleThat AliasName Checksum
  deriving (Eq)

data Module = Module
  { moduleSignature :: ModuleSignature,
    moduleFilePath :: Path Abs File
  }

data Source = Source
  { sourceModule :: Module,
    sourceFilePath :: Path Abs File
  }

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
  return $
    Module
      { moduleSignature = ModuleThis,
        moduleFilePath = filePath
      }

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
