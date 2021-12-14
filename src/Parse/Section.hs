{-# LANGUAGE TemplateHaskell #-}

module Parse.Section
  ( getSectionForSourceFile,
    getModuleRootDir,
    getSpecForCurrentDirectory,
    Spec (..),
  )
where

import Data.Entity (Entity, access, toDictionary, toString)
import Data.Global (mainModuleDirRef, moduleFileName)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (readIORef)
import Data.Log (raiseError')
import qualified Data.Text as T
import qualified Parse.Entity as E
import Path (Abs, Dir, File, Path, Rel, dirname, mkRelFile, parent, parseRelDir, parseRelFile, splitExtension, stripProperPrefix, toFilePath, (</>))
import Path.IO (doesFileExist, getCurrentDir)

type ModuleName =
  T.Text

newtype URL
  = URL T.Text

newtype Checksum
  = Checksum T.Text

data Spec = Spec
  { specSourcePath :: Path Abs Dir,
    specTargetPath :: Path Abs Dir,
    specEntryPoint :: Map.HashMap T.Text (Path Abs File),
    specDependency :: Map.HashMap ModuleName (URL, Checksum)
  }

getSectionForSourceFile :: Path Abs File -> IO [T.Text]
getSectionForSourceFile sourceFilePath = do
  specPath <- getModuleFilePath (parent sourceFilePath)
  let moduleDir = parent specPath
  spec <- parseModuleFile specPath
  let sourceDirPath = specSourcePath spec
  relFilePath <- stripProperPrefix sourceDirPath sourceFilePath
  (relFilePath', _) <- splitExtension relFilePath
  let section = T.splitOn "/" $ T.pack $ toFilePath relFilePath'
  mainModuleDir <- readIORef mainModuleDirRef
  if mainModuleDir == moduleDir
    then return $ "main" : section
    else return $ T.pack (toFilePath (dirname moduleDir)) : section

getModuleRootDir :: Path Abs File -> IO (Path Abs Dir)
getModuleRootDir sourceFile = do
  parent <$> getModuleFilePath (parent sourceFile)

getSpecForCurrentDirectory :: IO Spec
getSpecForCurrentDirectory = do
  currentDir <- getCurrentDir
  getModuleFilePath currentDir >>= parseModuleFile

getModuleFilePath :: Path Abs Dir -> IO (Path Abs File)
getModuleFilePath path = do
  let absModuleFile = path </> $(mkRelFile moduleFileName)
  moduleFileExists <- doesFileExist absModuleFile
  case (moduleFileExists, path /= parent path) of
    (True, _) ->
      return absModuleFile
    (_, True) ->
      getModuleFilePath $ parent path
    _ ->
      raiseError' "couldn't find a module file."

parseModuleFile :: Path Abs File -> IO Spec
parseModuleFile moduleFilePath = do
  entity <- E.parse moduleFilePath
  sourceDirPath <- access "source-directory" entity >>= toString >>= interpretRelDirString moduleFilePath
  targetDirPath <- access "target-directory" entity >>= toString >>= interpretRelDirString moduleFilePath
  entryPointEns <- access "entry-point" entity >>= toDictionary
  dependencyEns <- access "dependency" entity >>= toDictionary
  entryPoint <- mapM (interpretEntryPoint sourceDirPath) entryPointEns
  dependency <- mapM interpretDependency dependencyEns
  return
    Spec
      { specSourcePath = sourceDirPath,
        specTargetPath = targetDirPath,
        specEntryPoint = entryPoint,
        specDependency = dependency
      }

interpretEntryPoint :: Path Abs Dir -> Entity -> IO (Path Abs File)
interpretEntryPoint sourceDirPath entryPointFilePathValue = do
  entryPointFilePath <- toString entryPointFilePathValue
  relFilePath <- parseRelFile (T.unpack entryPointFilePath)
  return $ sourceDirPath </> relFilePath

interpretDependency :: Entity -> IO (URL, Checksum)
interpretDependency dependencyValue = do
  url <- access "URL" dependencyValue >>= toString
  checksum <- access "checksum" dependencyValue >>= toString
  return (URL url, Checksum checksum)

interpretRelDirString :: Path Abs File -> T.Text -> IO (Path Abs Dir)
interpretRelDirString moduleFilePath sourceDirPathString = do
  let moduleRootDir = parent moduleFilePath
  relSourceDir <- parseRelDir $ T.unpack sourceDirPathString
  return $ moduleRootDir </> relSourceDir
