module Parse.Section
  ( getSpecForBuildDirectory,
    pathToSection,
    sectionToPath,
    getMainSpec,
  )
where

import Data.Global (defaultModulePrefix, mainModuleDirRef, setCurrentFilePath, sourceFileExtension)
import Data.IORef (readIORef)
import Data.Module (getMainModule, moduleFilePath)
import Data.Spec (Spec (..), getSourceDir)
import qualified Data.Text as T
import Parse.Core (currentHint)
import Parse.Spec (moduleToSpec)
import Path (Abs, Dir, File, Path, dirname, parent, splitExtension, stripProperPrefix, toFilePath)
import System.FilePath.Posix (dropTrailingPathSeparator, pathSeparator)

pathToSection :: Spec -> Path Abs File -> IO (T.Text, [T.Text])
pathToSection spec sourceFilePath = do
  relFilePath <- stripProperPrefix (getSourceDir spec) sourceFilePath
  (relFilePath', _) <- splitExtension relFilePath
  let section = T.splitOn "/" $ T.pack $ toFilePath relFilePath'
  let moduleDir = parent $ specLocation spec
  mainModuleDir <- readIORef mainModuleDirRef
  if mainModuleDir == moduleDir
    then return (defaultModulePrefix, section)
    else return (T.pack (getDirectoryName moduleDir), section)

getDirectoryName :: Path a Dir -> String
getDirectoryName =
  dropTrailingPathSeparator . toFilePath . dirname

sectionToPath :: [T.Text] -> FilePath
sectionToPath sectionPath =
  T.unpack $ T.intercalate (T.singleton pathSeparator) sectionPath <> "." <> sourceFileExtension

getSpecForBuildDirectory :: IO Spec
getSpecForBuildDirectory = do
  m <- currentHint
  mo <- getMainModule
  moduleToSpec m mo

getMainSpec :: IO Spec
getMainSpec = do
  mainModule <- getMainModule
  setCurrentFilePath $ moduleFilePath mainModule
  m <- currentHint
  moduleToSpec m mainModule
