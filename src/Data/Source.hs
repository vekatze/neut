module Data.Source where

import Data.Basic (OutputKind (..))
import Data.Module
  ( Module,
    defaultModulePrefix,
    getArtifactDir,
    getMainModule,
    getSourceDir,
    moduleLocation,
  )
import qualified Data.Text as T
import Path (Abs, File, Path, Rel, addExtension, dirname, parent, splitExtension, stripProperPrefix, toFilePath, (</>))
import System.FilePath (dropTrailingPathSeparator)

data Source = Source
  { sourceFilePath :: Path Abs File,
    sourceModule :: Module
  }
  deriving (Show)

getRelPathFromSourceDir :: Source -> IO (Path Rel File)
getRelPathFromSourceDir source = do
  let sourceDir = getSourceDir $ sourceModule source
  stripProperPrefix sourceDir (sourceFilePath source)

getOutputPath :: Source -> OutputKind -> IO (Path Abs File)
getOutputPath source kind = do
  let artifactDir = getArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  attachExtension (artifactDir </> relPathWithoutExtension) kind

getSourceCachePath :: Source -> IO (Path Abs File)
getSourceCachePath source = do
  let artifactDir = getArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  addExtension ".i" (artifactDir </> relPathWithoutExtension)

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

getSection :: Source -> IO T.Text
getSection source = do
  baseModule <- getMainModule
  let domain = getDomain baseModule (sourceModule source)
  sigTail <- getSignatureTail source
  return $ T.intercalate "." $ domain : sigTail

getDomain :: Module -> Module -> T.Text
getDomain baseModule targetModule =
  if moduleLocation baseModule == moduleLocation targetModule
    then defaultModulePrefix
    else T.pack $ dropTrailingPathSeparator $ toFilePath $ dirname $ parent (moduleLocation targetModule)

getSignatureTail :: Source -> IO [T.Text]
getSignatureTail source = do
  relFilePath <- stripProperPrefix (getSourceDir $ sourceModule source) $ sourceFilePath source
  (relFilePath', _) <- splitExtension relFilePath
  return $ T.splitOn "/" $ T.pack $ toFilePath relFilePath'
