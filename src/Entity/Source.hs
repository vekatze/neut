module Entity.Source where

import qualified Context.Throw as Throw
import qualified Data.Text as T
import Entity.Hint
import Entity.Locator
import qualified Entity.Locator.Reflect as Locator
import qualified Entity.Locator.Reify as Locator
import Entity.Module
import Entity.OutputKind
import Path

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

getTypeCachePath :: Source -> IO (Path Abs File)
getTypeCachePath source = do
  let artifactDir = getArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  addExtension ".t" (artifactDir </> relPathWithoutExtension)

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

isMainFile :: Source -> IO Bool
isMainFile source = do
  sourceRelPath <- stripProperPrefix (getSourceDir (sourceModule source)) (sourceFilePath source)
  return $ elem sourceRelPath $ moduleTarget (sourceModule source)

getNextSource :: Throw.Context -> Hint -> Module -> T.Text -> IO Source
getNextSource ctx m currentModule sigText = do
  srcLocator <- Locator.fromText ctx m currentModule sigText
  srcAbsPath <- Locator.toAbsPath srcLocator
  return $
    Source
      { sourceModule = sourceLocatorModule srcLocator,
        sourceFilePath = srcAbsPath
      }

sourceToOutputPath :: OutputKind -> Source -> IO (Path Abs File)
sourceToOutputPath kind source = do
  let artifactDir = getArtifactDir $ sourceModule source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  addExtensionAlongKind (artifactDir </> relPathWithoutExtension) kind

addExtensionAlongKind :: Path Abs File -> OutputKind -> IO (Path Abs File)
addExtensionAlongKind file kind =
  case kind of
    OutputKindLLVM -> do
      addExtension ".ll" file
    OutputKindAsm -> do
      addExtension ".s" file
    OutputKindObject -> do
      addExtension ".o" file
    OutputKindExecutable -> do
      return file