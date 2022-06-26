module Entity.Source where

import qualified Data.Text as T
import Entity.Module
import Entity.OutputKind
import Path
import qualified System.FilePath as FP

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

getLocator :: Source -> IO T.Text
getLocator source = do
  domain <- getDomain (sourceModule source)
  sigTail <- getLocatorTail source
  return $ T.intercalate "." $ domain : sigTail

getDomain :: Module -> IO T.Text
getDomain targetModule = do
  mainModule <- getMainModule
  if moduleLocation mainModule == moduleLocation targetModule
    then return defaultModulePrefix
    else return $ T.pack $ FP.dropTrailingPathSeparator $ toFilePath $ dirname $ parent (moduleLocation targetModule)

getLocatorTail :: Source -> IO [T.Text]
getLocatorTail source = do
  relFilePath <- stripProperPrefix (getSourceDir $ sourceModule source) $ sourceFilePath source
  (relFilePath', _) <- splitExtension relFilePath
  return $ T.splitOn "/" $ T.pack $ toFilePath relFilePath'

isMainFile :: Source -> IO Bool
isMainFile source = do
  sourceRelPath <- stripProperPrefix (getSourceDir (sourceModule source)) (sourceFilePath source)
  return $ elem sourceRelPath $ moduleTarget (sourceModule source)
