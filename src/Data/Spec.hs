{-# LANGUAGE TemplateHaskell #-}

module Data.Spec where

import Control.Comonad.Cofree (Cofree (..))
import Data.Entity (EntityF (EntityDictionary, EntityString), ppEntityTopLevel)
import Data.Global (defaultModulePrefix, getMainFilePath, getMainModuleDir, nsSep, sourceFileExtension)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Log (raiseCritical')
import Data.Module (Alias, Checksum (Checksum), OutputKind (..))
import qualified Data.Text as T
import Path (Abs, Dir, File, Path, Rel, addExtension, dirname, mkRelDir, parent, splitExtension, stripProperPrefix, toFilePath, (</>))
import System.FilePath.Posix (dropTrailingPathSeparator, pathSeparator)
import System.IO.Unsafe (unsafePerformIO)

newtype URL
  = URL T.Text
  deriving (Show)

data Spec = Spec
  { specSourceDir :: Path Rel Dir,
    specTargetDir :: Path Rel Dir,
    specEntryPoint :: Map.HashMap T.Text (Path Rel File),
    specDependency :: Map.HashMap Alias (URL, Checksum),
    specLocation :: Path Abs File
  }
  deriving (Show)

data Source = Source
  { sourceFilePath :: Path Abs File,
    sourceSpec :: Spec
  }
  deriving (Show)

instance Eq Source where
  s1 == s2 =
    sourceFilePath s1 == sourceFilePath s2

addDependency :: Alias -> URL -> Checksum -> Spec -> Spec
addDependency alias url checksum spec =
  spec {specDependency = Map.insert alias (url, checksum) (specDependency spec)}

ppSpec :: Spec -> T.Text
ppSpec spec = do
  let entryPoint = Map.map (\x -> () :< EntityString (T.pack (toFilePath x))) $ specEntryPoint spec
  let dependency = flip Map.map (specDependency spec) $ \(URL url, Checksum checksum) -> do
        let urlEntity = () :< EntityString url
        let checksumEntity = () :< EntityString checksum
        () :< EntityDictionary (Map.fromList [("checksum", checksumEntity), ("URL", urlEntity)])
  ppEntityTopLevel $
    Map.fromList
      [ ("dependency", () :< EntityDictionary dependency),
        ("source-directory", () :< EntityString (T.pack $ toFilePath $ specSourceDir spec)),
        ("target", () :< EntityDictionary entryPoint),
        ("target-directory", () :< EntityString (T.pack $ toFilePath $ specTargetDir spec))
      ]

getSourceDir :: Spec -> Path Abs Dir
getSourceDir spec =
  parent (specLocation spec) </> specSourceDir spec

getTargetDir :: Spec -> Path Abs Dir
getTargetDir spec =
  parent (specLocation spec) </> specTargetDir spec

getEntryPoint :: Spec -> T.Text -> Maybe (Path Abs File)
getEntryPoint spec entryPointName = do
  relPath <- Map.lookup entryPointName (specEntryPoint spec)
  return $ getSourceDir spec </> relPath

pathToSection :: Spec -> Path Abs File -> IO (T.Text, [T.Text])
pathToSection spec path = do
  relFilePath <- stripProperPrefix (getSourceDir spec) path
  (relFilePath', _) <- splitExtension relFilePath
  let section = T.splitOn "/" $ T.pack $ toFilePath relFilePath'
  let moduleDir = parent $ specLocation spec
  mainModuleDir <- getMainModuleDir
  if mainModuleDir == moduleDir
    then return (defaultModulePrefix, section)
    else return (T.pack (getDirectoryName moduleDir), section)

sectionToPath :: [T.Text] -> FilePath
sectionToPath sectionPath =
  T.unpack $ T.intercalate (T.singleton pathSeparator) sectionPath <> "." <> sourceFileExtension

getDirectoryName :: Path a Dir -> String
getDirectoryName =
  dropTrailingPathSeparator . toFilePath . dirname

getMainFunctionName :: Spec -> Path Abs File -> IO T.Text
getMainFunctionName mainSpec mainFilePath = do
  (sectionHead, sectionTail) <- pathToSection mainSpec mainFilePath
  let section = sectionHead : sectionTail
  return $ T.intercalate nsSep $ section ++ ["main"]

{-# NOINLINE mainSpecRef #-}
mainSpecRef :: IORef (Maybe Spec)
mainSpecRef =
  unsafePerformIO (newIORef Nothing)

setMainSpec :: Spec -> IO ()
setMainSpec spec = do
  mainSpecOrNothing <- readIORef mainSpecRef
  case mainSpecOrNothing of
    Just _ ->
      raiseCritical' "the main spec is already initialized"
    Nothing ->
      modifyIORef' mainSpecRef $ const $ Just spec

getMainSpec :: IO Spec
getMainSpec = do
  mainSpecOrNothing <- readIORef mainSpecRef
  case mainSpecOrNothing of
    Just mainSpec ->
      return mainSpec
    Nothing ->
      raiseCritical' "the main spec is not initialized"

getSpecAliasList :: Spec -> [(T.Text, T.Text)]
getSpecAliasList spec = do
  let dependencyList = Map.toList $ specDependency spec
  map (\(key, (_, Checksum checksum)) -> (key, checksum)) dependencyList

getProjectRootDir :: Spec -> Path Abs Dir
getProjectRootDir spec =
  parent $ specLocation spec

getProjectSourceDir :: Spec -> Path Abs Dir
getProjectSourceDir spec =
  getProjectRootDir spec </> $(mkRelDir "source")

getProjectArtifactDir :: Spec -> Path Abs Dir
getProjectArtifactDir spec =
  getProjectRootDir spec </> $(mkRelDir "target/artifact")

getProjectExecutableDir :: Spec -> Path Abs Dir
getProjectExecutableDir spec =
  getProjectRootDir spec </> $(mkRelDir "target/executable")

getRelPathFromSourceDir :: Source -> IO (Path Rel File)
getRelPathFromSourceDir source = do
  let sourceDir = getProjectSourceDir $ sourceSpec source
  stripProperPrefix sourceDir (sourceFilePath source)

sourceToOutputPath :: Source -> OutputKind -> IO (Path Abs File)
sourceToOutputPath source kind = do
  let artifactDir = getProjectArtifactDir $ sourceSpec source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  attachExtension (artifactDir </> relPathWithoutExtension) kind

sourceToCachePath :: Source -> IO (Path Abs File)
sourceToCachePath source = do
  let artifactDir = getProjectArtifactDir $ sourceSpec source
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  addExtension ".cache" (artifactDir </> relPathWithoutExtension)

isMain :: Source -> IO Bool
isMain source = do
  mainFilePath <- getMainFilePath
  return $ sourceFilePath source == mainFilePath

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
