module Data.Spec where

import Control.Comonad.Cofree (Cofree (..))
import Data.Entity (EntityF (EntityDictionary, EntityString), ppEntityTopLevel)
import Data.Global (defaultModulePrefix, getMainModuleDir, nsSep)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Log (raiseCritical')
import Data.Module (Alias, Checksum (Checksum))
import qualified Data.Text as T
import Path (Abs, Dir, File, Path, Rel, dirname, parent, splitExtension, stripProperPrefix, toFilePath, (</>))
import System.FilePath.Posix (dropTrailingPathSeparator)
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
pathToSection spec sourceFilePath = do
  relFilePath <- stripProperPrefix (getSourceDir spec) sourceFilePath
  (relFilePath', _) <- splitExtension relFilePath
  let section = T.splitOn "/" $ T.pack $ toFilePath relFilePath'
  let moduleDir = parent $ specLocation spec
  mainModuleDir <- getMainModuleDir
  if mainModuleDir == moduleDir
    then return (defaultModulePrefix, section)
    else return (T.pack (getDirectoryName moduleDir), section)

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
