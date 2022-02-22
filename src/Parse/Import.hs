module Parse.Import
  ( parseImportSequence,
    skipImportSequence,
  )
where

import Control.Monad (unless, void)
import Data.Basic (AliasInfo (..), Checksum (Checksum), Hint)
import Data.Global
  ( getCurrentFilePath,
    getLibraryDirPath,
    sourceFileExtension,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (uncons)
import Data.Log (raiseError)
import Data.Module
  ( Module (moduleDependency),
    defaultModulePrefix,
    findModuleFile,
    getSourceDir,
    moduleFile,
  )
import Data.Source (Source (Source), sourceFilePath, sourceModule)
import qualified Data.Text as T
import Parse.Core
  ( currentHint,
    isSymbolChar,
    lookAhead,
    parseByPredicate,
    parseInBlock,
    parseManyList,
    parseSymbol,
    parseToken,
    tryPlanList,
  )
import qualified Parse.Module as Module
import Path
  ( Abs,
    Dir,
    File,
    Path,
    parent,
    (</>),
  )
import Path.IO (doesFileExist, resolveDir, resolveFile)
import System.FilePath (pathSeparator)
import System.IO.Unsafe (unsafePerformIO)

type SourceSignature =
  (T.Text, [T.Text], T.Text)

parseImportSequence :: Module -> IO ([Source], [AliasInfo])
parseImportSequence currentModule = do
  headSymbol <- lookAhead (parseByPredicate isSymbolChar)
  fmap unzip $
    case headSymbol of
      Just "import" ->
        parseInBlock "import" $
          parseManyList $ do
            tryPlanList
              [ parseImportQualified currentModule
              ]
              (parseImportSimple currentModule)
      _ ->
        return []

skipImportSequence :: IO ()
skipImportSequence = do
  headSymbol <- lookAhead (parseByPredicate isSymbolChar)
  case headSymbol of
    Just "import" ->
      void $
        parseInBlock "import" $
          parseManyList $
            tryPlanList [skipImportQualified] skipImportSimple
    _ ->
      return ()

parseImportSimple :: Module -> IO (Source, AliasInfo)
parseImportSimple currentModule = do
  m <- currentHint
  sigText <- parseSymbol
  source <- getNextSource m currentModule sigText
  return (source, AliasInfoUse sigText)

skipImportSimple :: IO ()
skipImportSimple = do
  _ <- parseSymbol
  return ()

parseImportQualified :: Module -> IO (Source, AliasInfo)
parseImportQualified currentModule = do
  m <- currentHint
  sigText <- parseSymbol
  parseToken "as"
  alias <- parseSymbol
  source <- getNextSource m currentModule sigText
  return (source, AliasInfoPrefix m alias sigText)

skipImportQualified :: IO ()
skipImportQualified = do
  _ <- parseSymbol
  parseToken "as"
  _ <- parseSymbol
  return ()

getNextSource :: Hint -> Module -> T.Text -> IO Source
getNextSource m currentModule sigText = do
  sig <- parseModuleInfo m sigText
  newModule <- getNextModule m currentModule sig
  filePath <- getSourceFilePath newModule sig
  return $
    Source
      { sourceModule = newModule,
        sourceFilePath = filePath
      }

parseModuleInfo :: Hint -> T.Text -> IO SourceSignature
parseModuleInfo m sectionString = do
  case getHeadMiddleLast $ T.splitOn "." sectionString of
    Just sig ->
      return sig
    Nothing ->
      raiseError m "found a malformed module signature"

getHeadMiddleLast :: [a] -> Maybe (a, [a], a)
getHeadMiddleLast xs = do
  (y, ys) <- uncons xs
  (zs, z) <- unsnoc ys
  return (y, zs, z)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs =
  case xs of
    [] ->
      Nothing
    [x] ->
      return ([], x)
    y : ys -> do
      (zs, z) <- unsnoc ys
      return (y : zs, z)

getNextModule :: Hint -> Module -> SourceSignature -> IO Module
getNextModule m currentModule sig@(domain, _, _) = do
  nextModuleFilePath <- getNextModuleFilePath m currentModule sig
  moduleMap <- readIORef moduleMapRef
  case Map.lookup nextModuleFilePath moduleMap of
    Just nextModule ->
      return nextModule
    Nothing -> do
      moduleFileExists <- doesFileExist nextModuleFilePath
      unless moduleFileExists $ do
        raiseError m $
          T.pack "could not find the module file for `"
            <> domain
            <> "`"
      nextModule <- Module.parse nextModuleFilePath
      modifyIORef' moduleMapRef $ Map.insert nextModuleFilePath nextModule
      return nextModule

getNextModuleFilePath :: Hint -> Module -> SourceSignature -> IO (Path Abs File)
getNextModuleFilePath m currentModule sig = do
  moduleDirPath <- getNextModuleDirPath m currentModule sig
  return $ moduleDirPath </> moduleFile

getNextModuleDirPath :: Hint -> Module -> SourceSignature -> IO (Path Abs Dir)
getNextModuleDirPath m currentModule (domain, _, _) =
  if domain == defaultModulePrefix
    then getCurrentFilePath >>= filePathToModuleFileDir
    else do
      Checksum checksum <- getChecksum m currentModule domain
      libraryDir <- getLibraryDirPath
      resolveDir libraryDir $ T.unpack checksum

getSourceFilePath :: Module -> SourceSignature -> IO (Path Abs File)
getSourceFilePath baseModule (_, locator, name) = do
  resolveFile (getSourceDir baseModule) (sectionToPath $ locator ++ [name])

getChecksum :: Hint -> Module -> T.Text -> IO Checksum
getChecksum m currentModule domain =
  case Map.lookup domain (moduleDependency currentModule) of
    Just (_, checksum) ->
      return checksum
    Nothing ->
      raiseError m $ "no such module alias is defined: " <> domain

{-# NOINLINE moduleMapRef #-}
moduleMapRef :: IORef (Map.HashMap (Path Abs File) Module)
moduleMapRef =
  unsafePerformIO (newIORef Map.empty)

sectionToPath :: [T.Text] -> FilePath
sectionToPath sectionPath =
  T.unpack $ T.intercalate (T.singleton pathSeparator) sectionPath <> "." <> sourceFileExtension

filePathToModuleFilePath :: Path Abs File -> IO (Path Abs File)
filePathToModuleFilePath filePath = do
  findModuleFile $ parent filePath

filePathToModuleFileDir :: Path Abs File -> IO (Path Abs Dir)
filePathToModuleFileDir filePath =
  parent <$> filePathToModuleFilePath filePath
