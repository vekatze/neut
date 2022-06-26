module Scene.Parse.Import
  ( parseImportSequence,
    skipImportSequence,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import Data.List
import qualified Data.Text as T
import Entity.AliasInfo
import Entity.Checksum
import Entity.Global
import Entity.Hint
import Entity.Log
import Entity.Module
import Entity.Source
import Path
import Path.IO
import Scene.Parse.Core
import qualified Scene.Parse.Module as Module
import qualified System.FilePath as FP
import System.IO.Unsafe
import Text.Megaparsec

type SourceSignature =
  (T.Text, [T.Text], T.Text)

parseImportSequence :: Module -> Parser ([Source], [AliasInfo])
parseImportSequence currentModule = do
  unzip
    <$> choice
      [ importBlock $ manyList $ parseSingleImport currentModule,
        return []
      ]

parseSingleImport :: Module -> Parser (Source, AliasInfo)
parseSingleImport currentModule = do
  choice
    [ try $ parseImportQualified currentModule,
      parseImportSimple currentModule
    ]

skipSingleImport :: Parser ()
skipSingleImport = do
  choice
    [ try skipImportQualified,
      skipImportSimple
    ]

skipImportSequence :: Parser ()
skipImportSequence = do
  choice
    [ void $ importBlock $ manyList skipSingleImport,
      return ()
    ]

parseImportSimple :: Module -> Parser (Source, AliasInfo)
parseImportSimple currentModule = do
  m <- currentHint
  sigText <- symbol
  source <- liftIO $ getNextSource m currentModule sigText
  return (source, AliasInfoUse sigText)

skipImportSimple :: Parser ()
skipImportSimple = do
  _ <- symbol
  return ()

parseImportQualified :: Module -> Parser (Source, AliasInfo)
parseImportQualified currentModule = do
  m <- currentHint
  sigText <- symbol
  keyword "as"
  alias <- symbol
  source <- liftIO $ getNextSource m currentModule sigText
  return (source, AliasInfoPrefix m alias sigText)

skipImportQualified :: Parser ()
skipImportQualified = do
  _ <- symbol
  keyword "as"
  _ <- symbol
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
unsnoc =
  foldr go Nothing
  where
    go x acc =
      case acc of
        Nothing ->
          Just ([], x)
        Just (ys, y) ->
          Just (x : ys, y)

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
  T.unpack $ T.intercalate (T.singleton FP.pathSeparator) sectionPath <> "." <> sourceFileExtension

filePathToModuleFilePath :: Path Abs File -> IO (Path Abs File)
filePathToModuleFilePath filePath = do
  findModuleFile $ parent filePath

filePathToModuleFileDir :: Path Abs File -> IO (Path Abs Dir)
filePathToModuleFileDir filePath =
  parent <$> filePathToModuleFilePath filePath
