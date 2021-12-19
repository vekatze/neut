{-# LANGUAGE TemplateHaskell #-}

module Parse.Import
  ( parseImport,
    parseImportSequence,
    Signature,
  )
where

import Control.Monad (forM_, unless)
import Data.Basic (Hint)
import Data.Global
  ( VisitInfo (VisitInfoActive, VisitInfoFinish),
    defaultModulePrefix,
    fileEnv,
    nsSep,
    topNameEnv,
    traceEnv,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import Data.Log (raiseCritical, raiseError)
import Data.Module (Module (..), ModuleSignature (ModuleThat, ModuleThis), Source (Source), getModuleName, signatureToModule, sourceFilePath, sourceModule)
import qualified Data.Set as S
import Data.Spec (Spec (specDependency), getSourceDir)
import Data.Stmt
  ( EnumInfo,
    HeaderStmtPlus,
    Stmt (StmtDef),
    WeakStmtPlus,
    loadCache,
  )
import qualified Data.Text as T
import Parse.Core
  ( currentHint,
    many,
    symbol,
    token,
    tryPlanList,
  )
import Parse.Enum (insEnumEnv)
import Parse.Section (pathToSection, sectionToPath)
import Parse.Spec (moduleToSpec)
import Path
  ( Abs,
    File,
    Path,
    toFilePath,
  )
import Path.IO (doesFileExist, resolveFile)

type Signature = (T.Text, [T.Text])

parseImport :: Spec -> (Source -> IO ([HeaderStmtPlus], WeakStmtPlus, [EnumInfo])) -> Signature -> IO [HeaderStmtPlus]
parseImport currentSpec visiter signature = do
  m <- currentHint
  source <- signatureToSource currentSpec signature
  denv <- readIORef fileEnv
  let newPath = sourceFilePath source
  case Map.lookup newPath denv of
    Just VisitInfoActive ->
      raiseCyclicInclusion m newPath
    Just VisitInfoFinish -> do
      return []
    Nothing -> do
      stmtListOrNothing <- loadCache m newPath
      case stmtListOrNothing of
        Just (stmtList, enumInfoList) ->
          useCache newPath stmtList enumInfoList
        Nothing ->
          visitNewFile visiter source

signatureToSource :: Spec -> Signature -> IO Source
signatureToSource currentSpec (moduleName, section) = do
  m <- currentHint
  mo <- parseModuleName m currentSpec moduleName >>= signatureToModule
  filePath <- getSourceFilePath m mo (sectionToPath section)
  return $
    Source
      { sourceModule = mo,
        sourceFilePath = filePath
      }

parseImportSequence :: IO [(Signature, Maybe T.Text)]
parseImportSequence =
  many $
    tryPlanList
      [ do
          (sectionString, alias) <- parseImportQualified
          return (sectionString, Just alias),
        do
          sectionString <- parseImportSimple
          return (sectionString, Nothing)
      ]

parseImportSimple :: IO Signature
parseImportSimple = do
  m <- currentHint
  token "import"
  symbol >>= parseModuleInfo m

parseImportQualified :: IO (Signature, T.Text)
parseImportQualified = do
  m <- currentHint
  token "import"
  preSection <- symbol >>= parseModuleInfo m
  token "as"
  alias <- symbol
  return (preSection, alias)

parseModuleName :: Hint -> Spec -> T.Text -> IO ModuleSignature
parseModuleName m currentSpec moduleName
  | moduleName == defaultModulePrefix =
    return ModuleThis
  | Just (_, checksum) <- Map.lookup moduleName (specDependency currentSpec) =
    return $ ModuleThat moduleName checksum
  | otherwise =
    raiseError m $ "no such module is declared: " <> moduleName

parseModuleInfo :: Hint -> T.Text -> IO Signature
parseModuleInfo m sectionString = do
  let xs = T.splitOn "." sectionString
  case xs of
    [] ->
      raiseCritical m "empty symbol"
    [_] ->
      raiseError m "this module signature is malformed"
    moduleName : sectionPath ->
      return (moduleName, sectionPath)

getSourceFilePath :: Hint -> Module -> FilePath -> IO (Path Abs File)
getSourceFilePath m mo relPathString = do
  spec <- moduleToSpec m mo
  filePath <- resolveFile (getSourceDir spec) relPathString
  ensureFileExistence m mo spec filePath
  return filePath

useCache ::
  Path Abs File ->
  [Stmt] ->
  [(Hint, T.Text, [(T.Text, Int)])] ->
  IO [HeaderStmtPlus]
useCache newPath stmtList enumInfoList = do
  forM_ enumInfoList $ \(mEnum, name, itemList) -> do
    insEnumEnv mEnum name itemList
  let names = S.fromList $ map (\(StmtDef _ _ x _ _) -> x) stmtList
  modifyIORef' topNameEnv $ S.union names
  modifyIORef' fileEnv $ \env -> Map.insert newPath VisitInfoFinish env
  return [(newPath, Left stmtList, enumInfoList)]

visitNewFile :: (t -> IO ([(a1, Either a2 b, c)], (a1, b), c)) -> t -> IO [(a1, Either a2 b, c)]
visitNewFile visiter newPath = do
  (ss, (pathInfo, bodyInfo), enumInfoList) <- visiter newPath
  return $ ss ++ [(pathInfo, Right bodyInfo, enumInfoList)]

raiseCyclicInclusion :: Hint -> Path Abs File -> IO b
raiseCyclicInclusion m newPath = do
  tenv <- readIORef traceEnv
  let cyclicPath = dropWhile (/= newPath) (reverse tenv) ++ [newPath]
  raiseError m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPath

ensureFileExistence :: Hint -> Module -> Spec -> Path Abs File -> IO ()
ensureFileExistence m mo spec sourcePath = do
  b <- doesFileExist sourcePath
  unless b $ do
    let moduleName = getModuleName mo
    (_, pathInfo) <- pathToSection spec sourcePath
    raiseError m $ "the module `" <> moduleName <> "` does not have the component `" <> T.intercalate nsSep pathInfo <> "`"

showCyclicPath :: [Path Abs File] -> T.Text
showCyclicPath pathList =
  case pathList of
    [] ->
      ""
    [path] ->
      T.pack (toFilePath path)
    (path : ps) ->
      "     " <> T.pack (toFilePath path) <> showCyclicPath' ps

showCyclicPath' :: [Path Abs File] -> T.Text
showCyclicPath' pathList =
  case pathList of
    [] ->
      ""
    [path] ->
      "\n  ~> " <> T.pack (toFilePath path)
    path : ps ->
      "\n  ~> " <> T.pack (toFilePath path) <> showCyclicPath' ps
