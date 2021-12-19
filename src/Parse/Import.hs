{-# LANGUAGE TemplateHaskell #-}

module Parse.Import (parseImport) where

import Control.Monad (forM_, unless)
import Data.Basic (Hint)
import Data.Global
  ( VisitInfo (VisitInfoActive, VisitInfoFinish),
    defaultModulePrefix,
    enumEnv,
    fileEnv,
    revEnumEnv,
    topNameEnv,
    topNameEnvExt,
    traceEnv,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.List (find)
import Data.Log (raiseCritical, raiseError)
import Data.Module (Module (..), ModuleSignature (ModuleThat, ModuleThis), Source (Source), getModuleName, signatureToModule, sourceFilePath, sourceModule)
import Data.Namespace (nsSep)
import Data.Spec (Spec (specDependency, specSourceDir))
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
    symbol,
    token,
  )
import Parse.Section (pathToSection, sectionToPath)
import Parse.Spec (moduleToSpec)
import Path
  ( Abs,
    File,
    Path,
    toFilePath,
  )
import Path.IO (doesFileExist, resolveFile)

parseImport :: Spec -> (Source -> IO ([HeaderStmtPlus], WeakStmtPlus, [EnumInfo])) -> IO [HeaderStmtPlus]
parseImport currentSpec visiter = do
  m <- currentHint
  source <- parseImport' currentSpec
  denv <- readIORef fileEnv
  let newPath = sourceFilePath source
  case Map.lookup newPath denv of
    Just VisitInfoActive ->
      raiseCyclicInclusion m newPath
    Just (VisitInfoFinish nameInfo) -> do
      modifyIORef' topNameEnvExt $ \env -> Map.union nameInfo env
      return []
    Nothing -> do
      stmtListOrNothing <- loadCache m newPath
      case stmtListOrNothing of
        Just (stmtList, enumInfoList) ->
          useCache newPath stmtList enumInfoList
        Nothing ->
          visitNewFile visiter source

parseImport' :: Spec -> IO Source
parseImport' currentSpec = do
  m <- currentHint
  token "import"
  (moduleName, section) <- parseModuleInfo
  mo <- parseModuleName m currentSpec moduleName >>= signatureToModule
  filePath <- getSourceFilePath m mo (sectionToPath section)
  return $
    Source
      { sourceModule = mo,
        sourceFilePath = filePath
      }

parseModuleName :: Hint -> Spec -> T.Text -> IO ModuleSignature
parseModuleName m currentSpec moduleName
  | moduleName == defaultModulePrefix =
    return ModuleThis
  | Just (_, checksum) <- Map.lookup moduleName (specDependency currentSpec) =
    return $ ModuleThat moduleName checksum
  | otherwise =
    raiseError m $ "no such module is declared: " <> moduleName

parseModuleInfo :: IO (T.Text, [T.Text])
parseModuleInfo = do
  m <- currentHint
  moduleSection <- symbol
  let xs = T.splitOn "." moduleSection
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
  let dirPath = specSourceDir spec
  filePath <- resolveFile dirPath relPathString
  ensureFileExistence m mo spec filePath
  return filePath

useCache ::
  Path Abs File ->
  [Stmt] ->
  [(Hint, T.Text, [(T.Text, Int)])] ->
  IO [HeaderStmtPlus]
useCache newPath stmtList enumInfoList = do
  forM_ enumInfoList $ \(mEnum, name, itemList) -> do
    insEnumEnv (toFilePath newPath) mEnum name itemList
  let names = Map.fromList $ map (\(StmtDef _ _ x _ _) -> (x, toFilePath newPath)) stmtList
  modifyIORef' topNameEnvExt $ \env -> Map.union names env
  modifyIORef' fileEnv $ \env -> Map.insert newPath (VisitInfoFinish names) env
  return [(newPath, Left stmtList, enumInfoList)]

visitNewFile :: (t -> IO ([(a1, Either a2 b, c)], (a1, b), c)) -> t -> IO [(a1, Either a2 b, c)]
visitNewFile visiter newPath = do
  nenvExt <- readIORef topNameEnvExt
  (ss, (pathInfo, bodyInfo), enumInfoList) <- visiter newPath
  newNameEnv <- readIORef topNameEnv
  writeIORef topNameEnv Map.empty
  writeIORef topNameEnvExt $ Map.union nenvExt newNameEnv
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

insEnumEnv :: FilePath -> Hint -> T.Text -> [(T.Text, Int)] -> IO ()
insEnumEnv path m name xis = do
  eenv <- readIORef enumEnv
  let definedEnums = Map.keys eenv ++ map fst (concatMap snd (Map.elems eenv))
  case find (`elem` definedEnums) $ name : map fst xis of
    Just x ->
      raiseError m $ "the constant `" <> x <> "` is already defined [ENUM]"
    _ -> do
      let (xs, is) = unzip xis
      let rev = Map.fromList $ zip xs (zip3 (repeat path) (repeat name) is)
      modifyIORef' enumEnv $ \env -> Map.insert name (path, xis) env
      modifyIORef' revEnumEnv $ \env -> Map.union rev env
