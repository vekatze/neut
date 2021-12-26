module Parse.Import
  ( parseImportSequence,
    Signature,
    signatureToSource,
  )
where

import Control.Monad (unless)
import Data.Basic (Hint)
import Data.Global
  ( defaultModulePrefix,
    nsSep,
  )
import qualified Data.HashMap.Lazy as Map
import Data.Log (raiseCritical, raiseError)
import Data.Module (Module (..), ModuleSignature (ModuleThat, ModuleThis), getModuleName, signatureToModule)
import Data.Spec (Source (Source), Spec (specDependency), getSourceDir, pathToSection, sectionToPath, sourceFilePath, sourceSpec)
import qualified Data.Text as T
import Parse.Core
  ( currentHint,
    many,
    symbol,
    token,
    tryPlanList,
  )
import qualified Parse.Spec as Spec
import Path
  ( Abs,
    File,
    Path,
  )
import Path.IO (doesFileExist, resolveFile)

type Signature = (T.Text, [T.Text])

signatureToSource :: Spec -> Signature -> IO Source
signatureToSource currentSpec (moduleName, section) = do
  m <- currentHint
  mo <- parseModuleName m currentSpec moduleName >>= signatureToModule
  spec <- moduleToSpec m mo
  filePath <- getSourceFilePath m mo spec (sectionToPath section)
  return $
    Source
      { sourceSpec = spec,
        sourceFilePath = filePath
      }

moduleToSpec :: Hint -> Module -> IO Spec
moduleToSpec m mo = do
  moduleFileExists <- doesFileExist (moduleFilePath mo)
  unless moduleFileExists $ do
    let moduleName = getModuleName mo
    raiseError m $
      T.pack "could not find the module file for `"
        <> moduleName
        <> "`"
  Spec.parse $ moduleFilePath mo

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

getSourceFilePath :: Hint -> Module -> Spec -> FilePath -> IO (Path Abs File)
getSourceFilePath m mo spec relPathString = do
  filePath <- resolveFile (getSourceDir spec) relPathString
  ensureFileExistence m mo spec filePath
  return filePath

ensureFileExistence :: Hint -> Module -> Spec -> Path Abs File -> IO ()
ensureFileExistence m mo spec sourcePath = do
  b <- doesFileExist sourcePath
  unless b $ do
    let moduleName = getModuleName mo
    (_, pathInfo) <- pathToSection spec sourcePath
    raiseError m $ "the module `" <> moduleName <> "` does not have the component `" <> T.intercalate nsSep pathInfo <> "`"
