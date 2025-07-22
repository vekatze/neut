module Kernel.Common.Module.FromPath
  ( fromFilePath,
    fromCurrentPath,
  )
where

import App.App (App)
import App.Error
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (liftEither)
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Ens.Ens (dictFromListVertical')
import Ens.Ens qualified as E
import Ens.Parse qualified as Ens
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.Const (archiveRelDir, cacheRelDir, sourceRelDir)
import Kernel.Common.Module
import Kernel.Common.Module.FindModuleFile
import Kernel.Common.ModuleURL
import Kernel.Common.Target
import Kernel.Common.ZenConfig (ZenConfig (..))
import Language.Common.BaseName qualified as BN
import Language.Common.ModuleAlias
import Language.Common.ModuleDigest
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Logger.Hint qualified as H
import Path
import Path.IO
import SyntaxTree.Series qualified as SE

fromFilePath :: Path Abs File -> App Module
fromFilePath moduleFilePath = do
  (_, (ens@(m :< _), _)) <- Ens.fromFilePath moduleFilePath
  targetEns <- liftEither $ E.access' keyTarget E.emptyDict ens >>= E.toDictionary
  target <- interpretTarget targetEns
  zenConfigEns <- liftEither (E.access' keyZen E.emptyDict ens) >>= interpretZenConfig
  dependencyEns <- liftEither $ E.access' keyDependency E.emptyDict ens >>= E.toDictionary
  dependency <- interpretDependencyDict dependencyEns
  (_, extraContentsEns) <- liftEither $ E.access' keyExtraContent E.emptyList ens >>= E.toList
  extraContents <- mapM (interpretExtraPath $ parent moduleFilePath) $ SE.extract extraContentsEns
  (_, antecedentsEns) <- liftEither $ E.access' keyAntecedent E.emptyList ens >>= E.toList
  antecedents <- mapM interpretAntecedent $ SE.extract antecedentsEns
  (_, staticFileEns) <- liftEither $ E.access' keyStatic E.emptyDict ens >>= E.toDictionary
  staticFileMap <- interpretStaticFiles staticFileEns
  archiveDirEns <- liftEither $ E.access' keyArchive (E.ensPath archiveRelDir) ens
  archiveDir <- interpretDirPath archiveDirEns
  cacheDirEns <- liftEither $ E.access' keyCache (E.ensPath cacheRelDir) ens
  cacheDir <- interpretDirPath cacheDirEns
  sourceDirEns <- liftEither $ E.access' keySource (E.ensPath sourceRelDir) ens
  sourceDir <- interpretDirPath sourceDirEns
  foreignDictEns <- liftEither $ E.access' keyForeign (emptyForeign m) ens
  foreignDict <- interpretForeignDict (parent moduleFilePath) foreignDictEns
  let mInlineLimit = interpretInlineLimit $ E.access keyInlineLimit ens
  (mPreset, presetEns) <- liftEither $ E.access' keyPreset E.emptyDict ens >>= E.toDictionary
  presetMap <- liftEither $ interpretPresetMap mPreset presetEns
  let isLibrary = E.hasKey keyAntecedent ens
  return
    Module
      { moduleID = if isLibrary then getDigestFromModulePath moduleFilePath else MID.Main,
        moduleArchiveDir = archiveDir,
        moduleCacheDir = cacheDir,
        moduleSourceDir = sourceDir,
        moduleTarget = target,
        moduleZenConfig = zenConfigEns,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleAntecedents = antecedents,
        moduleLocation = moduleFilePath,
        moduleStaticFiles = staticFileMap,
        moduleForeign = foreignDict,
        moduleInlineLimit = mInlineLimit,
        modulePresetMap = presetMap
      }

fromCurrentPath :: App Module
fromCurrentPath = do
  getCurrentModuleFilePath >>= fromFilePath

interpretPresetMap ::
  H.Hint ->
  SE.Series (T.Text, E.Ens) ->
  Either Error (Map.HashMap LocatorName [BN.BaseName])
interpretPresetMap _ ens = do
  kvs <- forM (SE.extract ens) $ \(k, v) -> do
    (_, presetSeries) <- E.toList v
    v' <- mapM (E.toString >=> uncurry BN.reflect) $ SE.extract presetSeries
    return (k, v')
  return $ Map.fromList kvs

interpretTarget :: (H.Hint, SE.Series (T.Text, E.Ens)) -> App (Map.HashMap TargetName TargetSummary)
interpretTarget (_, targetDict) = do
  kvs <- forM (SE.extract targetDict) $ \(k, v) -> do
    entryPoint <- liftEither (E.access keyMain v) >>= interpretSourceLocator
    clangOption <- interpretClangOption v
    return (k, TargetSummary {entryPoint, clangOption})
  return $ Map.fromList kvs

interpretZenConfig :: E.Ens -> App ZenConfig
interpretZenConfig zenDict = do
  clangOption <- interpretClangOption zenDict
  return $ ZenConfig {clangOption}

interpretClangOption :: E.Ens -> App CL.ClangOption
interpretClangOption v = do
  (_, buildOptEnsSeries) <- liftEither $ E.access' keyBuildOption E.emptyList v >>= E.toList
  buildOption <- liftEither $ mapM (E.toString >=> return . snd) $ SE.extract buildOptEnsSeries
  (_, compileOptEnsSeries) <- liftEither $ E.access' keyCompileOption E.emptyList v >>= E.toList
  compileOption <- liftEither $ mapM (E.toString >=> return . snd) $ SE.extract compileOptEnsSeries
  (_, linkOptEnsSeries) <- liftEither $ E.access' keyLinkOption E.emptyList v >>= E.toList
  linkOption <- liftEither $ mapM (E.toString >=> return . snd) $ SE.extract linkOptEnsSeries
  return $ CL.new buildOption compileOption linkOption

interpretStaticFiles :: SE.Series (T.Text, E.Ens) -> App (Map.HashMap T.Text (Path Rel File))
interpretStaticFiles staticFileDict = do
  kvs <- forM (SE.extract staticFileDict) $ \(staticFileKey, staticFilePathEns) -> do
    (_, staticFilePathText) <- liftEither $ E.toString staticFilePathEns
    staticFilePath <- parseRelFile $ T.unpack staticFilePathText
    return (staticFileKey, staticFilePath)
  return $ Map.fromList kvs

interpretSourceLocator :: E.Ens -> App SL.SourceLocator
interpretSourceLocator ens = do
  (m, pathString) <- liftEither $ E.toString ens
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return $ SL.SourceLocator relPath
    Nothing ->
      raiseError m $ "Invalid file path: " <> pathString

interpretRelFilePath :: E.Ens -> App (Path Rel File)
interpretRelFilePath ens = do
  (m, pathString) <- liftEither $ E.toString ens
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return relPath
    Nothing ->
      raiseError m $ "Invalid file path: " <> pathString

interpretDependencyDict ::
  (H.Hint, SE.Series (T.Text, E.Ens)) ->
  App (Map.HashMap ModuleAlias Dependency)
interpretDependencyDict (m, dep) = do
  items <- forM dep $ \(k, ens) -> do
    k' <- liftEither $ BN.reflect m k
    when (BN.isCapitalized k') $ do
      raiseError m $ "Module aliases cannot be capitalized, but found: " <> BN.reify k'
    when (S.member k' BN.reservedAlias) $
      raiseError m $
        "The reserved name `"
          <> BN.reify k'
          <> "` cannot be used as an alias of a module"
    (_, urlEnsSeries) <- liftEither $ E.access keyMirror ens >>= E.toList
    urlList <- liftEither $ mapM (E.toString >=> return . snd) $ SE.extract urlEnsSeries
    (_, digest) <- liftEither $ E.access keyDigest ens >>= E.toString
    (_, enablePreset) <- liftEither $ E.access' keyEnablePreset (E.Bool False) ens >>= E.toBool
    let mirrorList = map ModuleURL urlList
    let digest' = ModuleDigest digest
    return
      ( ModuleAlias k',
        Dependency
          { dependencyMirrorList = mirrorList,
            dependencyDigest = digest',
            dependencyPresetEnabled = enablePreset
          }
      )
  return $ Map.fromList $ SE.extract items

interpretExtraPath :: Path Abs Dir -> E.Ens -> App (SomePath Rel)
interpretExtraPath moduleRootDir entity = do
  (m, itemPathText) <- liftEither $ E.toString entity
  if T.last itemPathText == '/'
    then do
      dirPath <- parseRelDir $ T.unpack itemPathText
      ensureExistence m moduleRootDir dirPath doesDirExist "directory"
      return $ Left dirPath
    else do
      filePath <- parseRelFile $ T.unpack itemPathText
      ensureExistence m moduleRootDir filePath doesFileExist "file"
      return $ Right filePath

interpretForeignDict ::
  Path Abs Dir ->
  E.Ens ->
  App Foreign
interpretForeignDict moduleRootDir ens = do
  (_, input) <- liftEither $ E.access keyForeignInput ens >>= E.toList
  (_, output) <- liftEither $ E.access keyForeignOutput ens >>= E.toList
  (_, script) <- liftEither $ E.access keyForeignScript ens >>= E.toList
  input' <- mapM (interpretExtraPath moduleRootDir) $ SE.extract input
  output' <- mapM interpretRelFilePath $ SE.extract output
  script' <- fmap (map snd . SE.extract) $ liftEither $ mapM E.toString script
  return $ Foreign {input = input', script = script', output = output'}

interpretAntecedent :: E.Ens -> App ModuleDigest
interpretAntecedent ens = do
  (_, digestText) <- liftEither $ E.toString ens
  return $ ModuleDigest digestText

interpretDirPath :: E.Ens -> App (Path Rel Dir)
interpretDirPath ens = do
  (_, pathText) <- liftEither $ E.toString ens
  parseRelDir $ T.unpack pathText

interpretInlineLimit :: Either Error E.Ens -> Maybe Int
interpretInlineLimit errOrEns = do
  ens <- rightToMaybe errOrEns
  rightToMaybe $ E.toInt ens

ensureExistence ::
  H.Hint ->
  Path Abs Dir ->
  Path Rel t ->
  (Path Abs t -> App Bool) ->
  T.Text ->
  App ()
ensureExistence m moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker (moduleRootDir </> path)
  unless b $ do
    raiseError m $ "No such " <> kindText <> " exists: " <> T.pack (toFilePath path)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe errOrVal =
  case errOrVal of
    Left _ ->
      Nothing
    Right val ->
      Just val

emptyForeign :: H.Hint -> E.EnsF E.Ens
emptyForeign m =
  dictFromListVertical'
    [ (keyForeignInput, m :< E.List (SE.emptySeries (Just SE.Brace) SE.Comma)),
      (keyForeignOutput, m :< E.List (SE.emptySeries (Just SE.Brace) SE.Comma)),
      (keyForeignScript, m :< E.List (SE.emptySeries (Just SE.Brace) SE.Comma))
    ]
