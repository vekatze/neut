module Kernel.Common.Module.FromPath
  ( fromFilePath,
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
import Ens.Ens qualified as E
import Ens.Parse qualified as Ens
import Kernel.Common.Allocator (Allocator (..), defaultAllocator, showAllocator)
import Kernel.Common.ClangOption qualified as CL
import Kernel.Common.Const (archiveRelDir, cacheRelDir, sourceRelDir)
import Kernel.Common.Module
import Kernel.Common.ModuleURL
import Kernel.Common.PlatformPattern qualified as PP
import Kernel.Common.PlatformRule
import Kernel.Common.Target (TargetSummary (..))
import Kernel.Common.TargetConfig (TargetConfig (..))
import Language.Common.BaseName qualified as BN
import Language.Common.ModuleAlias
import Language.Common.ModuleDigest
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Logger.Hint qualified as H
import Path
import SyntaxTree.Series qualified as SE

fromFilePath :: Path Abs File -> App Module
fromFilePath moduleFilePath = do
  (_, (ens, _)) <- Ens.fromFilePath moduleFilePath
  targetEns <- liftEither $ E.access' keyTarget E.emptyDict ens >>= E.toDictionary
  target <- interpretTarget targetEns
  zenRuleList <- interpretRuleList keyZen interpretTargetPlatformPattern interpretTargetConfig ens
  dependencyEns <- liftEither $ E.access' keyDependency E.emptyDict ens >>= E.toDictionary
  dependency <- interpretDependencyDict dependencyEns
  (_, extraContentsEns) <- liftEither $ E.access' keyExtraContent E.emptyList ens >>= E.toList
  extraContents <- mapM interpretSomePath $ SE.extract extraContentsEns
  (_, antecedentsEns) <- liftEither $ E.access' keyAntecedent E.emptyList ens >>= E.toList
  antecedents <- mapM interpretAntecedent $ SE.extract antecedentsEns
  (_, staticFileEns) <- liftEither $ E.access' keyStaticFile E.emptyDict ens >>= E.toDictionary
  staticFileMap <- interpretStaticFiles staticFileEns
  archiveDirEns <- liftEither $ E.access' keyArchive (E.ensPath archiveRelDir) ens
  archiveDir <- interpretDirPath archiveDirEns
  cacheDirEns <- liftEither $ E.access' keyCache (E.ensPath cacheRelDir) ens
  cacheDir <- interpretDirPath cacheDirEns
  sourceDirEns <- liftEither $ E.access' keySource (E.ensPath sourceRelDir) ens
  sourceDir <- interpretDirPath sourceDirEns
  foreignRuleList <- interpretRuleList keyForeign interpretPlatformPattern interpretForeignDict ens
  mInlineLimit <- interpretInlineLimit ens
  (_, universal) <- liftEither $ E.access' keyUniversal (E.Bool True) ens >>= E.toBool
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
        moduleZen = zenRuleList,
        moduleDependency = dependency,
        moduleExtraContents = extraContents,
        moduleAntecedents = antecedents,
        moduleLocation = moduleFilePath,
        moduleStaticFiles = staticFileMap,
        moduleForeign = foreignRuleList,
        moduleInlineLimit = mInlineLimit,
        moduleUniversal = universal,
        modulePresetMap = presetMap
      }

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
    platform <- interpretPlatform v
    config <- interpretTargetConfig v
    return (k, TargetSummary {entryPoint, targetPlatform = platform, targetConfig = config})
  return $ Map.fromList kvs

interpretExecuteCommand :: E.Ens -> App (Maybe (H.Hint, T.Text))
interpretExecuteCommand ens = do
  if E.hasKey keyExecute ens
    then Just <$> liftEither (E.access keyExecute ens >>= E.toString)
    else return Nothing

interpretTextList :: T.Text -> E.Ens -> App [T.Text]
interpretTextList key ens = do
  (_, textEnsSeries) <- liftEither $ E.access' key E.emptyList ens >>= E.toList
  liftEither $ mapM (E.toString >=> return . snd) $ SE.extract textEnsSeries

interpretPlatform :: E.Ens -> App (H.Hint, PP.PlatformPattern)
interpretPlatform ens =
  if E.hasKey keyPlatform ens
    then liftEither (E.access keyPlatform ens) >>= interpretTargetPlatformPattern
    else return defaultPlatform

interpretPlatformPattern :: E.Ens -> App (H.Hint, PP.PlatformPattern)
interpretPlatformPattern ens = do
  (m, patternText) <- liftEither $ E.toString ens
  case PP.reflect patternText of
    Nothing ->
      raiseError m $ "Invalid platform pattern: " <> patternText
    Just platformPattern
      | not (PP.matchesSomePlatform platformPattern) ->
          raiseError m $ "No platform matches the pattern: " <> patternText
      | otherwise ->
          return (m, platformPattern)

interpretTargetPlatformPattern :: E.Ens -> App (H.Hint, PP.PlatformPattern)
interpretTargetPlatformPattern ens = do
  (m, platformPattern) <- interpretPlatformPattern ens
  unless (PP.determinesTarget platformPattern) $
    raiseError m $
      "The target part of the platform pattern is not determined by its host part: " <> PP.reify platformPattern
  return (m, platformPattern)

interpretRuleList ::
  T.Text ->
  (E.Ens -> App (H.Hint, PP.PlatformPattern)) ->
  (E.Ens -> App a) ->
  E.Ens ->
  App [PlatformRule a]
interpretRuleList key interpretPattern interpretBody ens = do
  (_, ruleSeries) <- liftEither $ E.access' key E.emptyList ens >>= E.toList
  forM (SE.extract ruleSeries) $ \ruleEns -> do
    rulePlatform <- liftEither (E.access keyPlatform ruleEns) >>= interpretPattern
    ruleBody <- interpretBody ruleEns
    return $ PlatformRule {rulePlatform, ruleBody}

interpretAllocator :: E.Ens -> App Allocator
interpretAllocator ens = do
  allocatorEns <- liftEither $ E.access' keyAllocator (E.String $ showAllocator defaultAllocator) ens
  (_, allocatorText) <- liftEither $ E.toString allocatorEns
  case allocatorText of
    "mimalloc" ->
      return Mimalloc
    "system" ->
      return System
    _ ->
      return defaultAllocator

interpretTargetConfig :: E.Ens -> App TargetConfig
interpretTargetConfig ens = do
  clangOption <- interpretClangOption ens
  allocator <- interpretAllocator ens
  executeCommand <- interpretExecuteCommand ens
  return $ TargetConfig {clangOption, allocator, executeCommand}

interpretClangOption :: E.Ens -> App CL.ClangOption
interpretClangOption v = do
  buildOption <- interpretTextList keyBuildOption v
  compileOption <- interpretTextList keyCompileOption v
  linkOption <- interpretTextList keyLinkOption v
  return $ CL.new buildOption compileOption linkOption

interpretStaticFiles :: SE.Series (T.Text, E.Ens) -> App (Map.HashMap T.Text (H.Hint, Path Rel File))
interpretStaticFiles staticFileDict = do
  kvs <- forM (SE.extract staticFileDict) $ \(staticFileKey, staticFilePathEns) -> do
    (m, staticFilePathText) <- liftEither $ E.toString staticFilePathEns
    staticFilePath <- interpretRelFile m staticFilePathText
    return (staticFileKey, (m, staticFilePath))
  return $ Map.fromList kvs

interpretSourceLocator :: E.Ens -> App SL.SourceLocator
interpretSourceLocator ens = do
  (m, pathString) <- liftEither $ E.toString ens
  case parseRelFile $ T.unpack pathString of
    Just relPath
      | Just sourceLocator <- SL.fromPath relPath ->
          return sourceLocator
    Nothing ->
      raiseError m $ "Invalid file path: " <> pathString
    _ ->
      raiseError m $ "The source path `" <> pathString <> "` contains the reserved segment `this`"

interpretRelFile :: H.Hint -> T.Text -> App (Path Rel File)
interpretRelFile m pathText =
  case parseRelFile $ T.unpack pathText of
    Just relPath ->
      return relPath
    Nothing ->
      raiseError m $ "Invalid file path: " <> pathText

interpretRelDir :: H.Hint -> T.Text -> App (Path Rel Dir)
interpretRelDir m pathText =
  case parseRelDir $ T.unpack pathText of
    Just relPath ->
      return relPath
    Nothing ->
      raiseError m $ "Invalid directory path: " <> pathText

interpretDependencyDict ::
  (H.Hint, SE.Series (T.Text, E.Ens)) ->
  App (Map.HashMap ModuleAlias Dependency)
interpretDependencyDict (m, dep) = do
  items <- forM dep $ \(k, ens) -> do
    k' <- interpretModuleAlias m k
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
  let itemList = SE.extract items
  ensureNoDependencyAliasCollision m itemList
  return $ Map.fromList itemList

interpretModuleAlias :: H.Hint -> T.Text -> App BN.BaseName
interpretModuleAlias m rawAlias = do
  alias <- liftEither $ BN.reflect m rawAlias
  when (BN.isCapitalized alias) $ do
    raiseError m $ "Module aliases cannot be capitalized, but found: " <> BN.reify alias
  when (S.member alias BN.reservedAlias) $
    raiseError m $
      "The reserved name `"
        <> BN.reify alias
        <> "` cannot be used as an alias of a module"
  return alias

ensureNoDependencyAliasCollision :: H.Hint -> [(ModuleAlias, Dependency)] -> App ()
ensureNoDependencyAliasCollision m itemList = do
  let directAliasList = map fst itemList
  ensureNoDuplicateAliases m directAliasList

ensureNoDuplicateAliases :: H.Hint -> [ModuleAlias] -> App ()
ensureNoDuplicateAliases m aliasList =
  ensureNoDuplicateAliasTexts m S.empty $ map (BN.reify . extract) aliasList

ensureNoDuplicateAliasTexts :: H.Hint -> S.Set T.Text -> [T.Text] -> App ()
ensureNoDuplicateAliasTexts m seen aliasList =
  case aliasList of
    [] ->
      return ()
    alias : rest -> do
      when (S.member alias seen) $
        raiseError m $
          "Duplicate module alias: " <> alias
      ensureNoDuplicateAliasTexts m (S.insert alias seen) rest

interpretSomePath :: E.Ens -> App (H.Hint, SomePath Rel)
interpretSomePath entity = do
  (m, itemPathText) <- liftEither $ E.toString entity
  path <-
    if T.isSuffixOf "/" itemPathText
      then Left <$> interpretRelDir m itemPathText
      else Right <$> interpretRelFile m itemPathText
  return (m, path)

interpretForeignDict :: E.Ens -> App Foreign
interpretForeignDict ens = do
  (_, input) <- liftEither $ E.access' keyForeignInput E.emptyList ens >>= E.toList
  (_, output) <- liftEither $ E.access' keyForeignOutput E.emptyList ens >>= E.toList
  (_, linkedOutput) <- liftEither $ E.access' keyForeignLink E.emptyList ens >>= E.toList
  (_, script) <- liftEither $ E.access' keyForeignScript E.emptyList ens >>= E.toList
  input' <- mapM interpretSomePath $ SE.extract input
  output' <- map snd <$> mapM interpretSomePath (SE.extract output)
  linkedOutput' <- mapM (interpretLinkedOutput output') $ SE.extract linkedOutput
  transitiveLinkOption <- interpretTextList keyForeignTransitiveLinkOption ens
  script' <- fmap SE.extract $ liftEither $ mapM E.toString script
  return $
    Foreign
      { input = input',
        output = output',
        linkedOutput = linkedOutput',
        transitiveLinkOption,
        script = script'
      }

interpretLinkedOutput :: [SomePath Rel] -> E.Ens -> App (Path Rel File)
interpretLinkedOutput outputList ens = do
  (m, pathText) <- liftEither $ E.toString ens
  path <- interpretRelFile m pathText
  if any (containsOutput path) outputList
    then return path
    else raiseError m $ "Not in the foreign output: " <> pathText

containsOutput :: Path Rel File -> SomePath Rel -> Bool
containsOutput path output =
  case output of
    Left dirPath ->
      isProperPrefixOf dirPath path
    Right filePath ->
      filePath == path

interpretAntecedent :: E.Ens -> App ModuleDigest
interpretAntecedent ens = do
  (_, digestText) <- liftEither $ E.toString ens
  return $ ModuleDigest digestText

interpretDirPath :: E.Ens -> App (Path Rel Dir)
interpretDirPath ens = do
  (m, pathText) <- liftEither $ E.toString ens
  interpretRelDir m pathText

interpretInlineLimit :: E.Ens -> App (Maybe Int)
interpretInlineLimit ens =
  if not (E.hasKey keyInlineLimit ens)
    then return Nothing
    else do
      limitEns@(m :< _) <- liftEither $ E.access keyInlineLimit ens
      limit <- liftEither $ E.toInt limitEns
      if limit < 0
        then raiseError m $ "The inline limit must not be negative, but is: " <> T.pack (show limit)
        else return $ Just limit
