module Main.Move.Scene.Module.Reflect
  ( Handle (..),
    new,
    fromFilePath,
    fromCurrentPath,
    findModuleFile,
    getCurrentModuleFilePath,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (MonadError (throwError), liftEither)
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Common.Rule.BaseName (isCapitalized)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.Error
import Language.Common.Rule.GlobalLocator qualified as GL
import Language.Common.Rule.Hint qualified as H
import Language.Common.Rule.ModuleAlias
import Language.Common.Rule.ModuleDigest
import Language.Common.Rule.ModuleID qualified as MID
import Language.Common.Rule.SourceLocator qualified as SL
import Language.RawTerm.Rule.Syntax.Series qualified as SE
import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Gensym qualified as Gensym
import Main.Move.Scene.Ens.Reflect qualified as Ens
import Main.Rule.ClangOption qualified as CL
import Main.Rule.Const (archiveRelDir, cacheRelDir, moduleFile, sourceRelDir)
import Main.Rule.Ens (dictFromListVertical')
import Main.Rule.Ens qualified as E
import Main.Rule.Module
import Main.Rule.ModuleURL
import Main.Rule.Target
import Main.Rule.ZenConfig (ZenConfig (..))
import Path
import Path.IO

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

fromFilePath :: Handle -> Path Abs File -> EIO Module
fromFilePath h moduleFilePath = do
  let h' = Ens.Handle {gensymHandle = gensymHandle h}
  (_, (ens@(m :< _), _)) <- Ens.fromFilePath h' moduleFilePath
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
  (mPrefix, prefixEns) <- liftEither $ E.access' keyPrefix E.emptyDict ens >>= E.toDictionary
  prefixMap <- liftEither $ interpretPrefixMap mPrefix prefixEns
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
        modulePrefixMap = prefixMap,
        moduleInlineLimit = mInlineLimit,
        modulePresetMap = presetMap
      }

fromCurrentPath :: Handle -> EIO Module
fromCurrentPath h = do
  getCurrentModuleFilePath >>= fromFilePath h

interpretPrefixMap ::
  H.Hint ->
  SE.Series (T.Text, E.Ens) ->
  Either Error (Map.HashMap BN.BaseName (ModuleAlias, SL.SourceLocator))
interpretPrefixMap m ens = do
  kvs' <- forM ens $ \(k, v) -> do
    k' <- BN.reflect m k
    unless (isCapitalized k') $ do
      Left $ newError m $ "Prefixes must be capitalized, but found: " <> BN.reify k'
    v' <- E.toString v >>= uncurry GL.reflectLocator
    return (k', v')
  return $ Map.fromList $ SE.extract kvs'

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

interpretTarget :: (H.Hint, SE.Series (T.Text, E.Ens)) -> EIO (Map.HashMap TargetName TargetSummary)
interpretTarget (_, targetDict) = do
  kvs <- forM (SE.extract targetDict) $ \(k, v) -> do
    entryPoint <- liftEither (E.access keyMain v) >>= interpretSourceLocator
    clangOption <- interpretClangOption v
    return (k, TargetSummary {entryPoint, clangOption})
  return $ Map.fromList kvs

interpretZenConfig :: E.Ens -> EIO ZenConfig
interpretZenConfig zenDict = do
  clangOption <- interpretClangOption zenDict
  return $ ZenConfig {clangOption}

interpretClangOption :: E.Ens -> EIO CL.ClangOption
interpretClangOption v = do
  (_, buildOptEnsSeries) <- liftEither $ E.access' keyBuildOption E.emptyList v >>= E.toList
  buildOption <- liftEither $ mapM (E.toString >=> return . snd) $ SE.extract buildOptEnsSeries
  (_, compileOptEnsSeries) <- liftEither $ E.access' keyCompileOption E.emptyList v >>= E.toList
  compileOption <- liftEither $ mapM (E.toString >=> return . snd) $ SE.extract compileOptEnsSeries
  (_, linkOptEnsSeries) <- liftEither $ E.access' keyLinkOption E.emptyList v >>= E.toList
  linkOption <- liftEither $ mapM (E.toString >=> return . snd) $ SE.extract linkOptEnsSeries
  return $ CL.new buildOption compileOption linkOption

interpretStaticFiles :: SE.Series (T.Text, E.Ens) -> EIO (Map.HashMap T.Text (Path Rel File))
interpretStaticFiles staticFileDict = do
  kvs <- forM (SE.extract staticFileDict) $ \(staticFileKey, staticFilePathEns) -> do
    (_, staticFilePathText) <- liftEither $ E.toString staticFilePathEns
    staticFilePath <- parseRelFile $ T.unpack staticFilePathText
    return (staticFileKey, staticFilePath)
  return $ Map.fromList kvs

interpretSourceLocator :: E.Ens -> EIO SL.SourceLocator
interpretSourceLocator ens = do
  (m, pathString) <- liftEither $ E.toString ens
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return $ SL.SourceLocator relPath
    Nothing ->
      throwError $ newError m $ "Invalid file path: " <> pathString

interpretRelFilePath :: E.Ens -> EIO (Path Rel File)
interpretRelFilePath ens = do
  (m, pathString) <- liftEither $ E.toString ens
  case parseRelFile $ T.unpack pathString of
    Just relPath ->
      return relPath
    Nothing ->
      throwError $ newError m $ "Invalid file path: " <> pathString

interpretDependencyDict ::
  (H.Hint, SE.Series (T.Text, E.Ens)) ->
  EIO (Map.HashMap ModuleAlias Dependency)
interpretDependencyDict (m, dep) = do
  items <- forM dep $ \(k, ens) -> do
    k' <- liftEither $ BN.reflect m k
    when (BN.isCapitalized k') $ do
      throwError $ newError m $ "Module aliases cannot be capitalized, but found: " <> BN.reify k'
    when (S.member k' BN.reservedAlias) $
      throwError $
        newError m $
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

interpretExtraPath :: Path Abs Dir -> E.Ens -> EIO (SomePath Rel)
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
  EIO Foreign
interpretForeignDict moduleRootDir ens = do
  (_, input) <- liftEither $ E.access keyForeignInput ens >>= E.toList
  (_, output) <- liftEither $ E.access keyForeignOutput ens >>= E.toList
  (_, script) <- liftEither $ E.access keyForeignScript ens >>= E.toList
  input' <- mapM (interpretExtraPath moduleRootDir) $ SE.extract input
  output' <- mapM interpretRelFilePath $ SE.extract output
  script' <- fmap (map snd . SE.extract) $ liftEither $ mapM E.toString script
  return $ Foreign {input = input', script = script', output = output'}

interpretAntecedent :: E.Ens -> EIO ModuleDigest
interpretAntecedent ens = do
  (_, digestText) <- liftEither $ E.toString ens
  return $ ModuleDigest digestText

interpretDirPath :: E.Ens -> EIO (Path Rel Dir)
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
  (Path Abs t -> EIO Bool) ->
  T.Text ->
  EIO ()
ensureExistence m moduleRootDir path existenceChecker kindText = do
  b <- existenceChecker (moduleRootDir </> path)
  unless b $ do
    throwError $ newError m $ "No such " <> kindText <> " exists: " <> T.pack (toFilePath path)

findModuleFile :: Path Abs Dir -> Path Abs Dir -> EIO (Path Abs File)
findModuleFile baseDir moduleRootDirCandidate = do
  let moduleFileCandidate = moduleRootDirCandidate </> moduleFile
  moduleFileExists <- doesFileExist moduleFileCandidate
  case (moduleFileExists, moduleRootDirCandidate /= parent moduleRootDirCandidate) of
    (True, _) ->
      return moduleFileCandidate
    (_, True) ->
      findModuleFile baseDir $ parent moduleRootDirCandidate
    _ ->
      throwError $ newError' $ "Could not find a module file (Context: " <> T.pack (toFilePath baseDir) <> ")"

getCurrentModuleFilePath :: EIO (Path Abs File)
getCurrentModuleFilePath = do
  baseDir <- getCurrentDir
  findModuleFile baseDir baseDir

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
