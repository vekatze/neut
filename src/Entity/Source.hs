module Entity.Source where

import Control.Monad.Catch
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Entity.Artifact qualified as A
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Hint
import Entity.Module qualified as M
import Entity.ModuleAlias
import Entity.ModuleID qualified as MID
import Entity.OutputKind qualified as OK
import Path

data Source = Source
  { sourceFilePath :: Path Abs File,
    sourceModule :: M.Module,
    sourceHint :: Maybe Hint
  }
  deriving (Show)

getRelPathFromSourceDir :: (MonadThrow m) => Source -> m (Path Rel File)
getRelPathFromSourceDir source = do
  M.getRelPathFromSourceDir (sourceModule source) (sourceFilePath source)

getBaseReadableLocator :: (MonadThrow m) => Source -> m T.Text
getBaseReadableLocator source = do
  relPath <- getRelPathFromSourceDir source
  (relPathWithoutExtension, _) <- splitExtension relPath
  return $ T.replace "/" nsSep $ T.pack $ toFilePath relPathWithoutExtension

getHumanReadableLocator :: (MonadThrow m) => M.Module -> Source -> m (NE.NonEmpty T.Text)
getHumanReadableLocator baseModule source = do
  let sourceModuleID = M.moduleID $ sourceModule source
  baseReadableLocator <- getBaseReadableLocator source
  case sourceModuleID of
    MID.Main -> do
      return $ NE.singleton $ "this" <> nsSep <> baseReadableLocator
    MID.Base -> do
      return $ NE.singleton $ "base" <> nsSep <> baseReadableLocator
    MID.Library digest -> do
      let digestMap = M.getDigestMap baseModule
      case Map.lookup digest digestMap of
        Nothing ->
          return $ NE.singleton $ "{unknown}" <> nsSep <> baseReadableLocator
        Just aliasList -> do
          let aliasList' = NE.map (BN.reify . extract) aliasList
          return $ NE.map (\x -> x <> nsSep <> baseReadableLocator) aliasList'

attachExtension :: (MonadThrow m) => Path Abs File -> OK.OutputKind -> m (Path Abs File)
attachExtension file kind =
  case kind of
    OK.LLVM -> do
      addExtension ".ll" file
    OK.Object -> do
      addExtension ".o" file

hasCore :: Source -> Bool
hasCore source =
  Map.member coreModuleAlias $ M.moduleDependency $ sourceModule source

isCompilationSkippable ::
  A.ArtifactTime ->
  [OK.OutputKind] ->
  Bool
isCompilationSkippable artifactTime outputKindList =
  case outputKindList of
    [] ->
      True
    kind : rest -> do
      case kind of
        OK.LLVM -> do
          let b1 = isJust $ A.llvmTime artifactTime
          let b2 = isCompilationSkippable artifactTime rest
          b1 && b2
        OK.Object -> do
          let b1 = isJust $ A.objectTime artifactTime
          let b2 = isCompilationSkippable artifactTime rest
          b1 && b2
