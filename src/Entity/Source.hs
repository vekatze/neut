module Entity.Source where

import Control.Monad.Catch
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Entity.Artifact qualified as A
import Entity.Hint
import Entity.Module
import Entity.ModuleAlias
import Entity.OutputKind qualified as OK
import Path

data Source = Source
  { sourceFilePath :: Path Abs File,
    sourceModule :: Module,
    sourceHint :: Maybe Hint
  }
  deriving (Show)

getRelPathFromSourceDir :: MonadThrow m => Source -> m (Path Rel File)
getRelPathFromSourceDir source = do
  let sourceDir = getSourceDir $ sourceModule source
  stripProperPrefix sourceDir (sourceFilePath source)

attachExtension :: MonadThrow m => Path Abs File -> OK.OutputKind -> m (Path Abs File)
attachExtension file kind =
  case kind of
    OK.LLVM -> do
      addExtension ".ll" file
    OK.Asm -> do
      addExtension ".s" file
    OK.Object -> do
      addExtension ".o" file

hasCore :: Source -> Bool
hasCore source =
  Map.member coreModuleAlias $ moduleDependency $ sourceModule source

isCompilationSkippable ::
  A.ArtifactTime ->
  [OK.OutputKind] ->
  Source ->
  Bool
isCompilationSkippable artifactTime outputKindList source =
  case outputKindList of
    [] ->
      True
    kind : rest -> do
      case kind of
        OK.LLVM -> do
          let b1 = isJust $ A.llvmTime artifactTime
          let b2 = isCompilationSkippable artifactTime rest source
          b1 && b2
        OK.Asm -> do
          let b1 = isJust $ A.asmTime artifactTime
          let b2 = isCompilationSkippable artifactTime rest source
          b1 && b2
        OK.Object -> do
          let b1 = isJust $ A.objectTime artifactTime
          let b2 = isCompilationSkippable artifactTime rest source
          b1 && b2
