module Entity.Source where

import Control.Monad.Catch
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
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
  Map.member coreModulePrefix $ moduleDependency $ sourceModule source

isCompilationSkippable :: S.Set (Path Abs File) -> S.Set (Path Abs File) -> [OK.OutputKind] -> Source -> Bool
isCompilationSkippable hasLLVMSet hasObjectSet outputKindList source =
  case outputKindList of
    [] ->
      True
    kind : rest -> do
      case kind of
        OK.LLVM -> do
          let b1 = S.member (sourceFilePath source) hasLLVMSet
          let b2 = isCompilationSkippable hasLLVMSet hasObjectSet rest source
          b1 && b2
        OK.Asm ->
          isCompilationSkippable hasLLVMSet hasObjectSet rest source
        OK.Object -> do
          let b1 = S.member (sourceFilePath source) hasObjectSet
          let b2 = isCompilationSkippable hasLLVMSet hasObjectSet rest source
          b1 && b2
