module Scene.Unravel
  ( unravel,
    unravelFromSGL,
  )
where

import Context.Alias qualified as Alias
import Context.Antecedent qualified as Antecedent
import Context.App
import Context.Env qualified as Env
import Context.Locator qualified as Locator
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Context.Unravel qualified as Unravel
import Control.Monad
import Data.Foldable
import Data.HashMap.Strict qualified as Map
import Data.List (unzip4)
import Data.Sequence as Seq
  ( Seq,
    empty,
    (><),
    (|>),
  )
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Hint
import Entity.Module
import Entity.OutputKind qualified as OK
import Entity.Source qualified as Source
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target
import Entity.VisitInfo qualified as VI
import Path
import Scene.Parse.Core qualified as ParseCore
import Scene.Parse.Export
import Scene.Parse.Import
import Scene.Source.ShiftToLatest qualified as Source

type IsCacheAvailable =
  Bool

type IsObjectAvailable =
  Bool

type IsLLVMAvailable =
  Bool

unravel :: Target -> App (IsCacheAvailable, IsLLVMAvailable, IsObjectAvailable, [Source.Source])
unravel target = do
  mainModule <- Module.getMainModule
  mainFilePath <- resolveTarget mainModule target >>= Module.getSourcePath
  unravel' >=> adjustUnravelResult $
    Source.Source
      { Source.sourceModule = mainModule,
        Source.sourceFilePath = mainFilePath
      }

unravelFromSGL ::
  SGL.StrictGlobalLocator ->
  App (IsCacheAvailable, IsLLVMAvailable, IsObjectAvailable, [Source.Source])
unravelFromSGL sgl = do
  mainModule <- Module.getMainModule
  path <- Module.getSourcePath sgl
  ensureFileModuleSanity path mainModule
  let initialSource = Source.Source {Source.sourceModule = mainModule, Source.sourceFilePath = path}
  unravel' >=> adjustUnravelResult $ initialSource

adjustUnravelResult ::
  (IsCacheAvailable, IsLLVMAvailable, IsObjectAvailable, Seq Source.Source) ->
  App (IsCacheAvailable, IsLLVMAvailable, IsObjectAvailable, [Source.Source])
adjustUnravelResult (b1, b2, b3, sourceSeq) = do
  let sourceList = toList sourceSeq
  registerAntecedentInfo sourceList
  sourceList' <- mapM Source.shiftToLatest sourceList
  return (b1, b2, b3, sanitizeSourceList sourceList')

ensureFileModuleSanity :: Path Abs File -> Module -> App ()
ensureFileModuleSanity filePath mainModule = do
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    Throw.raiseError' "the specified file is not in the current module"

resolveTarget :: Module -> Target -> App SGL.StrictGlobalLocator
resolveTarget mainModule target = do
  case Map.lookup target (moduleTarget mainModule) of
    Just path ->
      return path
    Nothing ->
      Throw.raiseError' $ "no such target is defined: `" <> extract target <> "`"

unravel' :: Source.Source -> App (IsCacheAvailable, IsLLVMAvailable, IsObjectAvailable, Seq Source.Source)
unravel' source = do
  visitEnv <- Unravel.getVisitEnv
  let path = Source.sourceFilePath source
  case Map.lookup path visitEnv of
    Just VI.Active ->
      raiseCyclicPath source
    Just VI.Finish -> do
      hasCacheSet <- Env.getHasCacheSet
      hasLLVMSet <- Env.getHasObjectSet
      hasObjectSet <- Env.getHasObjectSet
      return (path `S.member` hasCacheSet, path `S.member` hasLLVMSet, path `S.member` hasObjectSet, Seq.empty)
    Nothing -> do
      Unravel.insertToVisitEnv path VI.Active
      Unravel.pushToTraceSourceList source
      children <- getChildren source
      (isCacheAvailableList, isLLVMAvailableList, isObjectAvailableList, seqList) <- unzip4 <$> mapM unravel' children
      _ <- Unravel.popFromTraceSourceList
      Unravel.insertToVisitEnv path VI.Finish
      isCacheAvailable <- checkIfCacheIsAvailable isCacheAvailableList source
      isLLVMAvailable <- checkIfLLVMIsAvailable isLLVMAvailableList source
      isObjectAvailable <- checkIfObjectIsAvailable isObjectAvailableList source
      return (isCacheAvailable, isLLVMAvailable, isObjectAvailable, foldl' (><) Seq.empty seqList |> source)

checkIfCacheIsAvailable :: [IsCacheAvailable] -> Source.Source -> App IsCacheAvailable
checkIfCacheIsAvailable = do
  checkIfItemIsAvailable isFreshCacheAvailable Env.insertToHasCacheSet

checkIfLLVMIsAvailable :: [IsLLVMAvailable] -> Source.Source -> App IsLLVMAvailable
checkIfLLVMIsAvailable = do
  checkIfItemIsAvailable isFreshLLVMAvailable Env.insertToHasLLVMSet

checkIfObjectIsAvailable :: [IsObjectAvailable] -> Source.Source -> App IsObjectAvailable
checkIfObjectIsAvailable = do
  checkIfItemIsAvailable isFreshObjectAvailable Env.insertToHasObjectSet

checkIfItemIsAvailable ::
  (Source.Source -> App Bool) ->
  (Path Abs File -> App ()) ->
  [IsObjectAvailable] ->
  Source.Source ->
  App IsObjectAvailable
checkIfItemIsAvailable isFreshItemAvailable inserter isObjectAvailableList source = do
  b <- isFreshItemAvailable source
  let isObjectAvailable = and $ b : isObjectAvailableList
  when isObjectAvailable $ inserter $ Source.sourceFilePath source
  return isObjectAvailable

isFreshCacheAvailable :: Source.Source -> App Bool
isFreshCacheAvailable source = do
  cachePath <- Path.getSourceCachePath source
  isItemAvailable source cachePath

isFreshLLVMAvailable :: Source.Source -> App Bool
isFreshLLVMAvailable source = do
  llvmPath <- Path.sourceToOutputPath OK.LLVM source
  isItemAvailable source llvmPath

isFreshObjectAvailable :: Source.Source -> App Bool
isFreshObjectAvailable source = do
  objectPath <- Path.sourceToOutputPath OK.Object source
  isItemAvailable source objectPath

isItemAvailable :: Source.Source -> Path Abs File -> App Bool
isItemAvailable source itemPath = do
  existsItem <- Path.doesFileExist itemPath
  if not existsItem
    then return False
    else do
      srcModTime <- Path.getModificationTime $ Source.sourceFilePath source
      itemModTime <- Path.getModificationTime itemPath
      return $ itemModTime > srcModTime

raiseCyclicPath :: Source.Source -> App a
raiseCyclicPath source = do
  traceSourceList <- Unravel.getTraceSourceList
  let m = Entity.Hint.new 1 1 $ toFilePath $ Source.sourceFilePath source
  let cyclicPathList = map Source.sourceFilePath $ reverse $ source : traceSourceList
  Throw.raiseError m $ "found a cyclic inclusion:\n" <> showCyclicPath cyclicPathList

showCyclicPath :: [Path Abs File] -> T.Text
showCyclicPath pathList =
  case pathList of
    [] ->
      ""
    [path] ->
      T.pack (toFilePath path)
    path : ps ->
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

getChildren :: Source.Source -> App [Source.Source]
getChildren currentSource = do
  Env.setCurrentSource currentSource
  Alias.initializeAliasMap
  sourceChildrenMap <- Unravel.getSourceChildrenMap
  let currentSourceFilePath = Source.sourceFilePath currentSource
  case Map.lookup currentSourceFilePath sourceChildrenMap of
    Just sourceList ->
      return sourceList
    Nothing -> do
      let path = Source.sourceFilePath currentSource
      sourceList <- parseSourceHeader currentSource path
      Unravel.insertToSourceChildrenMap currentSourceFilePath sourceList
      return sourceList

parseSourceHeader :: Source.Source -> Path Abs File -> App [Source.Source]
parseSourceHeader currentSource path = do
  Locator.initialize
  map fst <$> ParseCore.run (parseImportBlock currentSource <* parseExportBlock) path

registerAntecedentInfo :: [Source.Source] -> App ()
registerAntecedentInfo sourceList =
  forM_ sourceList $ \source -> do
    let newModule = Source.sourceModule source
    let antecedents = moduleAntecedents newModule
    forM_ antecedents $ \antecedent ->
      Antecedent.insert antecedent newModule

sanitizeSourceList :: [Source.Source] -> [Source.Source]
sanitizeSourceList =
  sanitizeSourceList' S.empty

sanitizeSourceList' :: S.Set (Path Abs File) -> [Source.Source] -> [Source.Source]
sanitizeSourceList' pathSet sourceList =
  case sourceList of
    [] ->
      []
    source : rest -> do
      let path = Source.sourceFilePath source
      if S.member path pathSet
        then sanitizeSourceList' pathSet rest
        else source : sanitizeSourceList' (S.insert path pathSet) rest
