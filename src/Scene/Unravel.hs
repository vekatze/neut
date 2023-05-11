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
import Context.Parse qualified as Parse
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Context.Unravel qualified as Unravel
import Control.Monad
import Data.Foldable
import Data.HashMap.Strict qualified as Map
import Data.Sequence as Seq (Seq, empty, (><), (|>))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time
import Entity.Artifact qualified as A
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

type CacheTime =
  Maybe UTCTime

type LLVMTime =
  Maybe UTCTime

type AsmTime =
  Maybe UTCTime

type ObjectTime =
  Maybe UTCTime

unravel :: Target -> App (A.ArtifactTime, [Source.Source])
unravel target = do
  mainModule <- Module.getMainModule
  mainFilePath <- resolveTarget mainModule target >>= Module.getSourcePath
  unravel' >=> mapM adjustUnravelResult $
    Source.Source
      { Source.sourceModule = mainModule,
        Source.sourceFilePath = mainFilePath,
        Source.sourceHint = Nothing
      }

unravelFromSGL ::
  SGL.StrictGlobalLocator ->
  App (A.ArtifactTime, [Source.Source])
unravelFromSGL sgl = do
  mainModule <- Module.getMainModule
  path <- Module.getSourcePath sgl
  ensureFileModuleSanity path mainModule
  let initialSource =
        Source.Source
          { Source.sourceModule = mainModule,
            Source.sourceFilePath = path,
            Source.sourceHint = Nothing
          }
  unravel' >=> mapM adjustUnravelResult $ initialSource

adjustUnravelResult ::
  Seq Source.Source ->
  App [Source.Source]
adjustUnravelResult sourceSeq = do
  let sourceList = toList sourceSeq
  registerAntecedentInfo sourceList
  sourceList' <- mapM Source.shiftToLatest sourceList
  return $ sanitizeSourceList sourceList'

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

unravel' :: Source.Source -> App (A.ArtifactTime, Seq Source.Source)
unravel' source = do
  visitEnv <- Unravel.getVisitEnv
  let path = Source.sourceFilePath source
  case Map.lookup path visitEnv of
    Just VI.Active ->
      raiseCyclicPath source
    Just VI.Finish -> do
      artifactTime <- Env.lookupArtifactTime path
      return (artifactTime, Seq.empty)
    Nothing -> do
      Unravel.insertToVisitEnv path VI.Active
      Unravel.pushToTraceSourceList source
      children <- getChildren source
      (artifactTimeList, seqList) <- mapAndUnzipM unravel' children
      _ <- Unravel.popFromTraceSourceList
      Unravel.insertToVisitEnv path VI.Finish
      artifactTime <- getArtifactTime artifactTimeList source
      return (artifactTime, foldl' (><) Seq.empty seqList |> source)

getArtifactTime :: [A.ArtifactTime] -> Source.Source -> App A.ArtifactTime
getArtifactTime artifactTimeList source = do
  cacheTime <- getCacheTime (map A.cacheTime artifactTimeList) source
  llvmTime <- getLLVMTime (map A.llvmTime artifactTimeList) source
  asmTime <- getAsmTime (map A.asmTime artifactTimeList) source
  objectTime <- getObjectTime (map A.objectTime artifactTimeList) source
  let artifactTime =
        A.ArtifactTime
          { cacheTime = cacheTime,
            llvmTime = llvmTime,
            asmTime = asmTime,
            objectTime = objectTime
          }
  Env.insertToArtifactMap (Source.sourceFilePath source) artifactTime
  return artifactTime

getCacheTime :: [CacheTime] -> Source.Source -> App CacheTime
getCacheTime = do
  getItemTime getFreshCacheTime

getLLVMTime :: [LLVMTime] -> Source.Source -> App LLVMTime
getLLVMTime = do
  getItemTime getFreshLLVMTime

getAsmTime :: [AsmTime] -> Source.Source -> App AsmTime
getAsmTime = do
  getItemTime getFreshAsmTime

getObjectTime :: [ObjectTime] -> Source.Source -> App ObjectTime
getObjectTime = do
  getItemTime getFreshObjectTime

getItemTime ::
  (Source.Source -> App (Maybe UTCTime)) ->
  [Maybe UTCTime] ->
  Source.Source ->
  App (Maybe UTCTime)
getItemTime relatedTimeGetter mTimeList source = do
  mTime <- relatedTimeGetter source
  case (mTime, distributeMaybe mTimeList) of
    (Nothing, _) ->
      return Nothing
    (_, Nothing) ->
      return Nothing
    (Just time, Just childTimeList) -> do
      if all (time >) childTimeList
        then return $ Just time
        else return Nothing

distributeMaybe :: [Maybe a] -> Maybe [a]
distributeMaybe xs =
  case xs of
    [] ->
      return []
    my : rest -> do
      y <- my
      rest' <- distributeMaybe rest
      return $ y : rest'

getFreshCacheTime :: Source.Source -> App CacheTime
getFreshCacheTime source = do
  cachePath <- Path.getSourceCachePath source
  getFreshTime source cachePath

getFreshLLVMTime :: Source.Source -> App LLVMTime
getFreshLLVMTime source = do
  llvmPath <- Path.sourceToOutputPath OK.LLVM source
  getFreshTime source llvmPath

getFreshAsmTime :: Source.Source -> App ObjectTime
getFreshAsmTime source = do
  objectPath <- Path.sourceToOutputPath OK.Asm source
  getFreshTime source objectPath

getFreshObjectTime :: Source.Source -> App ObjectTime
getFreshObjectTime source = do
  objectPath <- Path.sourceToOutputPath OK.Object source
  getFreshTime source objectPath

getFreshTime :: Source.Source -> Path Abs File -> App (Maybe UTCTime)
getFreshTime source itemPath = do
  existsItem <- Path.doesFileExist itemPath
  if not existsItem
    then return Nothing
    else do
      srcModTime <- Path.getModificationTime $ Source.sourceFilePath source
      itemModTime <- Path.getModificationTime itemPath
      if itemModTime > srcModTime
        then return $ Just itemModTime
        else return Nothing

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
      sourceList <- parseSourceHeader currentSource
      Unravel.insertToSourceChildrenMap currentSourceFilePath sourceList
      return sourceList

parseSourceHeader :: Source.Source -> App [Source.Source]
parseSourceHeader currentSource = do
  Locator.initialize
  Parse.ensureExistence currentSource
  let path = Source.sourceFilePath currentSource
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
