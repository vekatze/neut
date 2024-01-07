module Scene.LSP.Complete (complete) where

import Context.App
import Context.AppM
import Context.Cache qualified as Cache
import Context.Path qualified as Path
import Control.Monad.Trans
import Data.Bifunctor (second)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Cache qualified as Cache
import Entity.Const (nsSep)
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident.Reify qualified as Ident
import Entity.LocalVarTree qualified as LVT
import Entity.Module
import Entity.ModuleAlias qualified as MA
import Entity.Source
import Entity.SourceLocator qualified as SL
import Entity.TopCandidate
import Language.LSP.Protocol.Types
import Scene.LSP.GetAllCachesInModule (getAllCachesInModule)
import Scene.Module.Reflect (getAllDependencies)
import Scene.Source.Reflect qualified as Source
import UnliftIO.Async

complete :: Uri -> Position -> AppM [CompletionItem]
complete uri pos = do
  pathString <- liftMaybe $ uriToFilePath uri
  currentSource <- lift (Source.reflect pathString) >>= liftMaybe
  let loc = positionToLoc pos
  lift $ fmap concat $ forConcurrently itemGetterList $ \itemGetter -> itemGetter currentSource loc

adjustTopCandidate :: Source -> Loc -> Source -> [TopCandidate] -> App [CompletionItem]
adjustTopCandidate currentSource loc candSource candList = do
  locator <- NE.head <$> getHumanReadableLocator (sourceModule currentSource) candSource
  prefixList <- getPrefixList (sourceModule currentSource) candSource
  let candIsInCurrentSource = sourceFilePath currentSource == sourceFilePath candSource
  return $ concatMap (topCandidateToCompletionItem candIsInCurrentSource locator loc prefixList) candList

itemGetterList :: [Source -> Loc -> App [CompletionItem]]
itemGetterList =
  [getLocalCompletionItems, getGlobalCompletionItems]

getLocalCompletionItems :: Source -> Loc -> App [CompletionItem]
getLocalCompletionItems source loc = do
  cachePath <- Path.getSourceCachePath source
  cacheOrNone <- Cache.loadCacheOptimistically cachePath
  case cacheOrNone of
    Nothing ->
      return []
    Just cache -> do
      let localVarList = LVT.collect loc (Cache.localVarTree cache)
      let localVarList' = nubOrd $ sort $ map Ident.toText localVarList
      return $ map identToCompletionItem localVarList'

getGlobalCompletionItems :: Source -> Loc -> App [CompletionItem]
getGlobalCompletionItems currentSource loc = do
  let baseModule = sourceModule currentSource
  globalVarList <- getAllTopCandidate baseModule
  concat <$> mapM (uncurry (adjustTopCandidate currentSource loc)) globalVarList

identToCompletionItem :: T.Text -> CompletionItem
identToCompletionItem x = do
  let CompletionItem {..} = toCompletionItem x
  CompletionItem {_kind = Just CompletionItemKind_Variable, ..}

topCandidateToCompletionItem :: Bool -> T.Text -> Loc -> [T.Text] -> TopCandidate -> [CompletionItem]
topCandidateToCompletionItem candIsInCurrentSource locator cursorLoc prefixList topCandidate = do
  if candIsInCurrentSource && cursorLoc < loc topCandidate
    then []
    else do
      let dd' = DD.reify (dd topCandidate)
      let ll = DD.localLocator (dd topCandidate)
      let labels = dd' : ll : map (\prefix -> prefix <> nsSep <> ll) prefixList
      flip map labels $ \label -> do
        let CompletionItem {..} = toCompletionItem label
        let _kind = Just $ fromCandidateKind $ kind topCandidate
        let _detail = Just ("in " <> locator)
        CompletionItem {_kind, _detail, ..}

toCompletionItem :: T.Text -> CompletionItem
toCompletionItem x =
  CompletionItem
    { _label = x,
      _labelDetails = Nothing,
      _kind = Nothing,
      _tags = Nothing,
      _detail = Nothing,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Nothing,
      _textEditText = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _data_ = Nothing
    }

positionToLoc :: Position -> Loc
positionToLoc Position {_line, _character} =
  (fromIntegral $ _line + 1, fromIntegral $ _character + 1)

getAllTopCandidate :: Module -> App [(Source, [TopCandidate])]
getAllTopCandidate baseModule = do
  dependencies <- getAllDependencies baseModule
  concat <$> mapConcurrently getAllTopCandidate' (baseModule : dependencies)

getAllTopCandidate' :: Module -> App [(Source, [TopCandidate])]
getAllTopCandidate' baseModule = do
  cacheSeq <- getAllCachesInModule baseModule
  return $ map (second Cache.topCandidate) cacheSeq

fromCandidateKind :: CandidateKind -> CompletionItemKind
fromCandidateKind candidateKind =
  case candidateKind of
    Constant ->
      CompletionItemKind_Constant
    Constructor ->
      CompletionItemKind_Constructor
    Function ->
      CompletionItemKind_Function

getPrefixList :: Module -> Source -> App [T.Text]
getPrefixList baseModule source = do
  if moduleID baseModule /= moduleID (sourceModule source)
    then return []
    else do
      locatorList <- NE.toList <$> getHumanReadableLocator baseModule source
      let prefixInfo = Map.toList $ modulePrefixMap baseModule
      return $ map (BN.reify . fst) $ filter (\(_, cod) -> uncurry reifyPrefixCod cod `elem` locatorList) prefixInfo

reifyPrefixCod :: MA.ModuleAlias -> SL.SourceLocator -> T.Text
reifyPrefixCod alias sl =
  MA.reify alias <> "." <> SL.toText sl
