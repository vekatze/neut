module Scene.LSP.Complete (complete) where

import Context.App
import Context.AppM
import Context.Cache qualified as Cache
import Context.Path qualified as Path
import Control.Monad
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
import Entity.RawImportSummary
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
  (globalVarList, aliasPresetMap) <- getAllTopCandidate baseModule
  baseCacheOrNone <- Path.getSourceCachePath currentSource >>= Cache.loadCacheOptimistically
  let importSummaryOrNone = baseCacheOrNone >>= Cache.rawImportSummary
  let impLoc = getImportLoc importSummaryOrNone
  if loc < impLoc
    then return []
    else do
      adjustTopCandidate importSummaryOrNone currentSource loc aliasPresetMap globalVarList

getImportLoc :: Maybe RawImportSummary -> Loc
getImportLoc importOrNone =
  case importOrNone of
    Nothing ->
      (1, 1)
    Just (_, loc) ->
      loc

adjustTopCandidate ::
  Maybe RawImportSummary ->
  Source ->
  Loc ->
  AliasPresetMap ->
  [(Source, [TopCandidate])] ->
  App [CompletionItem]
adjustTopCandidate summaryOrNone currentSource loc aliasPresetMap candInfo = do
  let importedNameList = getImportedNameList summaryOrNone aliasPresetMap
  fmap concat $ forM candInfo $ \(candSource, candList) -> do
    locator <- NE.head <$> getHumanReadableLocator (sourceModule currentSource) candSource
    prefixList <- getPrefixList (sourceModule currentSource) candSource
    let candIsInCurrentSource = sourceFilePath currentSource == sourceFilePath candSource
    return $ concatMap (topCandidateToCompletionItem summaryOrNone candIsInCurrentSource importedNameList locator loc prefixList) candList

identToCompletionItem :: T.Text -> CompletionItem
identToCompletionItem x = do
  let CompletionItem {..} = toCompletionItem x
  CompletionItem {_kind = Just CompletionItemKind_Variable, ..}

-- fixme: handle automatic preset imports
getImportedNameList :: Maybe RawImportSummary -> AliasPresetMap -> [T.Text]
getImportedNameList summaryOrNone aliasPresetMap =
  case summaryOrNone of
    Nothing ->
      []
    Just (summary, _) -> do
      concat $ flip map summary $ \(prefixOrGlobalLocator, specifiedLLs) -> do
        case Map.lookup prefixOrGlobalLocator aliasPresetMap of
          Nothing ->
            map (\ll -> prefixOrGlobalLocator <> nsSep <> ll) specifiedLLs
          Just defaultNameList -> do
            concat $ flip map (Map.toList defaultNameList) $ \(locator, lls) -> do
              map (\ll -> prefixOrGlobalLocator <> nsSep <> locator <> nsSep <> BN.reify ll) lls

topCandidateToCompletionItem ::
  Maybe RawImportSummary ->
  Bool ->
  [T.Text] ->
  T.Text ->
  Loc ->
  [T.Text] ->
  TopCandidate ->
  [CompletionItem]
topCandidateToCompletionItem importSummaryOrNone candIsInCurrentSource importedNameList locator cursorLoc prefixList topCandidate = do
  if candIsInCurrentSource && cursorLoc < loc topCandidate
    then []
    else do
      let baseDD = dd topCandidate
      let ll = DD.localLocator baseDD
      let fullyQualified = FullyQualified locator ll
      let isInPresetImport = reifyCand fullyQualified `elem` importedNameList
      let needsTextEdit = not candIsInCurrentSource && not isInPresetImport
      let bare = Bare locator ll
      let prefixed = map (`Prefixed` ll) prefixList
      let cands = fullyQualified : bare : prefixed
      flip map cands $ \cand -> do
        let textEditOrNone = if needsTextEdit then interpretCand importSummaryOrNone cand else Nothing
        let CompletionItem {..} = toCompletionItem $ reifyCand cand
        let _kind = Just $ fromCandidateKind $ kind topCandidate
        let _detail = Just ("in " <> locator)
        CompletionItem {_kind, _detail, _additionalTextEdits = textEditOrNone, ..}

data Cand
  = FullyQualified T.Text T.Text
  | Prefixed T.Text T.Text
  | Bare T.Text T.Text

interpretCand :: Maybe RawImportSummary -> Cand -> Maybe [TextEdit]
interpretCand importSummaryOrNone cand =
  case importSummaryOrNone of
    Nothing -> do
      let edit = inImportBlock $ constructEditText cand <> "\n"
      let pos = Position {_line = 0, _character = 0}
      Just [TextEdit {_range = Range {_start = pos, _end = pos}, _newText = edit}]
    Just (summary, loc) -> do
      if isAlreadyImported summary cand
        then Nothing
        else do
          let edit = constructEditText cand <> "\n"
          let pos = locToPosition loc
          Just [TextEdit {_range = Range {_start = pos, _end = pos}, _newText = edit}]

reifyCand :: Cand -> T.Text
reifyCand cand =
  case cand of
    FullyQualified gl ll ->
      gl <> nsSep <> ll
    Prefixed prefix ll ->
      prefix <> nsSep <> ll
    Bare _ ll ->
      ll

isAlreadyImported :: [(T.Text, [T.Text])] -> Cand -> Bool
isAlreadyImported summary cand =
  case cand of
    FullyQualified gl _ ->
      gl `elem` map fst summary
    Prefixed prefix _ ->
      prefix `elem` map fst summary
    Bare _ ll ->
      ll `elem` concatMap snd summary

constructEditText :: Cand -> T.Text
constructEditText cand =
  case cand of
    FullyQualified gl _ ->
      "- " <> gl
    Prefixed prefix _ ->
      "- " <> prefix
    Bare gl ll -> do
      "- " <> gl <> " {" <> ll <> "}"

inImportBlock :: T.Text -> T.Text
inImportBlock text =
  "import {\n" <> text <> "\n}"

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

locToPosition :: Loc -> Position
locToPosition (line, character) =
  Position {_line = fromIntegral $ line - 1, _character = fromIntegral $ character - 1}

getAllTopCandidate :: Module -> App ([(Source, [TopCandidate])], AliasPresetMap)
getAllTopCandidate baseModule = do
  dependencies <- getAllDependencies baseModule
  let visibleModuleList = (MA.defaultModuleAlias, baseModule) : dependencies
  (candListList, aliasPresetInfo) <- unzip <$> mapConcurrently getAllTopCandidate' visibleModuleList
  return (concat candListList, constructAliasPresetMap aliasPresetInfo)

constructAliasPresetMap :: [(T.Text, Module)] -> AliasPresetMap
constructAliasPresetMap =
  Map.fromList . map (second modulePresetMap)

getAllTopCandidate' :: (MA.ModuleAlias, Module) -> App ([(Source, [TopCandidate])], (T.Text, Module))
getAllTopCandidate' (alias, candModule) = do
  cacheSeq <- getAllCachesInModule candModule
  return (map (second Cache.topCandidate) cacheSeq, (MA.reify alias, candModule))

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
