module Scene.LSP.Complete (complete) where

import Context.Antecedent qualified as Antecedent
import Context.App
import Context.AppM
import Context.Cache qualified as Cache
import Context.External (getClangDigest)
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor (second)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (maybeToList)
import Data.Set qualified as S
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
import Entity.ModuleID qualified as MID
import Entity.RawImportSummary
import Entity.Source
import Entity.SourceLocator qualified as SL
import Entity.Target
import Entity.TopCandidate
import Language.LSP.Protocol.Types
import Scene.LSP.GetAllCachesInModule (getAllCompletionCachesInModule)
import Scene.Module.Reflect (getAllDependencies)
import Scene.Source.Reflect qualified as Source
import Scene.Unravel (registerShiftMap)
import UnliftIO.Async

complete :: Uri -> Position -> AppM [CompletionItem]
complete uri pos = do
  lift registerShiftMap
  pathString <- liftMaybe $ uriToFilePath uri
  currentSource <- lift (Source.reflect pathString) >>= liftMaybe
  _ <- lift getClangDigest -- cache
  let loc = positionToLoc pos
  lift (Throw.runMaybe $ collectCompletionItems currentSource loc) >>= liftMaybe

collectCompletionItems :: Source -> Loc -> App [CompletionItem]
collectCompletionItems currentSource loc = do
  fmap concat $ forConcurrently itemGetterList $ \itemGetter -> itemGetter currentSource loc

itemGetterList :: [Source -> Loc -> App [CompletionItem]]
itemGetterList =
  [getLocalCompletionItems, getGlobalCompletionItems]

getLocalCompletionItems :: Source -> Loc -> App [CompletionItem]
getLocalCompletionItems source loc = do
  cachePath <- Path.getSourceCompletionCachePath Peripheral source
  cacheOrNone <- Cache.loadCompletionCacheOptimistically cachePath
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
  baseCacheOrNone <-
    Path.getSourceCompletionCachePath Peripheral currentSource
      >>= Cache.loadCompletionCacheOptimistically
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
  FastPresetSummary ->
  [(Source, [TopCandidate])] ->
  App [CompletionItem]
adjustTopCandidate summaryOrNone currentSource loc prefixSummary candInfo = do
  fmap concat $ forM candInfo $ \(candSource, candList) -> do
    revMap <- Antecedent.getReverseMap
    locatorOrNone <- getHumanReadableLocator revMap (sourceModule currentSource) candSource
    case locatorOrNone of
      Nothing ->
        return []
      Just locator -> do
        prefixList <- getPrefixList (sourceModule currentSource) candSource
        let candIsInCurrentSource = sourceFilePath currentSource == sourceFilePath candSource
        return $ concatMap (topCandidateToCompletionItem summaryOrNone candIsInCurrentSource prefixSummary locator loc prefixList) candList

identToCompletionItem :: T.Text -> CompletionItem
identToCompletionItem x = do
  let CompletionItem {..} = toCompletionItem x
  CompletionItem {_kind = Just CompletionItemKind_Variable, ..}

topCandidateToCompletionItem ::
  Maybe RawImportSummary ->
  Bool ->
  FastPresetSummary ->
  T.Text ->
  Loc ->
  [T.Text] ->
  TopCandidate ->
  [CompletionItem]
topCandidateToCompletionItem importSummaryOrNone candIsInCurrentSource presetSummary locator cursorLoc prefixList topCandidate = do
  if candIsInCurrentSource && cursorLoc < loc topCandidate
    then []
    else do
      let baseDD = dd topCandidate
      let ll = DD.localLocator baseDD
      let fullyQualified = FullyQualified locator ll
      let bare = Bare locator ll
      let prefixed = map (`Prefixed` ll) prefixList
      let cands = filter (isProperCand importSummaryOrNone) (fullyQualified : bare : prefixed)
      flip map cands $ \cand -> do
        let inPresetImport = isInPresetImport presetSummary cand
        let needsTextEdit = not candIsInCurrentSource && not inPresetImport
        let textEditOrNone = if needsTextEdit then interpretCand importSummaryOrNone cand else Nothing
        let CompletionItem {..} = toCompletionItem $ reifyCand cand
        let _kind = Just $ fromCandidateKind $ kind topCandidate
        let _labelDetails = Just CompletionItemLabelDetails {_description = Just ("in " <> locator), _detail = Nothing}
        CompletionItem {_kind, _labelDetails, _additionalTextEdits = textEditOrNone, ..}

data Cand
  = FullyQualified T.Text T.Text
  | Prefixed T.Text T.Text
  | Bare T.Text T.Text

isProperCand :: Maybe RawImportSummary -> Cand -> Bool
isProperCand summaryOrNone cand =
  not (isAmbiguousCand summaryOrNone cand) && isVisibleCand cand

isAmbiguousCand :: Maybe RawImportSummary -> Cand -> Bool
isAmbiguousCand summaryOrNone cand = do
  case cand of
    Bare gl ll ->
      case summaryOrNone of
        Nothing ->
          False
        Just (summary, _) ->
          shouldRemoveLocatorPair (gl, ll) summary
    _ ->
      False

isVisibleCand :: Cand -> Bool
isVisibleCand cand =
  case cand of
    Bare gl ll -> do
      ("this." `T.isPrefixOf` gl) || (not ("._" `T.isInfixOf` gl) && not ("_" `T.isPrefixOf` ll))
    Prefixed _ ll ->
      not ("_" `T.isPrefixOf` ll)
    FullyQualified gl ll -> do
      ("this." `T.isPrefixOf` gl) || (not ("._" `T.isInfixOf` gl) && not ("_" `T.isPrefixOf` ll))

shouldRemoveLocatorPair :: (T.Text, T.Text) -> [(T.Text, [T.Text])] -> Bool
shouldRemoveLocatorPair (gl, ll) summary = do
  case summary of
    [] ->
      False
    (gl', lls) : rest -> do
      let b1 = gl /= gl' && ll `elem` lls
      let b2 = shouldRemoveLocatorPair (gl, ll) rest
      b1 || b2

isInPresetImport :: FastPresetSummary -> Cand -> Bool
isInPresetImport presetSummary cand =
  case cand of
    FullyQualified locator _ ->
      S.member locator (aliasSet presetSummary)
    Prefixed {} ->
      False
    Bare locator ll ->
      case Map.lookup locator (presetMap presetSummary) of
        Nothing ->
          False
        Just lls ->
          ll `elem` lls

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
          let prefix = if null summary then "\n" else ""
          let edit = prefix <> constructEditText cand <> "\n"
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
      "  " <> gl <> ","
    Prefixed prefix _ ->
      "  " <> prefix <> ","
    Bare gl ll -> do
      "  " <> gl <> " {" <> ll <> "}" <> ","

inImportBlock :: T.Text -> T.Text
inImportBlock text =
  "import {\n" <> text <> "}\n\n"

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

getAllTopCandidate :: Module -> App ([(Source, [TopCandidate])], FastPresetSummary)
getAllTopCandidate baseModule = do
  dependencies <- getAllDependencies baseModule
  let visibleModuleList = (MA.defaultModuleAlias, baseModule) : dependencies
  (candListList, aliasPresetInfo) <- unzip <$> mapConcurrently getAllTopCandidate' visibleModuleList
  let aliasList = getAliasListWithEnabledPresets baseModule
  let aliasPresetMap = constructAliasPresetMap aliasPresetInfo
  let presetSummary =
        concat $ flip map aliasList $ \alias -> do
          let alias' = MA.reify alias
          case Map.lookup alias' aliasPresetMap of
            Nothing ->
              []
            Just presetMap ->
              reifyPresetMap (MA.reify alias) presetMap
  return (concat candListList, fromPresetSummary presetSummary)

data FastPresetSummary = FastPresetSummary
  { presetMap :: Map.HashMap T.Text (S.Set T.Text),
    aliasSet :: S.Set T.Text
  }

fromPresetSummary :: PresetSummary -> FastPresetSummary
fromPresetSummary summary =
  FastPresetSummary
    { presetMap = Map.fromList $ map (second $ S.fromList . map BN.reify) summary,
      aliasSet = S.fromList $ map fst summary
    }

constructAliasPresetMap :: [(T.Text, Module)] -> AliasPresetMap
constructAliasPresetMap =
  Map.fromList . map (second modulePresetMap)

getAllTopCandidate' :: (MA.ModuleAlias, Module) -> App ([(Source, [TopCandidate])], (T.Text, Module))
getAllTopCandidate' (alias, candModule) = do
  cacheSeq <- getAllCompletionCachesInModule candModule
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
      revMap <- Antecedent.getReverseMap
      locatorList <- maybeToList <$> getHumanReadableLocator revMap baseModule source
      let prefixInfo = Map.toList $ modulePrefixMap baseModule
      return $ map (BN.reify . fst) $ filter (\(_, cod) -> uncurry reifyPrefixCod cod `elem` locatorList) prefixInfo

reifyPrefixCod :: MA.ModuleAlias -> SL.SourceLocator -> T.Text
reifyPrefixCod alias sl =
  MA.reify alias <> "." <> SL.toText sl

getHumanReadableLocator :: Antecedent.RevMap -> Module -> Source -> App (Maybe T.Text)
getHumanReadableLocator revMap baseModule source = do
  let sourceModuleID = moduleID $ sourceModule source
  baseReadableLocator <- getBaseReadableLocator source
  _getHumanReadableLocator revMap baseModule sourceModuleID baseReadableLocator

_getHumanReadableLocator :: Antecedent.RevMap -> Module -> MID.ModuleID -> T.Text -> App (Maybe T.Text)
_getHumanReadableLocator revMap baseModule sourceModuleID baseReadableLocator = do
  case sourceModuleID of
    MID.Main -> do
      return $ Just $ "this" <> nsSep <> baseReadableLocator
    MID.Base -> do
      return $ Just $ "base" <> nsSep <> baseReadableLocator
    MID.Library digest -> do
      let digestMap = getDigestMap baseModule
      case Map.lookup digest digestMap of
        Nothing -> do
          case Map.lookup sourceModuleID revMap of
            Just midSet ->
              _getHumanReadableLocator' revMap baseModule (S.toList midSet) baseReadableLocator
            Nothing ->
              return Nothing
        Just aliasList -> do
          let alias = BN.reify $ MA.extract $ NE.head aliasList
          return $ Just $ alias <> nsSep <> baseReadableLocator

_getHumanReadableLocator' :: Antecedent.RevMap -> Module -> [MID.ModuleID] -> T.Text -> App (Maybe T.Text)
_getHumanReadableLocator' revMap baseModule midList baseReadableLocator = do
  case midList of
    [] ->
      return Nothing
    mid : rest -> do
      locatorOrNone <- _getHumanReadableLocator revMap baseModule mid baseReadableLocator
      case locatorOrNone of
        Just locator ->
          return $ Just locator
        Nothing ->
          _getHumanReadableLocator' revMap baseModule rest baseReadableLocator
