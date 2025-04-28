module Move.Scene.LSP.Complete
  ( Handle,
    new,
    complete,
  )
where

import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.Trans
import Data.Bifunctor (second)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (maybeToList)
import Data.Set qualified as S
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Cache qualified as Cache
import Move.Context.Clang qualified as Clang
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO, forP, liftMaybe, runEIO)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.LSP.GetAllCachesInModule qualified as GAC
import Move.Scene.Module.GetModule qualified as GetModule
import Move.Scene.Source.Reflect qualified as SourceReflect
import Move.Scene.Unravel qualified as Unravel
import Rule.BaseName qualified as BN
import Rule.Cache qualified as Cache
import Rule.Const (nsSep)
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident.Reify qualified as Ident
import Rule.LocalVarTree qualified as LVT
import Rule.Module
import Rule.ModuleAlias qualified as MA
import Rule.ModuleID qualified as MID
import Rule.RawImportSummary
import Rule.Source
import Rule.SourceLocator qualified as SL
import Rule.Target
import Rule.TopCandidate

data Handle
  = Handle
  { unravelHandle :: Unravel.Handle,
    clangHandle :: Clang.Handle,
    pathHandle :: Path.Handle,
    antecedentHandle :: Antecedent.Handle,
    getModuleHandle :: GetModule.Handle,
    sourceReflectHandle :: SourceReflect.Handle,
    envHandle :: Env.Handle,
    gacHandle :: GAC.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Debug.Handle -> Antecedent.Handle -> Unravel.Handle -> App Handle
new envHandle gensymHandle debugHandle antecedentHandle unravelHandle = do
  clangHandle <- Clang.new debugHandle
  pathHandle <- Path.new envHandle debugHandle
  getModuleHandle <- GetModule.new gensymHandle
  sourceReflectHandle <- SourceReflect.new envHandle gensymHandle
  gacHandle <- GAC.new envHandle debugHandle antecedentHandle
  return $ Handle {..}

complete :: Handle -> Uri -> Position -> EIO [CompletionItem]
complete h uri pos = do
  Unravel.registerShiftMap (unravelHandle h)
  pathString <- liftMaybe $ uriToFilePath uri
  currentSource <- SourceReflect.reflect (sourceReflectHandle h) pathString >>= liftMaybe
  _ <- Clang.getClangDigest (clangHandle h) -- cache
  let loc = positionToLoc pos
  lift (runEIO $ collectCompletionItems h currentSource loc) >>= liftEither

collectCompletionItems :: Handle -> Source -> Loc -> EIO [CompletionItem]
collectCompletionItems h currentSource loc = do
  fmap concat $ forP (itemGetterList h) $ \itemGetter -> itemGetter currentSource loc

itemGetterList :: Handle -> [Source -> Loc -> EIO [CompletionItem]]
itemGetterList h =
  [getLocalCompletionItems h, getGlobalCompletionItems h]

getLocalCompletionItems :: Handle -> Source -> Loc -> EIO [CompletionItem]
getLocalCompletionItems h source loc = do
  cachePath <- Path.getSourceCompletionCachePath (pathHandle h) Peripheral source
  cacheOrNone <- Cache.loadCompletionCacheOptimistically cachePath
  case cacheOrNone of
    Nothing ->
      return []
    Just cache -> do
      let localVarList = LVT.collect loc (Cache.localVarTree cache)
      let localVarList' = nubOrd $ sort $ map Ident.toText localVarList
      return $ map identToCompletionItem localVarList'

getGlobalCompletionItems :: Handle -> Source -> Loc -> EIO [CompletionItem]
getGlobalCompletionItems h currentSource loc = do
  let baseModule = sourceModule currentSource
  (globalVarList, aliasPresetMap) <- getAllTopCandidate h baseModule
  baseCacheOrNone <-
    Path.getSourceCompletionCachePath (pathHandle h) Peripheral currentSource
      >>= Cache.loadCompletionCacheOptimistically
  let importSummaryOrNone = baseCacheOrNone >>= Cache.rawImportSummary
  let impLoc = getImportLoc importSummaryOrNone
  if loc < impLoc
    then return []
    else adjustTopCandidate h importSummaryOrNone currentSource loc aliasPresetMap globalVarList

getImportLoc :: Maybe RawImportSummary -> Loc
getImportLoc importOrNone =
  case importOrNone of
    Nothing ->
      (1, 1)
    Just (_, loc) ->
      loc

adjustTopCandidate ::
  Handle ->
  Maybe RawImportSummary ->
  Source ->
  Loc ->
  FastPresetSummary ->
  [(Source, [TopCandidate])] ->
  EIO [CompletionItem]
adjustTopCandidate h summaryOrNone currentSource loc prefixSummary candInfo = do
  fmap concat $ forM candInfo $ \(candSource, candList) -> do
    revMap <- liftIO $ Antecedent.getReverseMap (antecedentHandle h)
    locatorOrNone <- getHumanReadableLocator revMap (sourceModule currentSource) candSource
    case locatorOrNone of
      Nothing ->
        return []
      Just locator -> do
        prefixList <- getPrefixList h (sourceModule currentSource) candSource
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

getAllTopCandidate :: Handle -> Module -> EIO ([(Source, [TopCandidate])], FastPresetSummary)
getAllTopCandidate h baseModule = do
  mainModule <- Env.getMainModule (envHandle h)
  dependencies <- GetModule.getAllDependencies (getModuleHandle h) mainModule baseModule
  let visibleModuleList = (MA.defaultModuleAlias, baseModule) : dependencies
  (candListList, aliasPresetInfo) <- unzip <$> forP visibleModuleList (getAllTopCandidate' h)
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

getAllTopCandidate' :: Handle -> (MA.ModuleAlias, Module) -> EIO ([(Source, [TopCandidate])], (T.Text, Module))
getAllTopCandidate' h (alias, candModule) = do
  cacheSeq <- GAC.getAllCompletionCachesInModule (gacHandle h) candModule
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

getPrefixList :: Handle -> Module -> Source -> EIO [T.Text]
getPrefixList h baseModule source = do
  if moduleID baseModule /= moduleID (sourceModule source)
    then return []
    else do
      revMap <- liftIO $ Antecedent.getReverseMap (antecedentHandle h)
      locatorList <- maybeToList <$> getHumanReadableLocator revMap baseModule source
      let prefixInfo = Map.toList $ modulePrefixMap baseModule
      return $ map (BN.reify . fst) $ filter (\(_, cod) -> uncurry reifyPrefixCod cod `elem` locatorList) prefixInfo

reifyPrefixCod :: MA.ModuleAlias -> SL.SourceLocator -> T.Text
reifyPrefixCod alias sl =
  MA.reify alias <> "." <> SL.toText sl

getHumanReadableLocator :: Antecedent.RevMap -> Module -> Source -> EIO (Maybe T.Text)
getHumanReadableLocator revMap baseModule source = do
  let sourceModuleID = moduleID $ sourceModule source
  baseReadableLocator <- getBaseReadableLocator source
  return $ _getHumanReadableLocator revMap baseModule sourceModuleID baseReadableLocator

_getHumanReadableLocator :: Antecedent.RevMap -> Module -> MID.ModuleID -> T.Text -> Maybe T.Text
_getHumanReadableLocator revMap baseModule sourceModuleID baseReadableLocator = do
  case sourceModuleID of
    MID.Main -> do
      Just $ "this" <> nsSep <> baseReadableLocator
    MID.Base -> do
      Just $ "base" <> nsSep <> baseReadableLocator
    MID.Library digest -> do
      let digestMap = getDigestMap baseModule
      case Map.lookup digest digestMap of
        Nothing -> do
          case Map.lookup sourceModuleID revMap of
            Just midSet ->
              _getHumanReadableLocator' revMap baseModule (S.toList midSet) baseReadableLocator
            Nothing ->
              Nothing
        Just aliasList -> do
          let alias = BN.reify $ MA.extract $ NE.head aliasList
          Just $ alias <> nsSep <> baseReadableLocator

_getHumanReadableLocator' :: Antecedent.RevMap -> Module -> [MID.ModuleID] -> T.Text -> Maybe T.Text
_getHumanReadableLocator' revMap baseModule midList baseReadableLocator = do
  case midList of
    [] ->
      Nothing
    mid : rest -> do
      case _getHumanReadableLocator revMap baseModule mid baseReadableLocator of
        Just locator ->
          Just locator
        Nothing ->
          _getHumanReadableLocator' revMap baseModule rest baseReadableLocator
