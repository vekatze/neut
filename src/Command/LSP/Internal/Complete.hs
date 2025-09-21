module Command.LSP.Internal.Complete
  ( Handle,
    new,
    complete,
  )
where

import App.App (App)
import App.Run (forP, liftMaybe, runApp)
import CodeParser.Parser (nonSymbolCharSet)
import Command.LSP.Internal.GetAllCachesInModule qualified as GAC
import Command.LSP.Internal.Source.Reflect qualified as SourceReflect
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.Trans
import Data.Bifunctor (second)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List (sort, (!?))
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.LocalVarTree qualified as LVT
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.Module
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.RawImportSummary
import Kernel.Common.Source
import Kernel.Common.Target
import Kernel.Common.TopCandidate
import Kernel.Unravel.Unravel qualified as Unravel
import Language.Common.BaseName qualified as BN
import Language.Common.Const (nsSep)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Language.LSP.VFS (VirtualFile (..), virtualFileText)
import Logger.Hint

data Handle = Handle
  { unravelHandle :: Unravel.Handle,
    pathHandle :: Path.Handle,
    antecedentHandle :: Antecedent.Handle,
    getModuleHandle :: GetModule.Handle,
    sourceReflectHandle :: SourceReflect.Handle,
    envHandle :: Env.Handle,
    gacHandle :: GAC.Handle
  }

new :: Global.Handle -> IO Handle
new globalHandle@(Global.Handle {..}) = do
  unravelHandle <- liftIO $ Unravel.new globalHandle
  let getModuleHandle = GetModule.new globalHandle
  let sourceReflectHandle = SourceReflect.new globalHandle
  let gacHandle = GAC.new globalHandle
  return $ Handle {..}

complete :: Handle -> Uri -> Position -> Maybe VirtualFile -> App [CompletionItem]
complete h uri pos fileOrNone = do
  Unravel.registerShiftMap (unravelHandle h)
  pathString <- liftMaybe $ uriToFilePath uri
  currentSource <- SourceReflect.reflect (sourceReflectHandle h) pathString >>= liftMaybe
  let loc = positionToLoc pos
  allItems <- lift (runApp $ collectCompletionItems h currentSource loc) >>= liftEither
  return $ filterCompletionItems fileOrNone pos allItems

collectCompletionItems :: Handle -> Source -> Loc -> App [CompletionItem]
collectCompletionItems h currentSource loc = do
  fmap concat $ forP (itemGetterList h) $ \itemGetter -> itemGetter currentSource loc

itemGetterList :: Handle -> [Source -> Loc -> App [CompletionItem]]
itemGetterList h =
  [getLocalCompletionItems h, getGlobalCompletionItems h]

getLocalCompletionItems :: Handle -> Source -> Loc -> App [CompletionItem]
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

getGlobalCompletionItems :: Handle -> Source -> Loc -> App [CompletionItem]
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
  App [CompletionItem]
adjustTopCandidate h summaryOrNone currentSource loc prefixSummary candInfo = do
  fmap concat $ forM candInfo $ \(candSource, candList) -> do
    revMap <- liftIO $ Antecedent.getReverseMap (antecedentHandle h)
    locatorOrNone <- getHumanReadableLocator revMap (sourceModule currentSource) candSource
    case locatorOrNone of
      Nothing ->
        return []
      Just locator -> do
        let candIsInCurrentSource = sourceFilePath currentSource == sourceFilePath candSource
        return $ concatMap (topCandidateToCompletionItem summaryOrNone candIsInCurrentSource prefixSummary locator loc) candList

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
  TopCandidate ->
  [CompletionItem]
topCandidateToCompletionItem importSummaryOrNone candIsInCurrentSource presetSummary locator cursorLoc topCandidate = do
  if candIsInCurrentSource && cursorLoc < loc topCandidate
    then []
    else do
      let baseDD = dd topCandidate
      let ll = DD.localLocator baseDD
      let fullyQualified = FullyQualified locator ll
      let bare = Bare locator ll
      let cands = filter (isProperCand importSummaryOrNone) [fullyQualified, bare]
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
      Just $ interpretCand' cand
    Just (summary, _) -> do
      if isAlreadyImported summary cand
        then Nothing
        else Just $ interpretCand' cand

interpretCand' :: Cand -> [TextEdit]
interpretCand' cand = do
  let edit = inImportBlock $ constructEditText cand <> "\n"
  let pos = Position {_line = 0, _character = 0}
  [TextEdit {_range = Range {_start = pos, _end = pos}, _newText = edit}]

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

getAllTopCandidate :: Handle -> Module -> App ([(Source, [TopCandidate])], FastPresetSummary)
getAllTopCandidate h baseModule = do
  let mainModule = Env.getMainModule (envHandle h)
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

getAllTopCandidate' :: Handle -> (MA.ModuleAlias, Module) -> App ([(Source, [TopCandidate])], (T.Text, Module))
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

getHumanReadableLocator :: Antecedent.RevMap -> Module -> Source -> App (Maybe T.Text)
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

filterCompletionItems :: Maybe VirtualFile -> Position -> [CompletionItem] -> [CompletionItem]
filterCompletionItems fileOrNone pos items =
  case fileOrNone of
    Nothing ->
      items
    Just vfile -> do
      case extractSymbolAtPosition vfile pos of
        Nothing ->
          items
        Just prefix ->
          filter (\item -> prefix `T.isPrefixOf` (item ^. J.label)) items

extractSymbolAtPosition :: VirtualFile -> Position -> Maybe T.Text
extractSymbolAtPosition vfile pos = do
  let content = virtualFileText vfile
  let lines' = T.lines content
  let lineNum = fromIntegral $ pos ^. J.line
  let charNum = fromIntegral $ pos ^. J.character
  line <- lines' !? lineNum
  Just $ extractIdentifierAt line charNum

extractIdentifierAt :: T.Text -> Int -> T.Text
extractIdentifierAt line pos = do
  let (before, after) = T.splitAt pos line
  let beforeIdent = T.reverse $ T.takeWhile (not . isBoundaryChar) $ T.reverse before
  let afterIdent = T.takeWhile (not . isBoundaryChar) after
  beforeIdent <> afterIdent

isBoundaryChar :: Char -> Bool
isBoundaryChar c =
  S.member c nonSymbolCharSet
