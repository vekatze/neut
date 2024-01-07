module Scene.LSP.Complete (complete) where

import Context.App
import Context.AppM
import Context.Cache qualified as Cache
import Context.Path qualified as Path
import Control.Monad.Trans
import Data.Bifunctor (second)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Entity.Cache qualified as Cache
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident.Reify qualified as Ident
import Entity.LocalVarTree qualified as LVT
import Entity.Module
import Entity.Source
import Entity.TopCandidate
import Language.LSP.Protocol.Types
import Scene.LSP.GetAllCachesInModule (getAllCachesInModule)
import Scene.Module.Reflect (getAllDependencies)
import Scene.Source.Reflect qualified as Source
import UnliftIO.Async

complete :: Uri -> Position -> AppM [CompletionItem]
complete uri pos = do
  pathString <- liftMaybe $ uriToFilePath uri
  src <- lift (Source.reflect pathString) >>= liftMaybe
  let loc = positionToLoc pos
  localVarList <- getLocalCompletionItems src loc
  let baseModule = sourceModule src
  globalVarList <- lift $ getAllTopCandidate baseModule
  globalVarList' <- lift $ concat <$> mapM (uncurry (adjustTopCandidate baseModule loc)) globalVarList
  return $ localVarList ++ globalVarList'

adjustTopCandidate :: Module -> Loc -> Source -> [TopCandidate] -> App [CompletionItem]
adjustTopCandidate baseModule loc src candList = do
  locator <- NE.head <$> getHumanReadableLocator baseModule src
  let candIsInMainModule = moduleID baseModule == moduleID (sourceModule src)
  return $ concatMap (topCandidateToCompletionItem candIsInMainModule locator loc) candList

getLocalCompletionItems :: Source -> Loc -> AppM [CompletionItem]
getLocalCompletionItems source loc = do
  cachePath <- lift $ Path.getSourceCachePath source
  cache <- lift (Cache.loadCacheOptimistically cachePath) >>= liftMaybe
  let localVarList = LVT.collect loc (Cache.localVarTree cache)
  let localVarList' = nubOrd $ sort $ map Ident.toText localVarList
  return $ map identToCompletionItem localVarList'

identToCompletionItem :: T.Text -> CompletionItem
identToCompletionItem x = do
  let CompletionItem {..} = toCompletionItem x
  CompletionItem {_kind = Just CompletionItemKind_Variable, ..}

topCandidateToCompletionItem :: Bool -> T.Text -> Loc -> TopCandidate -> [CompletionItem]
topCandidateToCompletionItem candIsInMainModule locator cursorLoc topCandidate = do
  if candIsInMainModule && cursorLoc < loc topCandidate
    then []
    else do
      let labels = [DD.reify (dd topCandidate), DD.localLocator (dd topCandidate)]
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
