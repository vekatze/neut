module Scene.LSP.Complete (complete) where

import Context.App
import Context.AppM
import Context.Cache qualified as Cache
import Context.Global qualified as Global
import Context.KeyArg qualified as KeyArg
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Context.Unravel qualified as Unravel
import Control.Monad
import Control.Monad.Trans
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text qualified as T
import Entity.AliasInfo
import Entity.Cache qualified as Cache
import Entity.Const
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident.Reify qualified as Ident
import Entity.IsConstLike (IsConstLike)
import Entity.Key
import Entity.LocalVarTree qualified as LVT
import Entity.Module
import Entity.Source
import Entity.TopNameMap
import Language.LSP.Protocol.Types
import Path
import Scene.Collect qualified as Collect
import Scene.Initialize qualified as Initialize
import Scene.Load qualified as Load
import Scene.Parse qualified as Parse
import Scene.Source.Reflect qualified as Source
import Scene.Unravel qualified as Unravel
import UnliftIO.Async

complete :: Uri -> Position -> AppM [CompletionItem]
complete uri pos = do
  pathString <- liftMaybe $ uriToFilePath uri
  src <- lift (Source.reflect pathString) >>= liftMaybe
  lift (collectNames pathString) >>= liftMaybe
  localVarList <- getLocalCompletionItems src pos
  childrenMap <- lift Unravel.getSourceChildrenMap
  let children = concat $ maybeToList $ Map.lookup (sourceFilePath src) childrenMap
  sourceNameMap <- lift Global.getSourceNameMap
  childCompItemList <- fmap concat $ forM children $ \(child, aliasInfoList) -> do
    getChildCompItemList sourceNameMap (sourceModule src) child aliasInfoList
  case Map.lookup (sourceFilePath src) sourceNameMap of
    Nothing -> do
      return $ localVarList ++ childCompItemList
    Just nameInfo -> do
      nameList <- getLocalNameList nameInfo
      return $ localVarList ++ map (newCompletionItem Nothing) nameList ++ childCompItemList

getLocalCompletionItems :: Source -> Position -> AppM [CompletionItem]
getLocalCompletionItems source pos = do
  cachePath <- lift $ Path.getSourceCachePath source
  cache <- lift (Cache.loadCacheOptimistically cachePath) >>= liftMaybe
  let localVarList = LVT.collect (positionToLoc pos) (Cache.localVarTree cache)
  let localVarList' = nubOrd $ sort $ map Ident.toText localVarList
  return $ map identToCompletionItem localVarList'

collectNames :: FilePath -> App (Maybe ())
collectNames filePath = do
  Throw.runMaybe $ do
    Initialize.initializeForTarget
    paths <- Collect.collectSourceList (Just filePath)
    forM_ paths $ \path -> do
      (_, dependenceSeq) <- Unravel.unravelFromFile path
      contentSeq <- forConcurrently dependenceSeq $ \source -> do
        cacheOrContent <- Load.load source
        return (source, cacheOrContent)
      forM_ contentSeq $ \(source, cacheOrContent) -> do
        Initialize.initializeForSource source
        void $ Parse.parse source cacheOrContent

getChildCompItemList ::
  Map.HashMap (Path Abs File) TopNameMap ->
  Module ->
  Source ->
  [AliasInfo] ->
  AppM [CompletionItem]
getChildCompItemList sourceNameMap sourceModule child aliasInfoList = do
  case Map.lookup (sourceFilePath child) sourceNameMap of
    Nothing -> do
      return []
    Just topNameMap -> do
      nameList <- getLocalNameList topNameMap
      locator <- NE.head <$> getHumanReadableLocator sourceModule child
      let fullyQualifiedNameList = map (\(mk, x) -> (mk, locator <> nsSep <> x)) nameList
      let newCompletionItem' = newCompletionItem (Just locator)
      let rawAliasList = mapMaybe getRawAlias aliasInfoList
      if null rawAliasList
        then do
          let ns1 = map newCompletionItem' nameList
          let ns2 = map newCompletionItem' fullyQualifiedNameList
          return $ ns1 ++ ns2
        else do
          let aliasPrefixedNameList = concatMap (\rawAlias -> map (attachPrefix rawAlias) nameList) rawAliasList
          return $ map newCompletionItem' $ aliasPrefixedNameList ++ fullyQualifiedNameList

attachPrefix :: T.Text -> (a, T.Text) -> (a, T.Text)
attachPrefix rawAlias (mk, x) =
  (mk, rawAlias <> nsSep <> x)

getLocalNameList :: TopNameMap -> AppM [(Maybe (IsConstLike, [Key]), T.Text)]
getLocalNameList nameInfo = do
  let ddList = map fst $ Map.toList nameInfo
  mKeyArgList <- lift $ mapM KeyArg.lookupMaybe ddList
  return $ zip mKeyArgList $ map DD.localLocator ddList

newCompletionItem :: Maybe T.Text -> (Maybe (IsConstLike, [Key]), T.Text) -> CompletionItem
newCompletionItem mLocator (_, t) =
  CompletionItem
    { _label = t,
      _labelDetails = Nothing,
      _kind = Just CompletionItemKind_Function,
      _tags = Nothing,
      _detail = mLocator >>= \locator -> Just ("in " <> locator),
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

identToCompletionItem :: T.Text -> CompletionItem
identToCompletionItem x =
  CompletionItem
    { _label = x,
      _labelDetails = Nothing,
      _kind = Just CompletionItemKind_Variable,
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
