module Scene.LSP.Complete (complete) where

import Context.App
import Context.Global qualified as Global
import Context.KeyArg qualified as KeyArg
import Context.Unravel qualified as Unravel
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text qualified as T
import Entity.AliasInfo
import Entity.Const
import Entity.DefiniteDescription qualified as DD
import Entity.IsConstLike (IsConstLike)
import Entity.Key
import Entity.LocalLocator qualified as LL
import Entity.Module
import Entity.Source
import Entity.TopNameMap
import Language.LSP.Protocol.Types
import Path
import Scene.Source.Reflect qualified as Source

complete :: Uri -> App [CompletionItem]
complete uri = do
  case uriToFilePath uri of
    Nothing ->
      return []
    Just pathString -> do
      mSrc <- Source.reflect pathString
      case mSrc of
        Nothing -> do
          return []
        Just src -> do
          childrenMap <- Unravel.getSourceChildrenMap
          let children = concat $ maybeToList $ Map.lookup (sourceFilePath src) childrenMap
          sourceNameMap <- Global.getSourceNameMap
          childCompItemList <- fmap concat $ forM children $ \(child, aliasInfoList) -> do
            getChildCompItemList sourceNameMap (sourceModule src) child aliasInfoList
          case Map.lookup (sourceFilePath src) sourceNameMap of
            Nothing -> do
              return childCompItemList
            Just nameInfo -> do
              nameList <- getLocalNameList nameInfo
              return $ map (newCompletionItem Nothing) nameList ++ childCompItemList

getChildCompItemList ::
  Map.HashMap (Path Abs File) TopNameMap ->
  Module ->
  Source ->
  [AliasInfo] ->
  App [CompletionItem]
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

getLocalNameList :: TopNameMap -> App [(Maybe (IsConstLike, [Key]), T.Text)]
getLocalNameList nameInfo = do
  let ddList = map fst $ Map.toList nameInfo
  mKeyArgList <- mapM KeyArg.lookupMaybe ddList
  return $ zip mKeyArgList $ map (LL.reify . DD.localLocator) ddList

newCompletionItem :: Maybe T.Text -> (Maybe (IsConstLike, [Key]), T.Text) -> CompletionItem
newCompletionItem mLocator (mKeyArg, t) =
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
      _insertText = stylizeKeyArgList t mKeyArg,
      _insertTextFormat = Just InsertTextFormat_Snippet,
      _insertTextMode = Nothing,
      _textEdit = Nothing,
      _textEditText = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _data_ = Nothing
    }

stylizeKeyArgList :: T.Text -> Maybe (IsConstLike, [Key]) -> Maybe T.Text
stylizeKeyArgList t mKeyArgList = do
  (isConstLike, keyArgList) <- mKeyArgList
  if isConstLike
    then Nothing
    else return $ t <> "(" <> stylizeKeyArgList' 1 keyArgList <> ")"

stylizeKeyArgList' :: Int -> [Key] -> T.Text
stylizeKeyArgList' count keyArgList = do
  case keyArgList of
    [] ->
      ""
    [keyArg] ->
      stylizeKeyArg count keyArg
    keyArg : rest ->
      stylizeKeyArg count keyArg <> ", " <> stylizeKeyArgList' (count + 1) rest

stylizeKeyArg :: Int -> Key -> T.Text
stylizeKeyArg count keyArg =
  "${" <> T.pack (show count) <> ":" <> stylizeHoleKey keyArg <> "}"

stylizeHoleKey :: Key -> Key
stylizeHoleKey k =
  if T.elem '{' k -- hole
    then "_"
    else k
