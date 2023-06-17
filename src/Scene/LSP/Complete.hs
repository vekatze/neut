module Scene.LSP.Complete (complete) where

import Context.App
import Context.Global qualified as Global
import Context.KeyArg qualified as KeyArg
import Context.Unravel qualified as Unravel
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (maybeToList)
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
import Language.LSP.Types
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
          childCompItemList <- fmap concat $ forM children $ \(child, aliasInfo) -> do
            getChildCompItemList sourceNameMap (sourceModule src) child aliasInfo
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
  AliasInfo ->
  App [CompletionItem]
getChildCompItemList sourceNameMap sourceModule child aliasInfo = do
  case Map.lookup (sourceFilePath child) sourceNameMap of
    Nothing -> do
      return []
    Just topNameMap -> do
      nameList <- getLocalNameList topNameMap
      locator <- NE.head <$> getHumanReadableLocator sourceModule child
      let fullyQualifiedNameList = map (\(mk, x) -> (mk, locator <> nsSep <> x)) nameList
      let newCompletionItem' = newCompletionItem (Just locator)
      case getRawAlias aliasInfo of
        Nothing -> do
          let ns1 = map newCompletionItem' nameList
          let ns2 = map newCompletionItem' fullyQualifiedNameList
          return $ ns1 ++ ns2
        Just rawAlias -> do
          let aliasPrefixedNameList = map (\(mk, x) -> (mk, rawAlias <> nsSep <> x)) nameList
          let allNameList = aliasPrefixedNameList ++ fullyQualifiedNameList
          return $ map newCompletionItem' allNameList

getLocalNameList :: TopNameMap -> App [(Maybe (IsConstLike, [Key]), T.Text)]
getLocalNameList nameInfo = do
  let ddList = map fst $ Map.toList nameInfo
  mKeyArgList <- mapM KeyArg.lookupMaybe ddList
  return $ zip mKeyArgList $ map (LL.reify . DD.localLocator) ddList

newCompletionItem :: Maybe T.Text -> (Maybe (IsConstLike, [Key]), T.Text) -> CompletionItem
newCompletionItem mLocator (mKeyArg, t) =
  CompletionItem
    t
    (Just CiFunction)
    (Just (List []))
    (mLocator >>= \locator -> Just ("in " <> locator))
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    (stylizeKeyArgList t mKeyArg)
    (Just Snippet)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

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
