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
import Entity.Source
import Entity.TopNameMap
import Language.LSP.Types
import Scene.Source.Reflect qualified as Source

complete :: FilePath -> App [CompletionItem]
complete pathString = do
  mSrc <- Source.reflect pathString
  case mSrc of
    Nothing -> do
      return []
    Just src -> do
      childrenMap <- Unravel.getSourceChildrenMap
      sourceNameMap <- Global.getSourceNameMap
      let children = concat $ maybeToList $ Map.lookup (sourceFilePath src) childrenMap
      childCompItemList <- fmap concat $ forM children $ \(child, aliasInfo) -> do
        case Map.lookup (sourceFilePath child) sourceNameMap of
          Nothing -> do
            return []
          Just nameInfo -> do
            let nameInfo' = Map.toList $ Map.filter (\(v, _) -> isPublic v) nameInfo
            let ddList = map fst nameInfo'
            mKeyArgList <- mapM KeyArg.lookupMaybe ddList
            let nameList = zip mKeyArgList $ map (LL.reify . DD.localLocator) ddList
            locator <- NE.head <$> getHumanReadableLocator (sourceModule src) child
            let fullyQualifiedNameList = map (\(mk, x) -> (mk, locator <> nsSep <> x)) nameList
            case getRawAlias aliasInfo of
              Nothing -> do
                let ns1 = map (newCompletionItem (Just locator)) nameList
                let ns2 = map (newCompletionItem (Just locator)) fullyQualifiedNameList
                return $ ns1 ++ ns2
              Just rawAlias -> do
                let aliasPrefixedNameList = map (\(mk, x) -> (mk, rawAlias <> nsSep <> x)) nameList
                let allNameList = aliasPrefixedNameList ++ fullyQualifiedNameList
                return $ map (newCompletionItem $ Just locator) allNameList
      case Map.lookup (sourceFilePath src) sourceNameMap of
        Nothing -> do
          return childCompItemList
        Just nameInfo -> do
          let nameInfo' = Map.toList nameInfo
          let ddList = map fst nameInfo'
          mKeyArgList <- mapM KeyArg.lookupMaybe ddList
          let nameList = zip mKeyArgList $ map (LL.reify . DD.localLocator) ddList
          return $ map (newCompletionItem Nothing) nameList ++ childCompItemList

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
