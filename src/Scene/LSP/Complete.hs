module Scene.LSP.Complete (complete) where

import Context.App
import Context.Global qualified as Global
import Context.Unravel qualified as Unravel
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Entity.Const
import Entity.DefiniteDescription qualified as DD
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
      childCompItemList <- fmap concat $ forM children $ \child -> do
        case Map.lookup (sourceFilePath child) sourceNameMap of
          Nothing -> do
            return []
          Just nameInfo -> do
            let nameInfo' = Map.toList $ Map.filter (\(v, _) -> isPublic v) nameInfo
            let nameList = map (LL.reify . DD.localLocator . fst) nameInfo'
            locator <- NE.head <$> getHumanReadableLocator (sourceModule src) child
            let prefixedNameList = map (\x -> locator <> nsSep <> x) nameList
            return $ map (newCompletionItem $ Just locator) $ nameList ++ prefixedNameList
      case Map.lookup (sourceFilePath src) sourceNameMap of
        Nothing -> do
          return childCompItemList
        Just nameInfo -> do
          let nameInfo' = Map.toList nameInfo
          let nameList = map (LL.reify . DD.localLocator . fst) nameInfo'
          return $ map (newCompletionItem Nothing) nameList ++ childCompItemList

newCompletionItem :: Maybe T.Text -> T.Text -> CompletionItem
newCompletionItem mLocator t =
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
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
