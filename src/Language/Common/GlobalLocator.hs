module Language.Common.GlobalLocator
  ( GlobalLocator (..),
    reify,
    reflect,
    reflectModuleRoute,
  )
where

import App.Error
import Control.Monad
import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics
import Language.Common.BaseName qualified as BN
import Language.Common.Const
import Language.Common.ModuleAlias (ModuleAlias (..))
import Language.Common.ModuleAlias qualified as MA
import Language.Common.SourceLocator qualified as SL
import Logger.Hint qualified as H

data GlobalLocator = GlobalLocator
  { moduleRoute :: [ModuleAlias],
    sourceLocator :: SL.SourceLocator
  }
  deriving (Generic, Show, Eq)

instance Binary GlobalLocator

instance Hashable GlobalLocator

reify :: GlobalLocator -> T.Text
reify gl =
  case gl of
    GlobalLocator {moduleRoute = [], sourceLocator} ->
      SL.toText sourceLocator
    GlobalLocator {moduleRoute, sourceLocator} ->
      T.intercalate nsSep (map MA.reify moduleRoute) <> routeSep <> SL.toText sourceLocator

reflect :: H.Hint -> T.Text -> Either Error GlobalLocator
reflect m rawTxt = do
  case T.splitOn routeSep rawTxt of
    [sourceText]
      | not (T.isInfixOf ":" sourceText) -> do
          sourceLocator <- reflectSourceLocator m rawTxt sourceText
          return GlobalLocator {moduleRoute = [], sourceLocator}
    [routeText, sourceText]
      | not (T.isInfixOf ":" routeText),
        not (T.isInfixOf ":" sourceText) -> do
          moduleRoute <- reflectModuleRoute m routeText
          sourceLocator <- reflectSourceLocator m rawTxt sourceText
          return GlobalLocator {moduleRoute, sourceLocator}
    _ ->
      Left $ newError m $ "Invalid global locator: `" <> rawTxt <> "`"

reflectSourceLocator :: H.Hint -> T.Text -> T.Text -> Either Error SL.SourceLocator
reflectSourceLocator m rawTxt sourceText =
  reflectSourceLocatorFromList m rawTxt $ T.splitOn nsSep sourceText

reflectSourceLocatorFromList :: H.Hint -> T.Text -> [T.Text] -> Either Error SL.SourceLocator
reflectSourceLocatorFromList m rawTxt sourceTextList = do
  sourceBaseNameList <- mapM (BN.reflect m) sourceTextList
  case SL.fromBaseNameList sourceBaseNameList of
    Just sourceLocator ->
      return sourceLocator
    Nothing ->
      Left $ newError m $ "Invalid global locator: `" <> rawTxt <> "`"

reflectModuleRoute :: H.Hint -> T.Text -> Either Error [ModuleAlias]
reflectModuleRoute m routeText =
  reflectModuleRouteComponents m routeText $ T.splitOn nsSep routeText

reflectModuleRouteComponents :: H.Hint -> T.Text -> [T.Text] -> Either Error [ModuleAlias]
reflectModuleRouteComponents m routeText routeTextList =
  case routeTextList of
    [""] ->
      return []
    _ ->
      forM routeTextList $ \aliasText -> do
        when (T.null aliasText) $ do
          Left $ newError m $ "Invalid module route: `" <> routeText <> "`"
        alias <- BN.reflect m aliasText
        return $ ModuleAlias alias
