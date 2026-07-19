module Language.Common.GlobalLocator
  ( GlobalLocator (..),
    reify,
    reflect,
    reflectModulePath,
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
  { modulePath :: [ModuleAlias],
    sourceLocator :: SL.SourceLocator
  }
  deriving (Generic, Show, Eq)

instance Binary GlobalLocator

instance Hashable GlobalLocator

reify :: GlobalLocator -> T.Text
reify GlobalLocator {modulePath, sourceLocator} = do
  let modulePathText = case modulePath of
        [] -> MA.reify MA.thisModuleAlias
        _ -> T.intercalate nsSep $ map MA.reify modulePath
  modulePathText <> doubleColon <> SL.toText sourceLocator

reflect :: H.Hint -> T.Text -> Either Error GlobalLocator
reflect m rawTxt = do
  case T.splitOn doubleColon rawTxt of
    [modulePathText, sourceText]
      | not (T.isInfixOf ":" modulePathText),
        not (T.isInfixOf ":" sourceText) -> do
          modulePath <- reflectModulePath m modulePathText
          sourceLocator <- reflectSourceLocator m rawTxt sourceText
          return GlobalLocator {modulePath, sourceLocator}
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

reflectModulePath :: H.Hint -> T.Text -> Either Error [ModuleAlias]
reflectModulePath m modulePathText =
  reflectModulePathComponents m modulePathText $ T.splitOn nsSep modulePathText

reflectModulePathComponents :: H.Hint -> T.Text -> [T.Text] -> Either Error [ModuleAlias]
reflectModulePathComponents m modulePathText modulePathTextList =
  case modulePathTextList of
    [] ->
      Left $ newError m $ "Invalid module path: `" <> modulePathText <> "`"
    _ -> do
      aliases <- forM modulePathTextList $ \aliasText -> do
        when (T.null aliasText) $ do
          Left $ newError m $ "Invalid module path: `" <> modulePathText <> "`"
        alias <- BN.reflect m aliasText
        return $ ModuleAlias alias
      return $ filter (/= MA.thisModuleAlias) aliases
