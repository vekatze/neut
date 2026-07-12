module Kernel.Common.Trace
  ( Config,
    empty,
    new,
    isEnabled,
    matches,
  )
where

import Console.ReportMode qualified as Report
import Data.Bifunctor (first)
import Data.Text qualified as T
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Module qualified as M
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.GlobalLocator qualified as GL
import Logger.Hint (internalHint)

data Config = Config
  { phaseList :: [Report.TracePhase],
    prefixList :: [T.Text],
    mainModule :: Maybe M.MainModule
  }

empty :: Config
empty = Config {phaseList = [], prefixList = [], mainModule = Nothing}

isEnabled :: Config -> Bool
isEnabled config =
  not $ null $ phaseList config

new :: M.MainModule -> Report.TraceConfig -> Either T.Text Config
new mainModule traceConfig = do
  let phaseList = Report.tracePhaseList traceConfig
  let prefixList = Report.tracePrefixList traceConfig
  prefixList' <- mapM normalizePrefix prefixList
  return Config {phaseList, prefixList = prefixList', mainModule = Just mainModule}

normalizePrefix :: T.Text -> Either T.Text T.Text
normalizePrefix prefix = do
  locator <- first (const $ "Invalid trace prefix: " <> prefix) $ GL.reflect internalHint prefix
  return $ GL.reify locator

matches :: Config -> ModulePath.ModulePathMap -> Report.TracePhase -> DD.DefiniteDescription -> Bool
matches config modulePathMap phase dd = do
  case mainModule config of
    Nothing ->
      False
    Just mainModule' ->
      phase `elem` phaseList config
        && any (`isPrefixOfLocator` ModulePath.renderDD modulePathMap dd) (prefixList config)

isPrefixOfLocator :: T.Text -> T.Text -> Bool
isPrefixOfLocator prefix name = do
  prefix == name
    || case T.stripPrefix prefix name of
      Just rest ->
        case T.uncons rest of
          Just (c, _) -> c == '.' || c == '#' || c == ';'
          Nothing -> True
      Nothing -> False
