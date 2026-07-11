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
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.GlobalLocator qualified as GL
import Logger.Hint (internalHint)

data Config = Config
  { phaseList :: [Report.TracePhase],
    prefixList :: [T.Text],
    modulePathMap :: ModulePath.ModulePathMap
  }

empty :: Config
empty = Config {phaseList = [], prefixList = [], modulePathMap = mempty}

isEnabled :: Config -> Bool
isEnabled config =
  not $ null $ phaseList config

new :: ModulePath.ModulePathMap -> Report.TraceConfig -> Either T.Text Config
new modulePathMap traceConfig = do
  let phaseList = Report.tracePhaseList traceConfig
  let prefixList = Report.tracePrefixList traceConfig
  prefixList' <- mapM normalizePrefix prefixList
  return Config {phaseList, prefixList = prefixList', modulePathMap}

normalizePrefix :: T.Text -> Either T.Text T.Text
normalizePrefix prefix = do
  locator <- first (const $ "Invalid trace prefix: " <> prefix) $ GL.reflect internalHint prefix
  return $ GL.reify locator

matches :: Config -> Report.TracePhase -> DD.DefiniteDescription -> Bool
matches config phase dd = do
  phase `elem` phaseList config
    && any (`isPrefixOfLocator` ModulePath.renderDD (modulePathMap config) dd) (prefixList config)

isPrefixOfLocator :: T.Text -> T.Text -> Bool
isPrefixOfLocator prefix name = do
  prefix == name
    || case T.stripPrefix prefix name of
      Just rest ->
        case T.uncons rest of
          Just (c, _) -> c == '.' || c == '#' || c == ';'
          Nothing -> True
      Nothing -> False
