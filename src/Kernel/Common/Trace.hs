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
import Data.HashMap.Strict qualified as Map
import Data.List qualified as List
import Data.Text qualified as T
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Module qualified as M
import Language.Common.BaseName qualified as BN
import Language.Common.Const (doubleColon, nsSep)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.GlobalLocator qualified as GL
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID
import Language.Common.NamePath qualified as NamePath
import Language.Common.NamePrefix (NamePrefix (..))
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint (internalHint)

data Config = Config
  { phaseList :: [Report.TracePhase],
    prefixList :: [NamePrefix ModulePath.ModulePath],
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

normalizePrefix :: T.Text -> Either T.Text (NamePrefix ModulePath.ModulePath)
normalizePrefix prefix = do
  case T.splitOn doubleColon prefix of
    [modulePathText] -> do
      modulePath <- reflectModulePath prefix modulePathText
      return $ ModulePrefix $ map MA.reify modulePath
    [modulePathText, sourceText] -> do
      modulePath <- reflectModulePath prefix modulePathText
      sourcePath <- reflectNamePath prefix sourceText
      return $ SourcePrefix (map MA.reify modulePath) sourcePath
    [modulePathText, sourceText, bodyText] -> do
      modulePath <- reflectModulePath prefix modulePathText
      sourcePath <- reflectNamePath prefix sourceText
      if null sourcePath
        then Left $ invalidPrefix prefix
        else do
          bodyPath <- reflectNamePath prefix bodyText
          return $ BodyPrefix (map MA.reify modulePath) sourcePath bodyPath
    _ ->
      Left $ invalidPrefix prefix

invalidPrefix :: T.Text -> T.Text
invalidPrefix prefix =
  "Invalid trace prefix: " <> prefix

reflectModulePath :: T.Text -> T.Text -> Either T.Text [MA.ModuleAlias]
reflectModulePath original modulePathText = do
  first (const $ invalidPrefix original) $ GL.reflectModulePath internalHint modulePathText

reflectNamePath :: T.Text -> T.Text -> Either T.Text [BN.BaseName]
reflectNamePath original pathText = do
  let segmentTexts = T.splitOn nsSep pathText
  if T.null pathText || any T.null segmentTexts
    then Left $ invalidPrefix original
    else do
      segments <- first (const $ invalidPrefix original) $ mapM (BN.reflect internalHint) segmentTexts
      return $ NamePath.normalize segments

matches :: Config -> ModulePath.ModulePathMap -> Report.TracePhase -> DD.DefiniteDescription -> Bool
matches config modulePathMap phase dd = do
  case mainModule config of
    Nothing ->
      False
    Just _ ->
      phase `elem` phaseList config
        && any (matchesPrefix modulePathMap dd) (prefixList config)

matchesPrefix :: ModulePath.ModulePathMap -> DD.DefiniteDescription -> NamePrefix ModulePath.ModulePath -> Bool
matchesPrefix modulePathMap dd prefix = do
  let globalLocator = DD.strictGlobalLocator dd
  let moduleID = SGL.moduleID globalLocator
  let modulePath = Map.lookupDefault [MID.reify moduleID] moduleID modulePathMap
  let sourcePath = SGL.sourceSegments globalLocator
  let bodyPath = DD.bodySegments dd
  contains modulePath sourcePath bodyPath prefix

contains :: ModulePath.ModulePath -> [BN.BaseName] -> [BN.BaseName] -> NamePrefix ModulePath.ModulePath -> Bool
contains modulePath sourcePath bodyPath prefix = do
  case prefix of
    ModulePrefix requiredModulePath ->
      requiredModulePath `List.isPrefixOf` modulePath
    SourcePrefix requiredModulePath requiredSourcePath ->
      requiredModulePath == modulePath
        && requiredSourcePath `List.isPrefixOf` sourcePath
    BodyPrefix requiredModulePath requiredSourcePath requiredBodyPath ->
      requiredModulePath == modulePath
        && requiredSourcePath == sourcePath
        && requiredBodyPath `List.isPrefixOf` bodyPath
