module Move.Scene.Parse.Import
  ( activateImport,
    interpretImport,
  )
where

import Move.Context.Alias qualified as Alias
import Move.Context.App
import Move.Context.Global qualified as Global
import Move.Context.Locator qualified as Locator
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.Tag qualified as Tag
import Move.Context.Throw qualified as Throw
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Rule.AliasInfo qualified as AI
import Rule.BaseName qualified as BN
import Rule.C
import Rule.Const
import Rule.GlobalLocatorAlias qualified as GLA
import Rule.Hint
import Rule.Import (ImportItem (..))
import Rule.LocalLocator qualified as LL
import Rule.Module
import Rule.ModuleAlias (ModuleAlias (ModuleAlias))
import Rule.RawProgram
import Rule.Source qualified as Source
import Rule.SourceLocator qualified as SL
import Rule.StrictGlobalLocator qualified as SGL
import Rule.Syntax.Series qualified as SE
import Path
import Move.Scene.Module.GetEnabledPreset
import Move.Scene.Module.Reflect qualified as Module
import Move.Scene.Source.ShiftToLatest

type LocatorText =
  T.Text

activateImport :: Hint -> [ImportItem] -> App ()
activateImport m sourceInfoList = do
  forM_ sourceInfoList $ \importItem -> do
    case importItem of
      ImportItem source aliasInfoList -> do
        let path = Source.sourceFilePath source
        namesInSource <- Global.lookupSourceNameMap m path
        Global.activateTopLevelNames namesInSource
        forM_ aliasInfoList $ \aliasInfo ->
          Alias.activateAliasInfo namesInSource aliasInfo
      StaticKey pathList -> do
        forM_ pathList $ \(key, (mKey, path)) -> do
          Locator.activateStaticFile mKey key path

interpretImport :: Hint -> Source.Source -> [(RawImport, C)] -> App [ImportItem]
interpretImport m currentSource importList = do
  presetImportList <- interpretPreset m (Source.sourceModule currentSource)
  let (importList'@((RawImport _ _ importItemList _)), _) = mergeImportList m importList
  if SE.isEmpty importItemList
    then return presetImportList
    else do
      RawImportSummary.set importList'
      importItemList' <- fmap concat $ forM (SE.extract importItemList) $ \rawImportItem -> do
        case rawImportItem of
          RawImportItem mItem (locatorText, _) localLocatorList -> do
            let localLocatorList' = SE.extract localLocatorList
            interpretImportItem True (Source.sourceModule currentSource) mItem locatorText localLocatorList'
          RawStaticKey _ _ keys -> do
            let keys' = SE.extract keys
            interpretImportItemStatic (Source.sourceModule currentSource) keys'
      return $ presetImportList ++ importItemList'

interpretImportItemStatic ::
  Module ->
  [(Hint, T.Text)] ->
  App [ImportItem]
interpretImportItemStatic currentModule keyList = do
  currentModule' <- shiftToLatestModule currentModule
  let moduleRootDir = getModuleRootDir currentModule'
  pathList <- forM keyList $ \(mKey, key) -> do
    case Map.lookup key (moduleStaticFiles currentModule') of
      Just path -> do
        let fullPath = moduleRootDir </> path
        Tag.insertFileLoc mKey (T.length key) (newSourceHint fullPath)
        UnusedStaticFile.insert key mKey
        return (key, (mKey, fullPath))
      Nothing ->
        Throw.raiseError mKey $ "No such static file is defined: " <> key
  return [StaticKey pathList]

interpretImportItem ::
  AI.MustUpdateTag ->
  Module ->
  Hint ->
  LocatorText ->
  [(Hint, LL.LocalLocator)] ->
  App [ImportItem]
interpretImportItem mustUpdateTag currentModule m locatorText localLocatorList = do
  baseNameList <- Throw.liftEither $ BN.bySplit m locatorText
  case baseNameList of
    [] ->
      Throw.raiseCritical m "Scene.Parse.Import: empty parse locator"
    [baseName]
      | Just (moduleAlias, sourceLocator) <- Map.lookup baseName (modulePrefixMap currentModule) -> do
          sgl <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
          source <- getSource mustUpdateTag m sgl locatorText
          let gla = GLA.GlobalLocatorAlias baseName
          when mustUpdateTag $ do
            UnusedGlobalLocator.insert (SGL.reify sgl) m locatorText
            forM_ localLocatorList $ \(ml, ll) -> UnusedLocalLocator.insert ll ml
          return [ImportItem source [AI.Use mustUpdateTag sgl localLocatorList, AI.Prefix m gla sgl]]
      | otherwise ->
          Throw.raiseError m $ "No such prefix is defined: " <> BN.reify baseName
    aliasText : locator ->
      case SL.fromBaseNameList locator of
        Nothing ->
          Throw.raiseError m $ "Could not parse the locator: " <> locatorText
        Just sourceLocator -> do
          let moduleAlias = ModuleAlias aliasText
          sgl <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
          when mustUpdateTag $ do
            UnusedGlobalLocator.insert (SGL.reify sgl) m locatorText
            forM_ localLocatorList $ \(ml, ll) -> UnusedLocalLocator.insert ll ml
          source <- getSource mustUpdateTag m sgl locatorText
          return [ImportItem source [AI.Use mustUpdateTag sgl localLocatorList]]

getSource :: AI.MustUpdateTag -> Hint -> SGL.StrictGlobalLocator -> LocatorText -> App Source.Source
getSource mustUpdateTag m sgl locatorText = do
  nextModule <- Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  let nextPath = getSourceDir nextModule </> relPath
  when mustUpdateTag $
    Tag.insertFileLoc m (T.length locatorText) (newSourceHint nextPath)
  shiftToLatest
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = nextPath,
        Source.sourceHint = Just m
      }

interpretPreset :: Hint -> Module -> App [ImportItem]
interpretPreset m currentModule = do
  presetInfo <- getEnabledPreset currentModule
  fmap concat $ forM presetInfo $ \(locatorText, presetLocalLocatorList) -> do
    let presetLocalLocatorList' = map ((m,) . LL.new) presetLocalLocatorList
    interpretImportItem False currentModule m locatorText presetLocalLocatorList'
