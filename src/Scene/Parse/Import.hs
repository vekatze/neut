module Scene.Parse.Import
  ( activateImport,
    interpretImport,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Global qualified as Global
import Context.RawImportSummary qualified as RawImportSummary
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.AliasInfo qualified as AI
import Entity.BaseName qualified as BN
import Entity.C
import Entity.Const
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Module
import Entity.ModuleAlias (ModuleAlias (ModuleAlias))
import Entity.RawProgram
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Syntax.Series qualified as SE
import Path
import Scene.Module.GetEnabledPreset
import Scene.Module.Reflect qualified as Module

type LocatorText =
  T.Text

activateImport :: Hint -> [(Source.Source, [AI.AliasInfo])] -> App ()
activateImport m sourceInfoList = do
  forM_ sourceInfoList $ \(source, aliasInfoList) -> do
    let path = Source.sourceFilePath source
    namesInSource <- Global.lookupSourceNameMap m path
    Global.activateTopLevelNames namesInSource
    forM_ aliasInfoList $ \aliasInfo ->
      Alias.activateAliasInfo namesInSource aliasInfo

interpretImport :: Hint -> Source.Source -> [(RawImport, C)] -> App [(Source.Source, [AI.AliasInfo])]
interpretImport m currentSource importList = do
  presetImportList <- interpretPreset m (Source.sourceModule currentSource)
  let (importList'@((RawImport _ _ importItemList _)), _) = mergeImportList m importList
  if SE.isEmpty importItemList
    then return presetImportList
    else do
      RawImportSummary.set importList'
      importItemList' <- fmap concat $ forM (SE.extract importItemList) $ \rawImportItem -> do
        let RawImportItem mItem (locatorText, _) localLocatorList = rawImportItem
        let localLocatorList' = SE.extract localLocatorList
        interpretImportItem True (Source.sourceModule currentSource) mItem locatorText localLocatorList'
      return $ presetImportList ++ importItemList'

interpretImportItem ::
  AI.MustUpdateTag ->
  Module ->
  Hint ->
  LocatorText ->
  [(Hint, LL.LocalLocator)] ->
  App [(Source.Source, [AI.AliasInfo])]
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
          return [(source, [AI.Use mustUpdateTag sgl localLocatorList, AI.Prefix m gla sgl])]
      | otherwise ->
          Throw.raiseError m $ "no such prefix is defined: " <> BN.reify baseName
    aliasText : locator ->
      case SL.fromBaseNameList locator of
        Nothing ->
          Throw.raiseError m $ "couldn't parse the locator: " <> locatorText
        Just sourceLocator -> do
          let moduleAlias = ModuleAlias aliasText
          sgl <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
          when mustUpdateTag $ do
            UnusedGlobalLocator.insert (SGL.reify sgl) m locatorText
            forM_ localLocatorList $ \(ml, ll) -> UnusedLocalLocator.insert ll ml
          source <- getSource mustUpdateTag m sgl locatorText
          return [(source, [AI.Use mustUpdateTag sgl localLocatorList])]

getSource :: AI.MustUpdateTag -> Hint -> SGL.StrictGlobalLocator -> LocatorText -> App Source.Source
getSource mustUpdateTag m sgl locatorText = do
  nextModule <- Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  let nextPath = getSourceDir nextModule </> relPath
  when mustUpdateTag $
    Tag.insertFileLoc m (T.length locatorText) (newSourceHint nextPath)
  return $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = nextPath,
        Source.sourceHint = Just m
      }

interpretPreset :: Hint -> Module -> App [(Source.Source, [AI.AliasInfo])]
interpretPreset m currentModule = do
  presetInfo <- getEnabledPreset currentModule
  fmap concat $ forM presetInfo $ \(locatorText, presetLocalLocatorList) -> do
    let presetLocalLocatorList' = map ((m,) . LL.new) presetLocalLocatorList
    interpretImportItem False currentModule m locatorText presetLocalLocatorList'
