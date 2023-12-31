module Scene.Parse.Import
  ( activateImport,
    interpretImport,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Global qualified as Global
import Context.Module qualified as Module
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Context.UnusedImport qualified as UnusedImport
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Context.UnusedPreset qualified as UnusedPreset
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.AliasInfo qualified as AI
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.GlobalLocatorAlias qualified as GLA
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Module
import Entity.ModuleAlias (ModuleAlias (ModuleAlias))
import Entity.ModuleID qualified as MID
import Entity.RawProgram
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Syntax.Series qualified as SE
import Path
import Scene.Module.Reflect qualified as Module
import Scene.Source.ShiftToLatest

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

interpretImport :: Source.Source -> Maybe RawImport -> App [(Source.Source, [AI.AliasInfo])]
interpretImport currentSource importOrNone = do
  case importOrNone of
    Nothing ->
      return []
    Just (RawImport _ _ importItemList) -> do
      fmap concat $ forM (SE.extract importItemList) $ \rawImport -> do
        let RawImportItem m (locatorText, _) localLocatorList = rawImport
        let localLocatorList' = SE.extract localLocatorList
        interpretImportItem True (Source.sourceModule currentSource) m locatorText localLocatorList'

interpretImportItem ::
  Bool ->
  Module ->
  Hint ->
  LocatorText ->
  [(Hint, LL.LocalLocator)] ->
  App [(Source.Source, [AI.AliasInfo])]
interpretImportItem shouldUpdateTag currentModule m locatorText localLocatorList = do
  baseNameList <- Throw.liftEither $ BN.bySplit m locatorText
  case baseNameList of
    [] ->
      Throw.raiseCritical m "Scene.Parse.Import: empty parse locator"
    [baseName]
      | Just (moduleAlias, sourceLocator) <- Map.lookup baseName (modulePrefixMap currentModule) -> do
          sgl <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
          source <- getSource m sgl locatorText
          let gla = GLA.GlobalLocatorAlias baseName
          return [(source, [AI.Use sgl localLocatorList, AI.Prefix m gla sgl])]
      | Just (_, digest) <- Map.lookup (ModuleAlias baseName) (moduleDependency currentModule) -> do
          unless (null localLocatorList) $ do
            Throw.raiseError m "found a non-empty locator list when using alias import"
          nextModule <- Module.getModule m (MID.Library digest) locatorText
          let presetInfo = Map.toList $ modulePresetMap nextModule
          UnusedPreset.insert (MID.reify $ moduleID nextModule) m
          ensFileHint <- getEnsFileHint m nextModule
          Tag.insertFileLoc m (T.length locatorText) ensFileHint
          let m' = m {metaShouldSaveLocation = False}
          fmap concat $ forM presetInfo $ \(presetSourceLocator, presetLocalLocatorList) -> do
            let newLocatorText = BN.reify baseName <> nsSep <> presetSourceLocator
            let presetLocalLocatorList' = map ((m',) . LL.new) presetLocalLocatorList
            interpretImportItem False nextModule m' newLocatorText presetLocalLocatorList'
      | otherwise ->
          Throw.raiseError m $ "no such prefix or alias is defined: " <> BN.reify baseName
    aliasText : locator ->
      case SL.fromBaseNameList locator of
        Nothing ->
          Throw.raiseError m $ "couldn't parse the locator: " <> locatorText
        Just sourceLocator -> do
          let moduleAlias = ModuleAlias aliasText
          sgl <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
          when shouldUpdateTag $ do
            UnusedImport.insert (SGL.reify sgl) m locatorText
            forM_ localLocatorList $ \(ml, ll) -> UnusedLocalLocator.insert ll ml
          source <- getSource m sgl locatorText
          return [(source, [AI.Use sgl localLocatorList])]

getSource :: Hint -> SGL.StrictGlobalLocator -> LocatorText -> App Source.Source
getSource m sgl locatorText = do
  nextModule <- Module.getModule m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  let nextPath = getSourceDir nextModule </> relPath
  Tag.insertFileLoc m (T.length locatorText) (newSourceHint nextPath)
  shiftToLatest $
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = nextPath,
        Source.sourceHint = Just m
      }

getEnsFileHint :: Hint -> Module -> App Hint
getEnsFileHint m baseModule = do
  moduleFilePath <- Module.getModuleFilePath (Just m) (moduleID baseModule)
  return $ newSourceHint moduleFilePath
