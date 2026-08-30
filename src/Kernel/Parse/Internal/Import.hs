module Kernel.Parse.Internal.Import
  ( Handle,
    new,
    isLive,
    interpretImport,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Gensym.Handle qualified as Gensym
import Kernel.Common.Const
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.RawImportSummary qualified as RawImportSummary
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Import (ImportItem (..), Liveness)
import Kernel.Common.Capability qualified as Capability
import Kernel.Common.Import qualified as I
import Kernel.Common.Module
import Kernel.Common.Module.GetEnabledPreset qualified as GetEnabledPreset
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.Source qualified as Source
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.BranchAgreement qualified as BranchAgreement
import Kernel.Parse.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.Internal.Handle.UnusedTopLevelName qualified as UnusedTopLevelName
import Language.Common.BaseName qualified as BN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.GlobalLocator qualified as GL
import Language.Common.LocalLocator qualified as LL
import Language.Common.ModuleID (ModuleID)
import Language.Common.SourceLocator qualified as SL
import Language.Common.SourcePrefix qualified as SP
import Language.Common.StrictGlobalLocator qualified as SGL
import Language.RawTerm.Name (isConsName)
import Language.RawTerm.RawStmt
import Logger.Hint
import Path
import SyntaxTree.C
import SyntaxTree.Series qualified as SE

type LocatorText =
  T.Text

data Handle = Handle
  { envHandle :: Env.Handle,
    unusedHandle :: Unused.Handle,
    getEnabledPresetHandle :: GetEnabledPreset.Handle,
    shiftToLatestHandle :: STL.Handle,
    locatorHandle :: Locator.Handle,
    aliasHandle :: Alias.Handle,
    gensymHandle :: Gensym.Handle,
    rawImportSummaryHandle :: RawImportSummary.Handle,
    moduleHandle :: Module.Handle,
    globalNameMapHandle :: GlobalNameMap.Handle,
    tagHandle :: Tag.Handle,
    platformHandle :: Platform.Handle,
    unusedTopLevelNameHandle :: UnusedTopLevelName.Handle,
    targetCapabilities :: S.Set Capability.Capability,
    branchAgreementHandle :: BranchAgreement.Handle,
    presetCacheRef :: IORef (Map.HashMap ModuleID [ImportItem])
  }

new ::
  Gensym.Handle ->
  Global.Handle ->
  Local.Handle ->
  Handle
new gensymHandle globalHandle@(Global.Handle {..}) (Local.Handle {..}) = do
  let getEnabledPresetHandle = GetEnabledPreset.new globalHandle
  let shiftToLatestHandle = STL.new antecedentHandle
  let targetCapabilities = Capability.providedBy (Platform.getSelector platformHandle)
  Handle {..}

isLive :: Handle -> Liveness -> Bool
isLive h =
  I.isLiveIn (targetCapabilities h)

interpretImport :: Handle -> Hint -> Source.Source -> [(RawImport, C)] -> App [ImportItem]
interpretImport h m currentSource importList = do
  presetImportList <- interpretPreset h m (Source.sourceModule currentSource)
  let (importList'@((RawImport _ _ importItemList _)), _) = mergeImportList m importList
  if SE.isEmpty importItemList
    then return presetImportList
    else do
      liftIO $ RawImportSummary.set (rawImportSummaryHandle h) importList'
      let items = SE.extract importItemList
      mapM_ (ensureConditionalImportConsistency False) items
      importItemList' <- concat <$> mapM (interpretRawImportItem h currentSource I.everywhere) items
      return $ presetImportList ++ importItemList'

interpretRawImportItem :: Handle -> Source.Source -> Liveness -> RawImportItem -> App [ImportItem]
interpretRawImportItem h currentSource liveness rawImportItem = do
  case rawImportItem of
    RawImportItem mItem (locatorText, _) entrySeries -> do
      let entries = map interpretRawEntry $ SE.extract entrySeries
      interpretImportItem h liveness (isLive h liveness) mItem locatorText entries
    RawStaticFileKey _ _ keys -> do
      interpretImportItemStaticFile h (Source.sourceModule currentSource) $ SE.extract keys
    RawConditionalImport _ _ (mCapability, capabilityText, _) (thenSeries, _) _ elseSeries -> do
      capability <- interpretCapability mCapability capabilityText
      recordBranchAgreement h mCapability (SE.extract thenSeries) (SE.extract elseSeries)
      thenItems <- concat <$> mapM (interpretRawImportItem h currentSource (I.whereProvided capability liveness)) (SE.extract thenSeries)
      elseItems <- concat <$> mapM (interpretRawImportItem h currentSource (I.whereNotProvided capability liveness)) (SE.extract elseSeries)
      return $ thenItems ++ elseItems

recordBranchAgreement :: Handle -> Hint -> [RawImportItem] -> [RawImportItem] -> App ()
recordBranchAgreement h m thenList elseList = do
  thenMap <- branchNameMap h thenList
  elseMap <- branchNameMap h elseList
  forM_ (Map.toList thenMap) $ \(name, thenDD) ->
    case Map.lookup name elseMap of
      Nothing ->
        return ()
      Just elseDD ->
        liftIO $
          BranchAgreement.insert (branchAgreementHandle h) $
            BranchAgreement.Obligation
              { BranchAgreement.obligationHint = m,
                BranchAgreement.thenName = thenDD,
                BranchAgreement.elseName = elseDD
              }

branchNameMap :: Handle -> [RawImportItem] -> App (Map.HashMap T.Text DD.DefiniteDescription)
branchNameMap h itemList = do
  fmap (Map.fromList . concat) $ forM itemList $ \item ->
    case item of
      RawImportItem mItem (locatorText, _) entrySeries -> do
        gl <- liftEither $ GL.reflect mItem locatorText
        sgl <- Alias.resolveAlias (aliasHandle h) mItem gl
        return $ flip mapMaybe (SE.extract entrySeries) $ \entry ->
          case entry of
            RawImportName _ ll asClauseOrNone -> do
              let name = maybe (LL.reify ll) (\(RawAsClause _ _ _ alias) -> BN.reify alias) asClauseOrNone
              Just (name, DD.new sgl ll)
            RawImportWildcard {} ->
              Nothing
      RawStaticFileKey {} ->
        return []
      RawConditionalImport _ _ _ (thenSeries, _) _ _ ->
        Map.toList <$> branchNameMap h (SE.extract thenSeries)

ensureConditionalImportConsistency :: Bool -> RawImportItem -> App ()
ensureConditionalImportConsistency isInBranch rawImportItem = do
  case rawImportItem of
    RawImportItem mItem _ entries ->
      when isInBranch $
        case SE.extract entries of
          [] ->
            raiseError mItem "An item of a conditional import must list the names it supplies"
          entryList ->
            mapM_ ensureBranchEntryHasType entryList
    RawStaticFileKey mItem _ _ ->
      when isInBranch $
        raiseError mItem "A static file cannot be supplied by a conditional import"
    RawConditionalImport _ _ (mCapability, _, _) (thenSeries, _) _ elseSeries -> do
      let thenList = SE.extract thenSeries
      let elseList = SE.extract elseSeries
      mapM_ (ensureConditionalImportConsistency True) thenList
      mapM_ (ensureConditionalImportConsistency True) elseList
      ensureSameBoundNames mCapability thenList elseList

ensureBranchEntryHasType :: RawImportEntry -> App ()
ensureBranchEntryHasType entry =
  case entry of
    RawImportName {} ->
      return ()
    RawImportWildcard m (RawAsClause _ _ _ importAlias) ->
      raiseError m $
        "A name without a type cannot be supplied by a conditional import: `" <> BN.reify importAlias <> "`"

interpretCapability :: Hint -> T.Text -> App Capability.Capability
interpretCapability m name = do
  case Capability.reflect name of
    Just capability ->
      return capability
    Nothing ->
      raiseError m $ "No such capability exists: `" <> name <> "`"

ensureSameBoundNames :: Hint -> [RawImportItem] -> [RawImportItem] -> App ()
ensureSameBoundNames m thenList elseList = do
  let thenNames = S.fromList $ concatMap boundNameList thenList
  let elseNames = S.fromList $ concatMap boundNameList elseList
  let onlyInThen = S.difference thenNames elseNames
  let onlyInElse = S.difference elseNames thenNames
  unless (S.null onlyInThen && S.null onlyInElse) $ do
    raiseError m $
      "The two branches of this import must supply the same names"
        <> renderNameDifference "only in `if`" onlyInThen
        <> renderNameDifference "only in `else`" onlyInElse

renderNameDifference :: T.Text -> S.Set T.Text -> T.Text
renderNameDifference label nameSet =
  if S.null nameSet
    then ""
    else "\n" <> label <> ": " <> T.intercalate ", " (S.toList nameSet)

interpretImportItemStaticFile ::
  Handle ->
  Module ->
  [(Hint, T.Text)] ->
  App [ImportItem]
interpretImportItemStaticFile h currentModule keyList = do
  currentModule' <- STL.shiftToLatestModule (shiftToLatestHandle h) currentModule
  let moduleRootDir = getModuleRootDir currentModule'
  pathList <- forM keyList $ \(mKey, key) -> do
    case Map.lookup key (moduleStaticFiles currentModule') of
      Just path -> do
        let fullPath = moduleRootDir </> path
        liftIO $ Tag.insertStaticFile (tagHandle h) mKey key (newSourceHint fullPath)
        liftIO $ Unused.insertStaticFile (unusedHandle h) key mKey
        return (key, (mKey, fullPath))
      Nothing ->
        raiseError mKey $ "No such static file is defined: " <> key
  return [StaticFileKey pathList]

interpretRawEntry :: RawImportEntry -> I.ImportedEntry
interpretRawEntry entry =
  case entry of
    RawImportName m ll Nothing ->
      I.ImportedName m ll Nothing
    RawImportName m ll (Just (RawAsClause _ _ mAs importAlias)) ->
      I.ImportedName m ll $ Just (mAs, importAlias)
    RawImportWildcard _ (RawAsClause _ _ mAs importAlias) ->
      I.NamespaceView mAs importAlias

interpretImportItem ::
  Handle ->
  Liveness ->
  I.MustUpdateTag ->
  Hint ->
  LocatorText ->
  [I.ImportedEntry] ->
  App [ImportItem]
interpretImportItem h liveness mustUpdateTag m locatorText entries = do
  gl <- liftEither $ GL.reflect m locatorText
  sgl <- Alias.resolveAlias (aliasHandle h) m gl
  forM_ entries ensureImportAliasConsistency
  unless (isLive h liveness) $ do
    forM_ entries $ \entry ->
      case entry of
        I.ImportedName _ ll _ ->
          liftIO $ UnusedTopLevelName.recordReference (unusedTopLevelNameHandle h) Nothing (DD.new sgl ll)
        I.NamespaceView {} ->
          return ()
  when mustUpdateTag $ do
    liftIO $ Unused.insertGlobalLocator (unusedHandle h) (SGL.reify sgl) m locatorText
    forM_ entries $ \entry ->
      case entry of
        I.ImportedName mImportedName ll explicitAliasOrNone -> do
          let (mImportAlias, importAlias) = maybe (mImportedName, LL.baseName ll) id explicitAliasOrNone
          liftIO $ Unused.insertLocalLocator (unusedHandle h) (LL.new importAlias) mImportAlias
        I.NamespaceView mImportAlias importAlias ->
          liftIO $ Unused.insertLocalLocator (unusedHandle h) (LL.new importAlias) mImportAlias
  source <- getSource h mustUpdateTag m sgl locatorText
  return [ImportItem liveness source [I.ImportUse mustUpdateTag sgl entries]]

ensureImportAliasConsistency :: I.ImportedEntry -> App ()
ensureImportAliasConsistency entry =
  case entry of
    I.ImportedName _ ll (Just (mImportAlias, importAlias))
      | isConsName (BN.reify (LL.baseName ll)) /= isConsName (BN.reify importAlias) ->
          raiseError mImportAlias $
            "The import alias `"
              <> BN.reify importAlias
              <> "` must be capitalized like `"
              <> BN.reify (LL.baseName ll)
              <> "`"
    _ ->
      return ()

getSource :: Handle -> I.MustUpdateTag -> Hint -> SGL.StrictGlobalLocator -> LocatorText -> App Source.Source
getSource h mustUpdateTag m sgl locatorText = do
  let h' = GetModule.Handle {moduleHandle = moduleHandle h}
  let mainModule = Env.getMainModule (envHandle h)
  nextModule <- GetModule.getModule h' mainModule m (SGL.moduleID sgl) locatorText
  ensureSourceImportability h m sgl locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  let nextPath = getSourceDir nextModule </> relPath
  when mustUpdateTag $ do
    case T.splitOn doubleColon locatorText of
      [modulePathText, sourceText] -> do
        liftIO $ Tag.insertModuleFile (tagHandle h) m modulePathText (moduleLocation nextModule)
        let (line, column) = metaLocation m
        let sourceHint = m {metaLocation = (line, column + T.length modulePathText + T.length doubleColon)}
        liftIO $ Tag.insertResolvedSourceFile (tagHandle h) sourceHint sourceText (SGL.reify sgl) (newSourceHint nextPath)
      _ ->
        raiseError m $ "Invalid global locator: `" <> locatorText <> "`"
  STL.shiftToLatest
    (shiftToLatestHandle h)
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = nextPath,
        Source.sourceHint = Just m,
        Source.sourceImportLocator = Just locatorText
      }

ensureSourceImportability ::
  Handle ->
  Hint ->
  SGL.StrictGlobalLocator ->
  LocatorText ->
  App ()
ensureSourceImportability h m sgl locatorText = do
  let currentGlobalLocator = Locator.getCurrentGlobalLocator (locatorHandle h)
  unless (SP.canImport currentGlobalLocator sgl) $
    raiseError m $
      "The source `" <> locatorText <> "` is not visible from this source"

interpretPreset :: Handle -> Hint -> Module -> App [ImportItem]
interpretPreset h m currentModule = do
  ref <- liftIO $ readIORef (presetCacheRef h)
  case Map.lookup (moduleID currentModule) ref of
    Just items ->
      return items
    Nothing -> do
      presetInfo <- GetEnabledPreset.getEnabledPreset (getEnabledPresetHandle h) currentModule
      items <- fmap concat $ forM presetInfo $ \(locatorText, presetNameList) -> do
        let entries = map (\name -> I.ImportedName m (LL.new name) Nothing) presetNameList
        interpretImportItem h I.everywhere False m locatorText entries
      liftIO $ modifyIORef' (presetCacheRef h) $ Map.insert (moduleID currentModule) items
      return items
