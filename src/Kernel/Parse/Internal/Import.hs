module Kernel.Parse.Internal.Import
  ( Handle,
    new,
    interpretImport,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Text qualified as T
import Gensym.Handle qualified as Gensym
import Kernel.Common.Const
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.RawImportSummary qualified as RawImportSummary
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Import (ImportItem (..))
import Kernel.Common.Import qualified as I
import Kernel.Common.Module
import Kernel.Common.Module.GetEnabledPreset qualified as GetEnabledPreset
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.Source qualified as Source
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Language.Common.BaseName qualified as BN
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
  Handle {..}

interpretImport :: Handle -> Hint -> Source.Source -> [(RawImport, C)] -> App [ImportItem]
interpretImport h m currentSource importList = do
  presetImportList <- interpretPreset h m (Source.sourceModule currentSource)
  let (importList'@((RawImport _ _ importItemList _)), _) = mergeImportList m importList
  if SE.isEmpty importItemList
    then return presetImportList
    else do
      liftIO $ RawImportSummary.set (rawImportSummaryHandle h) importList'
      importItemList' <- fmap concat $ forM (SE.extract importItemList) $ \rawImportItem -> do
        case rawImportItem of
          RawImportItem mItem (locatorText, _) entrySeries -> do
            let entries = map interpretRawEntry $ SE.extract entrySeries
            interpretImportItem h True mItem locatorText entries
          RawStaticFileKey _ _ keys -> do
            let keys' = SE.extract keys
            interpretImportItemStaticFile h (Source.sourceModule currentSource) keys'
      return $ presetImportList ++ importItemList'

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
  I.MustUpdateTag ->
  Hint ->
  LocatorText ->
  [I.ImportedEntry] ->
  App [ImportItem]
interpretImportItem h mustUpdateTag m locatorText entries = do
  gl <- liftEither $ GL.reflect m locatorText
  sgl <- Alias.resolveAlias (aliasHandle h) m gl
  forM_ entries ensureImportAliasConsistency
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
  return [ImportItem source [I.ImportUse mustUpdateTag sgl entries]]

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
        interpretImportItem h False m locatorText entries
      liftIO $ modifyIORef' (presetCacheRef h) $ Map.insert (moduleID currentModule) items
      return items
