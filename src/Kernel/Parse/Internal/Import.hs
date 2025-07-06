module Kernel.Parse.Internal.Import
  ( Handle,
    new,
    interpretImport,
  )
where

import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (raiseCritical, raiseError)
import Gensym.Handle qualified as Gensym
import Kernel.Common.AliasInfo qualified as AI
import Kernel.Common.Const
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.RawImportSummary qualified as RawImportSummary
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Import (ImportItem (..))
import Kernel.Common.Module
import Kernel.Common.Module.GetEnabledPreset qualified as GetEnabledPreset
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.Source qualified as Source
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Language.Common.BaseName qualified as BN
import Language.Common.LocalLocator qualified as LL
import Language.Common.ModuleAlias (ModuleAlias (ModuleAlias))
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
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
    tagHandle :: Tag.Handle
  }

new ::
  Global.Handle ->
  Local.Handle ->
  Handle
new globalHandle@(Global.Handle {..}) (Local.Handle {..}) = do
  let getEnabledPresetHandle = GetEnabledPreset.new globalHandle
  let shiftToLatestHandle = STL.new antecedentHandle
  Handle {..}

interpretImport :: Handle -> Hint -> Source.Source -> [(RawImport, C)] -> EIO [ImportItem]
interpretImport h m currentSource importList = do
  presetImportList <- interpretPreset h m (Source.sourceModule currentSource)
  let (importList'@((RawImport _ _ importItemList _)), _) = mergeImportList m importList
  if SE.isEmpty importItemList
    then return presetImportList
    else do
      liftIO $ RawImportSummary.set (rawImportSummaryHandle h) importList'
      importItemList' <- fmap concat $ forM (SE.extract importItemList) $ \rawImportItem -> do
        case rawImportItem of
          RawImportItem mItem (locatorText, _) localLocatorList -> do
            let localLocatorList' = SE.extract localLocatorList
            interpretImportItem h True mItem locatorText localLocatorList'
          RawStaticKey _ _ keys -> do
            let keys' = SE.extract keys
            interpretImportItemStatic h (Source.sourceModule currentSource) keys'
      return $ presetImportList ++ importItemList'

interpretImportItemStatic ::
  Handle ->
  Module ->
  [(Hint, T.Text)] ->
  EIO [ImportItem]
interpretImportItemStatic h currentModule keyList = do
  currentModule' <- STL.shiftToLatestModule (shiftToLatestHandle h) currentModule
  let moduleRootDir = getModuleRootDir currentModule'
  pathList <- forM keyList $ \(mKey, key) -> do
    case Map.lookup key (moduleStaticFiles currentModule') of
      Just path -> do
        let fullPath = moduleRootDir </> path
        liftIO $ Tag.insertFileLoc (tagHandle h) mKey (T.length key) (newSourceHint fullPath)
        liftIO $ Unused.insertStaticFile (unusedHandle h) key mKey
        return (key, (mKey, fullPath))
      Nothing ->
        raiseError mKey $ "No such static file is defined: " <> key
  return [StaticKey pathList]

interpretImportItem ::
  Handle ->
  AI.MustUpdateTag ->
  Hint ->
  LocatorText ->
  [(Hint, LL.LocalLocator)] ->
  EIO [ImportItem]
interpretImportItem h mustUpdateTag m locatorText localLocatorList = do
  baseNameList <- liftEither $ BN.bySplit m locatorText
  case baseNameList of
    [] ->
      raiseCritical m "Scene.Parse.Import: empty parse locator"
    aliasText : locator ->
      case SL.fromBaseNameList locator of
        Nothing ->
          raiseError m $ "Could not parse the locator: " <> locatorText
        Just sourceLocator -> do
          let moduleAlias = ModuleAlias aliasText
          sgl <- Alias.resolveLocatorAlias (aliasHandle h) m moduleAlias sourceLocator
          when mustUpdateTag $ do
            liftIO $ Unused.insertGlobalLocator (unusedHandle h) (SGL.reify sgl) m locatorText
            forM_ localLocatorList $ \(ml, ll) ->
              liftIO $ Unused.insertLocalLocator (unusedHandle h) ll ml
          source <- getSource h mustUpdateTag m sgl locatorText
          return [ImportItem source [AI.Use mustUpdateTag sgl localLocatorList]]

getSource :: Handle -> AI.MustUpdateTag -> Hint -> SGL.StrictGlobalLocator -> LocatorText -> EIO Source.Source
getSource h mustUpdateTag m sgl locatorText = do
  let h' = GetModule.Handle {gensymHandle = gensymHandle h, moduleHandle = moduleHandle h}
  let mainModule = Env.getMainModule (envHandle h)
  nextModule <- GetModule.getModule h' mainModule m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  let nextPath = getSourceDir nextModule </> relPath
  when mustUpdateTag $
    liftIO $
      Tag.insertFileLoc (tagHandle h) m (T.length locatorText) (newSourceHint nextPath)
  STL.shiftToLatest
    (shiftToLatestHandle h)
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = nextPath,
        Source.sourceHint = Just m
      }

interpretPreset :: Handle -> Hint -> Module -> EIO [ImportItem]
interpretPreset h m currentModule = do
  presetInfo <- GetEnabledPreset.getEnabledPreset (getEnabledPresetHandle h) currentModule
  fmap concat $ forM presetInfo $ \(locatorText, presetLocalLocatorList) -> do
    let presetLocalLocatorList' = map ((m,) . LL.new) presetLocalLocatorList
    interpretImportItem h False m locatorText presetLocalLocatorList'
