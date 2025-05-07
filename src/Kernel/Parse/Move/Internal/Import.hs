module Kernel.Parse.Move.Internal.Import
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
import Error.Rule.EIO (EIO)
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Common.Rule.AliasInfo qualified as AI
import Kernel.Common.Rule.Const
import Kernel.Common.Rule.Import (ImportItem (..))
import Kernel.Common.Rule.Module
import Kernel.Common.Rule.Source qualified as Source
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.Locator qualified as Locator
import Kernel.Move.Context.Module qualified as Module
import Kernel.Move.Context.RawImportSummary qualified as RawImportSummary
import Kernel.Move.Context.Tag qualified as Tag
import Kernel.Move.Scene.Init.Global qualified as Global
import Kernel.Move.Scene.Init.Local qualified as Local
import Kernel.Move.Scene.Module.GetEnabledPreset qualified as GetEnabledPreset
import Kernel.Move.Scene.Module.GetModule qualified as GetModule
import Kernel.Move.Scene.Source.ShiftToLatest qualified as STL
import Kernel.Parse.Move.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Move.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Kernel.Parse.Move.Internal.Handle.Unused qualified as Unused
import Language.Common.Move.Raise (raiseCritical, raiseError)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.GlobalLocatorAlias qualified as GLA
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.ModuleAlias (ModuleAlias (ModuleAlias))
import Language.Common.Rule.SourceLocator qualified as SL
import Language.Common.Rule.StrictGlobalLocator qualified as SGL
import Language.RawTerm.Rule.RawStmt
import Logger.Rule.Hint
import Path
import SyntaxTree.Rule.C
import SyntaxTree.Rule.Series qualified as SE

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
            interpretImportItem h True (Source.sourceModule currentSource) mItem locatorText localLocatorList'
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
  Module ->
  Hint ->
  LocatorText ->
  [(Hint, LL.LocalLocator)] ->
  EIO [ImportItem]
interpretImportItem h mustUpdateTag currentModule m locatorText localLocatorList = do
  baseNameList <- liftEither $ BN.bySplit m locatorText
  case baseNameList of
    [] ->
      raiseCritical m "Scene.Parse.Import: empty parse locator"
    [baseName]
      | Just (moduleAlias, sourceLocator) <- Map.lookup baseName (modulePrefixMap currentModule) -> do
          sgl <- Alias.resolveLocatorAlias (aliasHandle h) m moduleAlias sourceLocator
          source <- getSource h mustUpdateTag m sgl locatorText
          let gla = GLA.GlobalLocatorAlias baseName
          when mustUpdateTag $ do
            liftIO $ Unused.insertGlobalLocator (unusedHandle h) (SGL.reify sgl) m locatorText
            forM_ localLocatorList $ \(ml, ll) ->
              liftIO $ Unused.insertLocalLocator (unusedHandle h) ll ml
          return [ImportItem source [AI.Use mustUpdateTag sgl localLocatorList, AI.Prefix m gla sgl]]
      | otherwise ->
          raiseError m $ "No such prefix is defined: " <> BN.reify baseName
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
    interpretImportItem h False currentModule m locatorText presetLocalLocatorList'
