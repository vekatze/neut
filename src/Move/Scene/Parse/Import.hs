module Move.Scene.Parse.Import
  ( Handle,
    new,
    activateImport,
    interpretImport,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.Alias qualified as Alias
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, toApp)
import Move.Context.Env (getMainModule)
import Move.Context.Global qualified as Global
import Move.Context.Locator qualified as Locator
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.Tag qualified as Tag
import Move.Context.Throw qualified as Throw
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Move.Scene.Module.GetEnabledPreset qualified as GetEnabledPreset
import Move.Scene.Module.GetModule qualified as Module
import Move.Scene.Source.ShiftToLatest qualified as STL
import Path
import Rule.AliasInfo qualified as AI
import Rule.BaseName qualified as BN
import Rule.C
import Rule.Const
import Rule.GlobalLocatorAlias qualified as GLA
import Rule.Hint
import Rule.Import (ImportItem (..))
import Rule.LocalLocator qualified as LL
import Rule.LocationTree qualified as LT
import Rule.Module
import Rule.ModuleAlias (ModuleAlias (ModuleAlias))
import Rule.RawProgram
import Rule.Source qualified as Source
import Rule.SourceLocator qualified as SL
import Rule.StrictGlobalLocator qualified as SGL
import Rule.Syntax.Series qualified as SE

type LocatorText =
  T.Text

data Handle
  = Handle
  { counter :: IORef Int,
    mainModule :: MainModule,
    moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module),
    getEnabledPresetHandle :: GetEnabledPreset.Handle,
    shiftToLatestHandle :: STL.Handle,
    tagMapRef :: IORef LT.LocationTree
  }

new :: App Handle
new = do
  counter <- asks App.counter
  mainModule <- getMainModule
  moduleCacheMapRef <- asks App.moduleCacheMap
  getEnabledPresetHandle <- GetEnabledPreset.new
  shiftToLatestHandle <- STL.new
  tagMapRef <- asks App.tagMap
  return $ Handle {..}

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

interpretImport :: Handle -> Hint -> Source.Source -> [(RawImport, C)] -> App [ImportItem]
interpretImport h m currentSource importList = do
  presetImportList <- interpretPreset h m (Source.sourceModule currentSource)
  let (importList'@((RawImport _ _ importItemList _)), _) = mergeImportList m importList
  if SE.isEmpty importItemList
    then return presetImportList
    else do
      RawImportSummary.set importList'
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
  App [ImportItem]
interpretImportItemStatic h currentModule keyList = do
  currentModule' <- toApp $ STL.shiftToLatestModule (shiftToLatestHandle h) currentModule
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
  Handle ->
  AI.MustUpdateTag ->
  Module ->
  Hint ->
  LocatorText ->
  [(Hint, LL.LocalLocator)] ->
  App [ImportItem]
interpretImportItem h mustUpdateTag currentModule m locatorText localLocatorList = do
  baseNameList <- Throw.liftEither $ BN.bySplit m locatorText
  case baseNameList of
    [] ->
      Throw.raiseCritical m "Scene.Parse.Import: empty parse locator"
    [baseName]
      | Just (moduleAlias, sourceLocator) <- Map.lookup baseName (modulePrefixMap currentModule) -> do
          sgl <- Alias.resolveLocatorAlias m moduleAlias sourceLocator
          source <- toApp $ getSource h mustUpdateTag m sgl locatorText
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
          source <- toApp $ getSource h mustUpdateTag m sgl locatorText
          return [ImportItem source [AI.Use mustUpdateTag sgl localLocatorList]]

getSource :: Handle -> AI.MustUpdateTag -> Hint -> SGL.StrictGlobalLocator -> LocatorText -> EIO Source.Source
getSource h mustUpdateTag m sgl locatorText = do
  let h' = Module.Handle {counter = counter h, mcm = moduleCacheMapRef h}
  nextModule <- Module.getModule h' (mainModule h) m (SGL.moduleID sgl) locatorText
  relPath <- addExtension sourceFileExtension $ SL.reify $ SGL.sourceLocator sgl
  let nextPath = getSourceDir nextModule </> relPath
  when mustUpdateTag $
    liftIO $
      Tag.insertFileLocIO (tagMapRef h) m (T.length locatorText) (newSourceHint nextPath)
  STL.shiftToLatest
    (shiftToLatestHandle h)
    Source.Source
      { Source.sourceModule = nextModule,
        Source.sourceFilePath = nextPath,
        Source.sourceHint = Just m
      }

interpretPreset :: Handle -> Hint -> Module -> App [ImportItem]
interpretPreset h m currentModule = do
  presetInfo <- toApp $ GetEnabledPreset.getEnabledPreset (getEnabledPresetHandle h) currentModule
  fmap concat $ forM presetInfo $ \(locatorText, presetLocalLocatorList) -> do
    let presetLocalLocatorList' = map ((m,) . LL.new) presetLocalLocatorList
    interpretImportItem h False currentModule m locatorText presetLocalLocatorList'
