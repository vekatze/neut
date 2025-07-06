module Kernel.Parse.Interpret
  ( Handle,
    new,
    interpret,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Error.EIO (EIO)
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.RawImportSummary qualified as RawImportSummary
import Kernel.Common.Handle.Local.SymLoc qualified as SymLoc
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Handle.Local.TopCandidate qualified as TopCandidate
import Kernel.Common.Import
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.Source qualified as Source
import Kernel.Common.Target
import Kernel.Parse.Internal.Discern qualified as Discern
import Kernel.Parse.Internal.Discern.Handle qualified as Discern
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.GlobalNameMap qualified as GlobalNameMap
import Kernel.Parse.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.Internal.Import qualified as Import
import Kernel.Parse.VarDefKind
import Language.Common.Ident.Reify
import Language.Common.LocalLocator qualified as LL
import Language.RawTerm.RawStmt
import Language.WeakTerm.WeakStmt
import Logger.Hint
import Logger.Log qualified as L
import Logger.LogLevel qualified as L

data Handle = Handle
  { aliasHandle :: Alias.Handle,
    locatorHandle :: Locator.Handle,
    discernHandle :: Discern.Handle,
    pathHandle :: Path.Handle,
    importHandle :: Import.Handle,
    nameMapHandle :: NameMap.Handle,
    globalNameMapHandle :: GlobalNameMap.Handle,
    unusedHandle :: Unused.Handle,
    symLocHandle :: SymLoc.Handle,
    topCandidateHandle :: TopCandidate.Handle,
    rawImportSummaryHandle :: RawImportSummary.Handle
  }

new ::
  Global.Handle ->
  Local.Handle ->
  IO Handle
new globalHandle localHandle = do
  let unusedHandle = Local.unusedHandle localHandle
  let pathHandle = Global.pathHandle globalHandle
  let importHandle = Import.new globalHandle localHandle
  let globalNameMapHandle = Global.globalNameMapHandle globalHandle
  let aliasHandle = Local.aliasHandle localHandle
  let locatorHandle = Local.locatorHandle localHandle
  let tagHandle = Local.tagHandle localHandle
  nameMapHandle <- NameMap.new globalHandle unusedHandle tagHandle
  let symLocHandle = Local.symLocHandle localHandle
  let topCandidateHandle = Local.topCandidateHandle localHandle
  let rawImportSummaryHandle = Local.rawImportSummaryHandle localHandle
  let discernHandle = Discern.new globalHandle localHandle nameMapHandle
  return $ Handle {..}

interpret ::
  Handle ->
  Target ->
  Source.Source ->
  Either Cache.Cache PostRawProgram ->
  EIO (Either Cache.Cache [WeakStmt], [L.Log])
interpret h t source cacheOrContent = do
  case cacheOrContent of
    Left cache -> do
      return (Left cache, Cache.remarkList cache)
    Right prog -> do
      (prog', logs) <- interpret' h source prog
      tmap <- liftIO $ Tag.get (Discern.tagHandle (discernHandle h))
      Cache.saveLocationCache (pathHandle h) t source $ Cache.LocationCache tmap
      localVarTree <- liftIO $ SymLoc.get (symLocHandle h)
      topCandidate <- liftIO $ TopCandidate.get (topCandidateHandle h)
      rawImportSummary <- liftIO $ RawImportSummary.get (rawImportSummaryHandle h)
      Cache.saveCompletionCache (pathHandle h) t source $
        Cache.CompletionCache
          { Cache.localVarTree = localVarTree,
            Cache.topCandidate = topCandidate,
            Cache.rawImportSummary = rawImportSummary
          }
      return (Right prog', logs)

interpret' :: Handle -> Source.Source -> PostRawProgram -> EIO ([WeakStmt], [L.Log])
interpret' h currentSource (PostRawProgram m importList stmtList) = do
  Import.interpretImport (importHandle h) m currentSource importList >>= activateImport h m
  stmtList'' <- Discern.discernStmtList (discernHandle h) stmtList
  NameMap.reportMissingDefinitions (Discern.nameMapHandle (discernHandle h))
  logs1 <- liftIO $ registerUnusedVariableRemarks h
  logs2 <- liftIO $ registerUnusedGlobalLocatorRemarks h
  logs3 <- liftIO $ registerUnusedLocalLocatorRemarks h
  logs4 <- liftIO $ registerUnusedStaticFileRemarks h
  return (stmtList'', logs1 ++ logs2 ++ logs3 ++ logs4)

activateImport :: Handle -> Hint -> [ImportItem] -> EIO ()
activateImport h m sourceInfoList = do
  forM_ sourceInfoList $ \importItem -> do
    case importItem of
      ImportItem source aliasInfoList -> do
        let path = Source.sourceFilePath source
        namesInSource <- GlobalNameMap.lookup (globalNameMapHandle h) m path
        liftIO $ NameMap.activateTopLevelNames (nameMapHandle h) namesInSource
        forM_ aliasInfoList $ \aliasInfo ->
          Alias.activateAliasInfo (aliasHandle h) source namesInSource aliasInfo
      StaticKey pathList -> do
        forM_ pathList $ \(key, (mKey, path)) -> do
          Locator.activateStaticFile (locatorHandle h) mKey key path

registerUnusedVariableRemarks :: Handle -> IO [L.Log]
registerUnusedVariableRemarks h = do
  unusedVars <- Unused.getVariable (unusedHandle h)
  return $ flip map unusedVars $ \(mx, x, k) ->
    case k of
      Normal ->
        L.newLog mx L.Warning $
          "Defined but not used: `" <> toText x <> "`"
      Borrowed ->
        L.newLog mx L.Warning $
          "Borrowed but not used: `" <> toText x <> "`"
      Relayed ->
        L.newLog mx L.Warning $
          "Relayed but not used: `" <> toText x <> "`"

registerUnusedGlobalLocatorRemarks :: Handle -> IO [L.Log]
registerUnusedGlobalLocatorRemarks h = do
  unusedGlobalLocatorMap <- Unused.getGlobalLocator (unusedHandle h)
  let unusedGlobalLocators = concatMap snd unusedGlobalLocatorMap
  return $ flip map unusedGlobalLocators $ \(m, locatorText) ->
    L.newLog m L.Warning $
      "Imported but not used: `" <> locatorText <> "`"

registerUnusedLocalLocatorRemarks :: Handle -> IO [L.Log]
registerUnusedLocalLocatorRemarks h = do
  unusedLocalLocatorMap <- Unused.getLocalLocator (unusedHandle h)
  return $ flip map unusedLocalLocatorMap $ \(ll, m) ->
    L.newLog m L.Warning $
      "Imported but not used: `" <> LL.reify ll <> "`"

registerUnusedStaticFileRemarks :: Handle -> IO [L.Log]
registerUnusedStaticFileRemarks h = do
  unusedStaticFiles <- Unused.getStaticFile (unusedHandle h)
  return $ flip map unusedStaticFiles $ \(k, m) ->
    L.newLog m L.Warning $
      "Imported but not used: `" <> k <> "`"
