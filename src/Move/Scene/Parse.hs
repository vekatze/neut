module Move.Scene.Parse
  ( Handle,
    new,
    parse,
    parseCachedStmtList,
    getUnusedLocators,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Logger.Rule.LogLevel qualified as L
import Move.Context.Cache qualified as Cache
import Move.Context.EIO (EIO)
import Move.Context.Path qualified as Path
import Move.Context.Tag qualified as Tag
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Init.Local qualified as Local
import Move.Scene.Parse.Core qualified as P
import Move.Scene.Parse.Discern qualified as Discern
import Move.Scene.Parse.Discern.Handle qualified as Discern
import Move.Scene.Parse.Handle.Global qualified as Global
import Move.Scene.Parse.Handle.NameMap qualified as NameMap
import Move.Scene.Parse.Handle.Unused qualified as Unused
import Move.Scene.Parse.Import qualified as Import
import Move.Scene.Parse.Program qualified as Parse
import Move.UI.Handle.LocalRemark qualified as LocalRemark
import Rule.ArgNum qualified as AN
import Rule.Cache qualified as Cache
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident.Reify
import Rule.LocalLocator qualified as LL
import Rule.RawProgram
import Rule.Source qualified as Source
import Rule.Stmt
import Rule.Target
import Rule.UnusedGlobalLocators (UnusedGlobalLocators)
import Rule.UnusedLocalLocators (UnusedLocalLocators)
import Rule.VarDefKind

data Handle
  = Handle
  { parseHandle :: P.Handle,
    discernHandle :: Discern.Handle,
    pathHandle :: Path.Handle,
    importHandle :: Import.Handle,
    globalHandle :: Global.Handle,
    localRemarkHandle :: LocalRemark.Handle,
    nameMapHandle :: NameMap.Handle,
    unusedHandle :: Unused.Handle
  }

new ::
  Base.Handle ->
  Local.Handle ->
  Handle
new baseHandle localHandle = do
  let localRemarkHandle = Local.localRemarkHandle localHandle
  let unusedHandle = Local.unusedHandle localHandle
  let parseHandle = P.new (Base.gensymHandle baseHandle)
  let discernHandle = Discern.new baseHandle localHandle
  let pathHandle = Base.pathHandle baseHandle
  let importHandle = Import.new baseHandle localHandle
  let globalHandle = Local.globalHandle localHandle
  let nameMapHandle = Base.nameMapHandle baseHandle
  Handle {..}

parse :: Handle -> Target -> Source.Source -> Either Cache.Cache T.Text -> EIO (Either Cache.Cache [WeakStmt])
parse h t source cacheOrContent = do
  parseSource h t source cacheOrContent

parseSource :: Handle -> Target -> Source.Source -> Either Cache.Cache T.Text -> EIO (Either Cache.Cache [WeakStmt])
parseSource h t source cacheOrContent = do
  let filePath = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList h stmtList
      saveTopLevelNames h source $ getStmtName stmtList
      return $ Left cache
    Right fileContent -> do
      prog <- P.parseFile (parseHandle h) filePath fileContent True Parse.parseProgram
      prog' <- interpret h source (snd prog)
      tmap <- liftIO $ Tag.get (Discern.tagHandle (discernHandle h))
      Cache.saveLocationCache (pathHandle h) t source $ Cache.LocationCache tmap
      return $ Right prog'

parseCachedStmtList :: Handle -> [Stmt] -> EIO ()
parseCachedStmtList h stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs _ _ -> do
        let expArgNames = map (\(_, x, _) -> toText x) expArgs
        let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
        Global.registerStmtDefine (globalHandle h) isConstLike m stmtKind name allArgNum expArgNames
      StmtForeign {} ->
        return ()

interpret :: Handle -> Source.Source -> RawProgram -> EIO [WeakStmt]
interpret h currentSource (RawProgram m importList stmtList) = do
  Import.interpretImport (importHandle h) m currentSource importList >>= Import.activateImport (importHandle h) m
  stmtList' <- Discern.discernStmtList (discernHandle h) (Source.sourceModule currentSource) $ map fst stmtList
  Global.reportMissingDefinitions (globalHandle h)
  saveTopLevelNames h currentSource $ getWeakStmtName stmtList'
  liftIO $ registerUnusedVariableRemarks h
  liftIO $ registerUnusedGlobalLocatorRemarks h
  liftIO $ registerUnusedLocalLocatorRemarks h
  liftIO $ registerUnusedPresetRemarks h
  liftIO $ registerUnusedStaticFileRemarks h
  return stmtList'

saveTopLevelNames :: Handle -> Source.Source -> [(Hint, DD.DefiniteDescription)] -> EIO ()
saveTopLevelNames h source topNameList = do
  globalNameList <- mapM (uncurry $ Global.lookup' (globalHandle h)) topNameList
  let nameMap = Map.fromList $ zip (map snd topNameList) globalNameList
  liftIO $ NameMap.saveCurrentNameSet (nameMapHandle h) (Source.sourceFilePath source) nameMap

getUnusedLocators :: Handle -> IO (UnusedGlobalLocators, UnusedLocalLocators)
getUnusedLocators h = do
  unusedGlobalLocators <- Unused.getGlobalLocator (unusedHandle h)
  unusedLocalLocators <- Unused.getLocalLocator (unusedHandle h)
  return (unusedGlobalLocators, unusedLocalLocators)

registerUnusedVariableRemarks :: Handle -> IO ()
registerUnusedVariableRemarks h = do
  unusedVars <- Unused.getVariable (unusedHandle h)
  forM_ unusedVars $ \(mx, x, k) ->
    case k of
      Normal ->
        LocalRemark.insert (localRemarkHandle h) $
          newLog mx L.Warning $
            "Defined but not used: `" <> toText x <> "`"
      Borrowed ->
        LocalRemark.insert (localRemarkHandle h) $
          newLog mx L.Warning $
            "Borrowed but not used: `" <> toText x <> "`"
      Relayed ->
        LocalRemark.insert (localRemarkHandle h) $
          newLog mx L.Warning $
            "Relayed but not used: `" <> toText x <> "`"

registerUnusedGlobalLocatorRemarks :: Handle -> IO ()
registerUnusedGlobalLocatorRemarks h = do
  unusedGlobalLocatorMap <- Unused.getGlobalLocator (unusedHandle h)
  let unusedGlobalLocators = concatMap snd unusedGlobalLocatorMap
  forM_ unusedGlobalLocators $ \(m, locatorText) ->
    LocalRemark.insert (localRemarkHandle h) $
      newLog m L.Warning $
        "Imported but not used: `" <> locatorText <> "`"

registerUnusedLocalLocatorRemarks :: Handle -> IO ()
registerUnusedLocalLocatorRemarks h = do
  unusedLocalLocatorMap <- Unused.getLocalLocator (unusedHandle h)
  forM_ unusedLocalLocatorMap $ \(ll, m) ->
    LocalRemark.insert (localRemarkHandle h) $
      newLog m L.Warning $
        "Imported but not used: `" <> LL.reify ll <> "`"

registerUnusedPresetRemarks :: Handle -> IO ()
registerUnusedPresetRemarks h = do
  unusedPresets <- Unused.getPreset (unusedHandle h)
  forM_ (Map.toList unusedPresets) $ \(presetName, m) ->
    LocalRemark.insert (localRemarkHandle h) $
      newLog m L.Warning $
        "Imported but not used: `" <> presetName <> "`"

registerUnusedStaticFileRemarks :: Handle -> IO ()
registerUnusedStaticFileRemarks h = do
  unusedStaticFiles <- Unused.getStaticFile (unusedHandle h)
  forM_ unusedStaticFiles $ \(k, m) ->
    LocalRemark.insert (localRemarkHandle h) $
      newLog m L.Warning $
        "Imported but not used: `" <> k <> "`"
