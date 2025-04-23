module Move.Scene.Parse
  ( Handle,
    new,
    parse,
    parseCachedStmtList,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Cache qualified as Cache
import Move.Context.EIO (EIO)
import Move.Context.Global qualified as Global
import Move.Context.Path qualified as Path
import Move.Scene.Parse.Core qualified as P
import Move.Scene.Parse.Discern qualified as Discern
import Move.Scene.Parse.Discern.Handle qualified as Discern
import Move.Scene.Parse.Import qualified as Import
import Move.Scene.Parse.Program qualified as Parse
import Rule.ArgNum qualified as AN
import Rule.Cache qualified as Cache
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify
import Rule.LocalLocator qualified as LL
import Rule.RawProgram
import Rule.Remark qualified as R
import Rule.Source qualified as Source
import Rule.Stmt
import Rule.Target
import Rule.VarDefKind

data Handle
  = Handle
  { parseHandle :: P.Handle,
    discernHandle :: Discern.Handle,
    pathHandle :: Path.Handle,
    importHandle :: Import.Handle,
    globalHandle :: Global.Handle,
    unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind)),
    unusedGlobalLocatorMapRef :: IORef (Map.HashMap T.Text [(Hint, T.Text)]), -- (SGL ~> [(hint, locatorText)])
    unusedLocalLocatorMapRef :: IORef (Map.HashMap LL.LocalLocator Hint),
    unusedPresetMapRef :: IORef (Map.HashMap T.Text Hint), -- (ModuleID ~> Hint)
    unusedStaticFileMapRef :: IORef (Map.HashMap T.Text Hint),
    usedVariableSetRef :: IORef (S.Set Int),
    remarkListRef :: IORef [R.Remark] -- per file
  }

new :: App Handle
new = do
  parseHandle <- P.new
  discernHandle <- Discern.new
  pathHandle <- Path.new
  importHandle <- Import.new
  globalHandle <- Global.new
  unusedVariableMapRef <- asks App.unusedVariableMap
  unusedGlobalLocatorMapRef <- asks App.unusedGlobalLocatorMap
  unusedLocalLocatorMapRef <- asks App.unusedLocalLocatorMap
  unusedPresetMapRef <- asks App.unusedPresetMap
  unusedStaticFileMapRef <- asks App.unusedStaticFileMap
  usedVariableSetRef <- asks App.usedVariableSet
  remarkListRef <- asks App.remarkList
  return $ Handle {..}

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
      tmap <- liftIO $ readIORef $ Discern.tagMapRef (discernHandle h)
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
  liftIO $ Global.saveCurrentNameSet (globalHandle h) (Source.sourceFilePath source) nameMap

registerUnusedVariableRemarks :: Handle -> IO ()
registerUnusedVariableRemarks h = do
  vars <- readIORef (unusedVariableMapRef h)
  usedVarSet <- readIORef (usedVariableSetRef h)
  let unusedVars = filter (\(_, var, _) -> not (isHole var) && S.notMember (toInt var) usedVarSet) $ IntMap.elems vars
  forM_ unusedVars $ \(mx, x, k) ->
    case k of
      Normal ->
        insertRemark h $ R.newRemark mx R.Warning $ "Defined but not used: `" <> toText x <> "`"
      Borrowed ->
        insertRemark h $ R.newRemark mx R.Warning $ "Borrowed but not used: `" <> toText x <> "`"
      Relayed ->
        insertRemark h $ R.newRemark mx R.Warning $ "Relayed but not used: `" <> toText x <> "`"

registerUnusedGlobalLocatorRemarks :: Handle -> IO ()
registerUnusedGlobalLocatorRemarks h = do
  unusedGlobalLocatorMap <- readIORef (unusedGlobalLocatorMapRef h)
  let unusedGlobalLocators = concatMap snd $ Map.toList unusedGlobalLocatorMap
  forM_ unusedGlobalLocators $ \(m, locatorText) ->
    insertRemark h $ R.newRemark m R.Warning $ "Imported but not used: `" <> locatorText <> "`"

registerUnusedLocalLocatorRemarks :: Handle -> IO ()
registerUnusedLocalLocatorRemarks h = do
  unusedLocalLocatorMap <- readIORef (unusedLocalLocatorMapRef h)
  forM_ (Map.toList unusedLocalLocatorMap) $ \(ll, m) ->
    insertRemark h $ R.newRemark m R.Warning $ "Imported but not used: `" <> LL.reify ll <> "`"

registerUnusedPresetRemarks :: Handle -> IO ()
registerUnusedPresetRemarks h = do
  unusedPresets <- readIORef (unusedPresetMapRef h)
  forM_ (Map.toList unusedPresets) $ \(presetName, m) ->
    insertRemark h $ R.newRemark m R.Warning $ "Imported but not used: `" <> presetName <> "`"

registerUnusedStaticFileRemarks :: Handle -> IO ()
registerUnusedStaticFileRemarks h = do
  unusedStaticFiles <- readIORef (unusedStaticFileMapRef h)
  forM_ (Map.toList unusedStaticFiles) $ \(k, m) ->
    insertRemark h $ R.newRemark m R.Warning $ "Imported but not used: `" <> k <> "`"

insertRemark :: Handle -> R.Remark -> IO ()
insertRemark h r = do
  modifyIORef' (remarkListRef h) $ (:) r
