module Move.Scene.Parse
  ( Handle,
    new,
    parse,
    parseCachedStmtList,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Cache qualified as Cache
import Move.Context.EIO (EIO, toApp)
import Move.Context.Env qualified as Env
import Move.Context.Global qualified as Global
import Move.Context.Path qualified as Path
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Context.UnusedPreset qualified as UnusedPreset
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Move.Context.UnusedVariable qualified as UnusedVariable
import Move.Scene.Parse.Core qualified as P
import Move.Scene.Parse.Discern qualified as Discern
import Move.Scene.Parse.Discern.Handle qualified as Discern
import Move.Scene.Parse.Import qualified as Import
import Move.Scene.Parse.Program qualified as Parse
import Rule.ArgNum qualified as AN
import Rule.Cache qualified as Cache
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident.Reify
import Rule.RawProgram
import Rule.Source qualified as Source
import Rule.Stmt
import Rule.Target

data Handle
  = Handle
  { parseHandle :: P.Handle,
    discernHandle :: Discern.Handle,
    pathHandle :: Path.Handle,
    importHandle :: Import.Handle,
    globalHandle :: Global.Handle
  }

new :: App Handle
new = do
  parseHandle <- P.new
  discernHandle <- Discern.new
  pathHandle <- Path.new
  importHandle <- Import.new
  globalHandle <- Global.new
  return $ Handle {..}

parse :: Handle -> Target -> Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache [WeakStmt])
parse h t source cacheOrContent = do
  parseSource h t source cacheOrContent

parseSource :: Handle -> Target -> Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache [WeakStmt])
parseSource h t source cacheOrContent = do
  let filePath = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      toApp $ saveTopLevelNames h source $ getStmtName stmtList
      return $ Left cache
    Right fileContent -> do
      prog <- toApp $ P.parseFile (parseHandle h) filePath fileContent True Parse.parseProgram
      prog' <- interpret h source (snd prog)
      tmap <- Env.getTagMap
      toApp $ Cache.saveLocationCache (pathHandle h) t source $ Cache.LocationCache tmap
      return $ Right prog'

parseCachedStmtList :: [Stmt] -> App ()
parseCachedStmtList stmtList = do
  h <- Global.new
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs _ _ -> do
        let expArgNames = map (\(_, x, _) -> toText x) expArgs
        let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
        toApp $ Global.registerStmtDefine h isConstLike m stmtKind name allArgNum expArgNames
      StmtForeign {} ->
        return ()

interpret :: Handle -> Source.Source -> RawProgram -> App [WeakStmt]
interpret h currentSource (RawProgram m importList stmtList) = do
  toApp $ do
    Import.interpretImport (importHandle h) m currentSource importList >>= Import.activateImport (importHandle h) m
  stmtList' <- toApp $ Discern.discernStmtList (discernHandle h) (Source.sourceModule currentSource) $ map fst stmtList
  toApp $ Global.reportMissingDefinitions (globalHandle h)
  toApp $ saveTopLevelNames h currentSource $ getWeakStmtName stmtList'
  UnusedVariable.registerRemarks
  UnusedGlobalLocator.registerRemarks
  UnusedLocalLocator.registerRemarks
  UnusedPreset.registerRemarks
  UnusedStaticFile.registerRemarks
  return stmtList'

saveTopLevelNames :: Handle -> Source.Source -> [(Hint, DD.DefiniteDescription)] -> EIO ()
saveTopLevelNames h source topNameList = do
  globalNameList <- mapM (uncurry $ Global.lookup' (globalHandle h)) topNameList
  let nameMap = Map.fromList $ zip (map snd topNameList) globalNameList
  liftIO $ Global.saveCurrentNameSet (globalHandle h) (Source.sourceFilePath source) nameMap
