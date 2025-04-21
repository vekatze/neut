module Move.Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal (counter)
import Move.Context.Cache qualified as Cache
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Global qualified as Global
import Move.Context.Path qualified as Path
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Context.UnusedPreset qualified as UnusedPreset
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Move.Context.UnusedVariable qualified as UnusedVariable
import Move.Scene.Parse.Core (Handle (Handle))
import Move.Scene.Parse.Core qualified as P
import Move.Scene.Parse.Discern qualified as Discern
import Move.Scene.Parse.Import
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

parse :: Target -> Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache [WeakStmt])
parse t source cacheOrContent = do
  parseSource t source cacheOrContent

parseSource :: Target -> Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache [WeakStmt])
parseSource t source cacheOrContent = do
  let filePath = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      saveTopLevelNames source $ getStmtName stmtList
      return $ Left cache
    Right fileContent -> do
      counter <- asks counter
      let h = Handle {counter, filePath, fileContent, mustParseWholeFile = True}
      prog <- toApp $ P.parseFile h Parse.parseProgram
      prog' <- interpret source (snd prog)
      tmap <- Env.getTagMap
      h' <- Path.new
      toApp $ Cache.saveLocationCache h' t source $ Cache.LocationCache tmap
      return $ Right prog'

parseCachedStmtList :: [Stmt] -> App ()
parseCachedStmtList stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs _ _ -> do
        let expArgNames = map (\(_, x, _) -> toText x) expArgs
        let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
        Global.registerStmtDefine isConstLike m stmtKind name allArgNum expArgNames
      StmtForeign {} ->
        return ()

interpret :: Source.Source -> RawProgram -> App [WeakStmt]
interpret currentSource (RawProgram m importList stmtList) = do
  interpretImport m currentSource importList >>= activateImport m
  stmtList' <- Discern.discernStmtList (Source.sourceModule currentSource) $ map fst stmtList
  Global.reportMissingDefinitions
  saveTopLevelNames currentSource $ getWeakStmtName stmtList'
  UnusedVariable.registerRemarks
  UnusedGlobalLocator.registerRemarks
  UnusedLocalLocator.registerRemarks
  UnusedPreset.registerRemarks
  UnusedStaticFile.registerRemarks
  return stmtList'

saveTopLevelNames :: Source.Source -> [(Hint, DD.DefiniteDescription)] -> App ()
saveTopLevelNames source topNameList = do
  globalNameList <- mapM (uncurry Global.lookup') topNameList
  let nameMap = Map.fromList $ zip (map snd topNameList) globalNameList
  Global.saveCurrentNameSet (Source.sourceFilePath source) nameMap
