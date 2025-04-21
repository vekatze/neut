module Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Context.App
import Context.Cache qualified as Cache
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Context.UnusedPreset qualified as UnusedPreset
import Context.UnusedStaticFile qualified as UnusedStaticFile
import Context.UnusedVariable qualified as UnusedVariable
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Rule.ArgNum qualified as AN
import Rule.Cache qualified as Cache
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident.Reify
import Rule.RawProgram
import Rule.Source qualified as Source
import Rule.Stmt
import Rule.Target
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Import
import Scene.Parse.Program qualified as Parse

parse :: Target -> Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache [WeakStmt])
parse t source cacheOrContent = do
  parseSource t source cacheOrContent

parseSource :: Target -> Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache [WeakStmt])
parseSource t source cacheOrContent = do
  let path = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      saveTopLevelNames source $ getStmtName stmtList
      return $ Left cache
    Right content -> do
      prog <- P.parseFile True Parse.parseProgram path content
      prog' <- interpret source (snd prog)
      tmap <- Env.getTagMap
      Cache.saveLocationCache t source $ Cache.LocationCache tmap
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
