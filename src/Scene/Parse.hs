module Scene.Parse
  ( parse,
    parseCachedStmtList,
  )
where

import Context.App
import Context.Global qualified as Global
import Context.Remark (printNote')
import Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Context.UnusedPreset qualified as UnusedPreset
import Context.UnusedStaticFile qualified as UnusedStaticFile
import Context.UnusedVariable qualified as UnusedVariable
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Cache qualified as Cache
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident.Reify
import Entity.Opacity qualified as O
import Entity.RawProgram
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Scene.Parse.Core qualified as P
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Import
import Scene.Parse.Program qualified as Parse

parse :: Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache [WeakStmt])
parse source cacheOrContent = do
  printNote' "parse"
  parseSource source cacheOrContent

parseSource :: Source.Source -> Either Cache.Cache T.Text -> App (Either Cache.Cache [WeakStmt])
parseSource source cacheOrContent = do
  let path = Source.sourceFilePath source
  case cacheOrContent of
    Left cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      saveTopLevelNames source $ getStmtName stmtList
      return $ Left cache
    Right content -> do
      prog <- P.parseFile True Parse.parseProgram path content
      Right <$> interpret source (snd prog)

parseCachedStmtList :: [Stmt] -> App ()
parseCachedStmtList stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs _ _ -> do
        let expArgNames = map (\(_, x, _) -> toText x) expArgs
        let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
        Global.registerStmtDefine isConstLike m stmtKind name allArgNum expArgNames
      StmtDefineConst (SavedHint m) dd _ _ ->
        Global.registerStmtDefine True m (SK.Normal O.Clear) dd AN.zero []
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
