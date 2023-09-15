module Scene.Parse (parse) where

import Context.App
import Context.Cache qualified as Cache
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.NameDependence qualified as NameDependence
import Context.Remark qualified as Remark
import Context.Throw (liftEither)
import Context.Throw qualified as Throw
import Context.UnusedVariable qualified as UnusedVariable
import Context.Via qualified as Via
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.ArgNum qualified as AN
import Entity.Atom qualified as AT
import Entity.Cache qualified as Cache
import Entity.Const (macroMaxStep)
import Entity.Decl qualified as DE
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident.Reify
import Entity.Macro (MacroInfo)
import Entity.Macro.Reduce qualified as Macro
import Entity.Remark qualified as Remark
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.Tree
import Entity.ViaMap qualified as VM
import Path
import Scene.Parse.Alias
import Scene.Parse.Data (interpretDataTree)
import Scene.Parse.Declare
import Scene.Parse.Define
import Scene.Parse.Discern qualified as Discern
import Scene.Parse.Import
import Scene.Parse.Macro (interpretDefineMacro)
import Scene.Parse.Resource (interpretResourceTree)
import Scene.Parse.Tree (parseFile)

parse :: App (Either Cache.Cache ([WeakStmt], [MacroInfo], [DE.Decl]))
parse = do
  source <- Env.getCurrentSource
  result <- parseSource source
  mMainDD <- Locator.getMainDefiniteDescription source
  case mMainDD of
    Just mainDD -> do
      let m = Entity.Hint.new 1 1 $ toFilePath $ Source.sourceFilePath source
      ensureMain m mainDD
      return result
    Nothing ->
      return result

parseSource :: Source.Source -> App (Either Cache.Cache ([WeakStmt], [MacroInfo], [DE.Decl]))
parseSource source = do
  mCache <- Cache.loadCache source
  let path = Source.sourceFilePath source
  case mCache of
    Just cache -> do
      let stmtList = Cache.stmtList cache
      parseCachedStmtList stmtList
      saveTopLevelNames path $ map getStmtName stmtList
      NameDependence.add path $ Map.fromList $ Cache.nameDependence cache
      Via.union path $ VM.decode $ Cache.viaInfo cache
      forM_ (Cache.macroInfoList cache) $ uncurry Env.insertToMacroEnv
      return $ Left cache
    Nothing -> do
      (_, treeList) <- parseFile $ Source.sourceFilePath source
      (defList, macroInfoList, declList) <- interp0 source treeList
      registerTopLevelNames defList
      stmtList <- Discern.discernStmtList defList
      saveTopLevelNames path $ map getWeakStmtName stmtList
      UnusedVariable.registerRemarks
      return $ Right (stmtList, macroInfoList, declList)

saveTopLevelNames :: Path Abs File -> [(Hint, DD.DefiniteDescription)] -> App ()
saveTopLevelNames path topNameList = do
  globalNameList <- mapM (uncurry Global.lookup') topNameList
  let nameMap = Map.fromList $ zip (map snd topNameList) globalNameList
  Global.saveCurrentNameSet path nameMap

parseCachedStmtList :: [Stmt] -> App ()
parseCachedStmtList stmtList = do
  forM_ stmtList $ \stmt -> do
    case stmt of
      StmtDefine isConstLike stmtKind m name impArgNum args _ _ -> do
        let explicitArgs = drop (AN.reify impArgNum) args
        let argNames = map (\(_, x, _) -> toText x) explicitArgs
        Global.registerStmtDefine isConstLike m stmtKind name impArgNum argNames
      StmtDefineResource m name _ _ ->
        Global.registerStmtDefineResource m name

ensureMain :: Hint -> DD.DefiniteDescription -> App ()
ensureMain m mainFunctionName = do
  mMain <- Global.lookup m mainFunctionName
  case mMain of
    Just (_, GN.TopLevelFunc _ _) ->
      return ()
    _ ->
      Throw.raiseError m "`main` is missing"

interp0 :: Source.Source -> [Tree] -> App ([RawStmt], [MacroInfo], [DE.Decl])
interp0 src treeList = do
  case treeList of
    [] ->
      return ([], [], [])
    t : rest
      | headSymEq "import" t -> do
          procImportStmt src t
          interp1 rest
      | otherwise ->
          interp1 treeList

interp1 :: [Tree] -> App ([RawStmt], [MacroInfo], [DE.Decl])
interp1 treeList = do
  case treeList of
    [] ->
      return ([], [], [])
    t : rest
      | headSymEq "declare" t -> do
          declList <- interpretDeclareTree t
          (defList, macroInfoList) <- interp2 [] rest
          return (defList, macroInfoList, declList)
      | otherwise -> do
          (defList, macroInfoList) <- interp2 [] treeList
          return (defList, macroInfoList, [])

interp2 :: [MacroInfo] -> [Tree] -> App ([RawStmt], [MacroInfo])
interp2 macroInfoList treeList = do
  case treeList of
    [] ->
      return ([], macroInfoList)
    t : rest
      | headSymEq "rule" t -> do
          macroInfo <- interpretDefineMacro t
          interp2 (macroInfo : macroInfoList) rest
      | otherwise -> do
          forM_ macroInfoList $ uncurry Env.insertToMacroEnv
          stmtList <- concat <$> mapM interpTree treeList
          return (stmtList, macroInfoList)

interpTree :: Tree -> App [RawStmt]
interpTree t = do
  rules <- Env.getMacroEnv
  t' <- liftEither $ Macro.reduce macroMaxStep rules t
  Remark.printNote' "expanded stmt:"
  Remark.printNote' $ showTree t'
  (m, ts) <- liftEither $ toNode t'
  case ts of
    [] ->
      return []
    hd : rest ->
      case hd of
        _ :< Atom (AT.Symbol sym)
          | sym == "#define" -> do
              return <$> interpretDefineTree m rest
          | sym == "data" || sym == "enum" -> do
              interpretDataTree t'
          | sym == "resource" -> do
              return <$> interpretResourceTree m rest
          | sym == "alias" -> do
              return <$> interpretAliasTree m rest
          | sym == "expand" -> do
              forM_ rest $ \tree -> do
                let reduceRemark =
                      Remark.newRemarkWithoutPadding m Remark.Note $
                        "expanded tree:\n" <> showTree tree
                Remark.insertRemark reduceRemark
              return []
        _ ->
          Throw.raiseError m $ "no such statement is defined: " <> showTree hd

registerTopLevelNames :: [RawStmt] -> App ()
registerTopLevelNames stmtList =
  case stmtList of
    [] ->
      return ()
    RawStmtDefine isConstLike stmtKind m functionName impArgNum xts _ _ : rest -> do
      let explicitArgs = drop (AN.reify impArgNum) xts
      let argNames = map (\(_, x, _) -> x) explicitArgs
      Global.registerStmtDefine isConstLike m stmtKind functionName impArgNum argNames
      registerTopLevelNames rest
    RawStmtDefineResource m name _ _ : rest -> do
      Global.registerStmtDefineResource m name
      registerTopLevelNames rest
    RawStmtVia {} : rest ->
      registerTopLevelNames rest

getWeakStmtName :: WeakStmt -> (Hint, DD.DefiniteDescription)
getWeakStmtName stmt =
  case stmt of
    WeakStmtDefine _ _ m name _ _ _ _ ->
      (m, name)
    WeakStmtDefineResource m name _ _ ->
      (m, name)

getStmtName :: Stmt -> (Hint, DD.DefiniteDescription)
getStmtName stmt =
  case stmt of
    StmtDefine _ _ m name _ _ _ _ ->
      (m, name)
    StmtDefineResource m name _ _ ->
      (m, name)
