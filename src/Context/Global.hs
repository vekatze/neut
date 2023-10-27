module Context.Global
  ( registerStmtDefine,
    registerStmtDefineResource,
    lookup,
    initialize,
    activateTopLevelNames,
    clearSourceNameMap,
    getSourceNameMap,
    saveCurrentNameSet,
    lookupSourceNameMap,
    lookup',
  )
where

import Context.App
import Context.App.Internal
import Context.Env qualified as Env
import Context.Implicit qualified as Implicit
import Context.KeyArg qualified as KeyArg
import Context.OptimizableData qualified as OptimizableData
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalName
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Hint qualified as Hint
import Entity.IsConstLike
import Entity.Key
import Entity.OptimizableData qualified as OD
import Entity.PrimOp.FromText qualified as PrimOp
import Entity.PrimType.FromText qualified as PT
import Entity.StmtKind qualified as SK
import Entity.TopNameMap
import Path
import Prelude hiding (lookup)

registerStmtDefine ::
  IsConstLike ->
  Hint ->
  SK.BaseStmtKind x t ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  [Key] ->
  App ()
registerStmtDefine isConstLike m stmtKind name impArgNum expArgNames = do
  let allArgNum = AN.fromInt (AN.reify impArgNum + length expArgNames)
  let argNum = AN.fromInt (AN.reify allArgNum)
  KeyArg.insert name isConstLike argNum expArgNames
  case stmtKind of
    SK.Normal _ ->
      registerTopLevelFunc isConstLike m name impArgNum allArgNum
    SK.Data dataName dataArgs consInfoList -> do
      registerData isConstLike m dataName dataArgs consInfoList
      registerAsEnumIfNecessary dataName dataArgs consInfoList
      registerAsUnaryIfNecessary dataName consInfoList
    SK.DataIntro {} ->
      return ()

registerAsEnumIfNecessary ::
  DD.DefiniteDescription ->
  [a] ->
  [(Hint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  App ()
registerAsEnumIfNecessary dataName dataArgs consInfoList =
  when (hasNoArgs dataArgs consInfoList) $ do
    OptimizableData.insert dataName OD.Enum
    mapM_ (flip OptimizableData.insert OD.Enum . (\(_, consName, _, _, _) -> consName)) consInfoList

hasNoArgs :: [a] -> [(Hint, DD.DefiniteDescription, b, [a], D.Discriminant)] -> Bool
hasNoArgs dataArgs consInfoList =
  null dataArgs && all (null . (\(_, _, _, consArgs, _) -> consArgs)) consInfoList

registerAsUnaryIfNecessary ::
  DD.DefiniteDescription ->
  [(Hint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  App ()
registerAsUnaryIfNecessary dataName consInfoList =
  when (isUnary consInfoList) $ do
    OptimizableData.insert dataName OD.Unary
    mapM_ (flip OptimizableData.insert OD.Unary . (\(_, consName, _, _, _) -> consName)) consInfoList

isUnary :: [(Hint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] -> Bool
isUnary consInfoList =
  case consInfoList of
    [(_, _, _, [_], _)] ->
      True
    _ ->
      False

registerTopLevelFunc :: IsConstLike -> Hint -> DD.DefiniteDescription -> AN.ArgNum -> AN.ArgNum -> App ()
registerTopLevelFunc isConstLike m topLevelName impArgNum allArgNum = do
  registerTopLevelFunc' m topLevelName impArgNum $ GN.TopLevelFunc allArgNum isConstLike

registerTopLevelFunc' :: Hint -> DD.DefiniteDescription -> AN.ArgNum -> GN.GlobalName -> App ()
registerTopLevelFunc' m topLevelName impArgNum gn = do
  ensureFreshness m topLevelName
  insertToNameMap topLevelName m gn
  Implicit.insert topLevelName impArgNum

registerData ::
  IsConstLike ->
  Hint ->
  DD.DefiniteDescription ->
  [a] ->
  [(Hint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  App ()
registerData isConstLike m dataName dataArgs consInfoList = do
  ensureFreshness m dataName
  let dataArgNum = AN.fromInt $ length dataArgs
  let consNameArrowList = map (toConsNameArrow dataArgNum) consInfoList
  insertToNameMap dataName m $ GN.Data dataArgNum consNameArrowList isConstLike
  forM_ consNameArrowList $ \(consDD, consGN) -> do
    ensureFreshness m consDD
    uncurry (insertToNameMap consDD) consGN
    Implicit.insert consDD dataArgNum

toConsNameArrow ::
  AN.ArgNum ->
  (Hint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant) ->
  (DD.DefiniteDescription, (Hint, GN.GlobalName))
toConsNameArrow dataArgNum (m, consDD, isConstLikeCons, consArgs, discriminant) = do
  let consArgNum = AN.fromInt $ length consArgs
  (consDD, (m, GN.DataIntro dataArgNum consArgNum discriminant isConstLikeCons))

registerStmtDefineResource :: Hint -> DD.DefiniteDescription -> App ()
registerStmtDefineResource m resourceName = do
  ensureFreshness m resourceName
  insertToNameMap resourceName m GN.Resource

lookup :: Hint.Hint -> DD.DefiniteDescription -> App (Maybe (Hint, GlobalName))
lookup m name = do
  nameMap <- readRef' nameMap
  dataSize <- Env.getDataSize m
  case Map.lookup name nameMap of
    Just kind ->
      return $ Just kind
    Nothing
      | Just primType <- PT.fromDefiniteDescription dataSize name ->
          return $ Just (m, GN.PrimType primType)
      | Just primOp <- PrimOp.fromDefiniteDescription dataSize name ->
          return $ Just (m, GN.PrimOp primOp)
      | otherwise -> do
          return Nothing

lookup' :: Hint.Hint -> DD.DefiniteDescription -> App (Hint, GlobalName)
lookup' m name = do
  mgn <- lookup m name
  case mgn of
    Just gn ->
      return gn
    Nothing ->
      Throw.raiseError m $ "no such top-level name is defined: " <> DD.reify name

initialize :: App ()
initialize = do
  writeRef' nameMap Map.empty

ensureFreshness :: Hint.Hint -> DD.DefiniteDescription -> App ()
ensureFreshness m name = do
  topNameMap <- readRef' nameMap
  when (Map.member name topNameMap) $
    Throw.raiseError m $
      "`" <> DD.reify name <> "` is already defined"

insertToNameMap :: DD.DefiniteDescription -> Hint -> GN.GlobalName -> App ()
insertToNameMap dd m gn = do
  modifyRef' nameMap $ Map.insert dd (m, gn)

clearSourceNameMap :: App ()
clearSourceNameMap =
  writeRef' sourceNameMap Map.empty

getSourceNameMap :: App (Map.HashMap (Path Abs File) TopNameMap)
getSourceNameMap =
  readRef' sourceNameMap

lookupSourceNameMap :: Hint.Hint -> Path Abs File -> App TopNameMap
lookupSourceNameMap m sourcePath = do
  smap <- readRef' sourceNameMap
  case Map.lookup sourcePath smap of
    Just topLevelNameInfo -> do
      return topLevelNameInfo
    Nothing ->
      Throw.raiseCritical m $ "top-level names for " <> T.pack (toFilePath sourcePath) <> " is not registered"

activateTopLevelNames :: TopNameMap -> App ()
activateTopLevelNames namesInSource = do
  forM_ (Map.toList namesInSource) $ \(dd, (mDef, gn)) ->
    insertToNameMap dd mDef gn

saveCurrentNameSet :: Path Abs File -> TopNameMap -> App ()
saveCurrentNameSet currentPath nameMap = do
  modifyRef' sourceNameMap $ Map.insert currentPath nameMap
