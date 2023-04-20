module Context.Global
  ( registerStmtDefine,
    registerStmtDefineResource,
    registerStmtExport,
    lookup,
    lookupStrict,
    initialize,
    lookupSourceNameMap,
    activateTopLevelNamesInSource,
    saveCurrentNameSet,
  )
where

import Context.App
import Context.App.Internal
import Context.Enum qualified as Enum
import Context.Env qualified as Env
import Context.Implicit qualified as Implicit
import Context.Remark (printNote')
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Arity qualified as A
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalName
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Hint qualified as Hint
import Entity.IsConstLike
import Entity.NameArrow qualified as NA
import Entity.PrimOp.FromText qualified as PrimOp
import Entity.PrimType.FromText qualified as PT
import Entity.Source qualified as Source
import Entity.Stmt as ST
import Path
import Prelude hiding (lookup)

type NameMap = Map.HashMap DD.DefiniteDescription GN.GlobalName

registerStmtDefine ::
  IsConstLike ->
  Hint ->
  ST.StmtKindF a ->
  DD.DefiniteDescription ->
  AN.ArgNum ->
  AN.ArgNum ->
  App ()
registerStmtDefine isConstLike m stmtKind name impArgNum allArgNum = do
  case stmtKind of
    ST.Normal _ ->
      registerTopLevelFunc isConstLike m name impArgNum allArgNum
    ST.Data dataName dataArgs consInfoList projectionList -> do
      registerData isConstLike m dataName dataArgs consInfoList projectionList
      registerAsEnumIfNecessary dataName dataArgs consInfoList
    ST.DataIntro {} ->
      return ()
    ST.Projection ->
      registerProjection isConstLike m name impArgNum allArgNum

registerAsEnumIfNecessary ::
  DD.DefiniteDescription ->
  [BinderF a] ->
  [(DD.DefiniteDescription, IsConstLike, [BinderF a], D.Discriminant)] ->
  App ()
registerAsEnumIfNecessary dataName dataArgs consInfoList =
  when (hasNoArgs dataArgs consInfoList) $ do
    Enum.insert dataName
    mapM_ (Enum.insert . (\(consName, _, _, _) -> consName)) consInfoList

hasNoArgs :: [BinderF a] -> [(DD.DefiniteDescription, b, [BinderF a], D.Discriminant)] -> Bool
hasNoArgs dataArgs consInfoList =
  null dataArgs && null (concatMap (\(_, _, consArgs, _) -> consArgs) consInfoList)

registerTopLevelFunc :: IsConstLike -> Hint -> DD.DefiniteDescription -> AN.ArgNum -> AN.ArgNum -> App ()
registerTopLevelFunc isConstLike m topLevelName impArgNum allArgNum = do
  let arity = A.fromInt (AN.reify allArgNum)
  registerTopLevelFunc' m topLevelName impArgNum $ GN.TopLevelFunc arity isConstLike

registerProjection :: IsConstLike -> Hint -> DD.DefiniteDescription -> AN.ArgNum -> AN.ArgNum -> App ()
registerProjection isConstLike m topLevelName impArgNum allArgNum = do
  let arity = A.fromInt (AN.reify allArgNum)
  registerTopLevelFunc' m topLevelName impArgNum $ GN.Projection arity isConstLike

registerTopLevelFunc' :: Hint -> DD.DefiniteDescription -> AN.ArgNum -> GN.GlobalName -> App ()
registerTopLevelFunc' m topLevelName impArgNum gn = do
  topNameMap <- readRef' nameMap
  ensureFreshness m topNameMap topLevelName
  modifyRef' nameMap $ Map.insert topLevelName gn
  Implicit.insert topLevelName impArgNum

registerData ::
  IsConstLike ->
  Hint ->
  DD.DefiniteDescription ->
  [BinderF a] ->
  [(DD.DefiniteDescription, IsConstLike, [BinderF a], D.Discriminant)] ->
  [DD.DefiniteDescription] ->
  App ()
registerData isConstLike m dataName dataArgs consInfoList projectionList = do
  topNameMap <- readRef' nameMap
  ensureFreshness m topNameMap dataName
  let consList = map (\(consName, _, _, _) -> consName) consInfoList
  let dataArity = A.fromInt $ length dataArgs
  let dataArgNum = AN.fromInt (length dataArgs)
  modifyRef' nameMap $ Map.insert dataName $ GN.Data dataArity (consList ++ projectionList) isConstLike
  forM_ consInfoList $ \(consName, isConstLikeCons, consArgs, discriminant) -> do
    topNameMap' <- readRef' nameMap
    ensureFreshness m topNameMap' consName
    let consArity = A.fromInt $ length consArgs
    modifyRef' nameMap $ Map.insert consName $ GN.DataIntro dataArity consArity discriminant isConstLikeCons
    Implicit.insert consName dataArgNum

registerStmtDefineResource :: Hint -> DD.DefiniteDescription -> App ()
registerStmtDefineResource m resourceName = do
  topNameMap <- readRef' nameMap
  ensureFreshness m topNameMap resourceName
  modifyRef' nameMap $ Map.insert resourceName GN.Resource

registerStmtExport :: NA.NameArrow -> App ()
registerStmtExport ((m, alias), (_, origGN)) = do
  topNameMap <- readRef' nameMap
  ensureFreshness m topNameMap alias
  modifyRef' nameMap $ Map.insert alias origGN

lookup :: Hint.Hint -> DD.DefiniteDescription -> App (Maybe GlobalName)
lookup m name = do
  nameMap <- readRef' nameMap
  dataSize <- Env.getDataSize m
  case Map.lookup name nameMap of
    Just kind ->
      return $ Just kind
    Nothing
      | Just primType <- PT.fromDefiniteDescription dataSize name ->
          return $ Just $ GN.PrimType primType
      | Just primOp <- PrimOp.fromDefiniteDescription dataSize name ->
          return $ Just $ GN.PrimOp primOp
      | otherwise -> do
          return Nothing

lookupStrict :: Hint.Hint -> DD.DefiniteDescription -> App GlobalName
lookupStrict m name = do
  mGlobalName <- lookup m name
  case mGlobalName of
    Just globalName ->
      return globalName
    Nothing ->
      Throw.raiseCritical m $ "the top-level " <> DD.reify name <> " doesn't have its global name"

initialize :: App ()
initialize = do
  writeRef' nameMap Map.empty

ensureFreshness :: Hint.Hint -> NameMap -> DD.DefiniteDescription -> App ()
ensureFreshness m topNameMap name = do
  when (Map.member name topNameMap) $
    Throw.raiseError m $
      "`" <> DD.reify name <> "` is already defined"

insertToSourceNameMap :: Path Abs File -> [(DD.DefiniteDescription, GN.GlobalName)] -> App ()
insertToSourceNameMap sourcePath topLevelNameInfo = do
  modifyRef' sourceNameMap $ Map.insert sourcePath $ Map.fromList topLevelNameInfo

lookupSourceNameMap :: Hint.Hint -> Path Abs File -> App (Map.HashMap DD.DefiniteDescription GN.GlobalName)
lookupSourceNameMap m sourcePath = do
  smap <- readRef' sourceNameMap
  case Map.lookup sourcePath smap of
    Just topLevelNameInfo ->
      return topLevelNameInfo
    Nothing ->
      Throw.raiseCritical m $ "top-level names for " <> T.pack (toFilePath sourcePath) <> " is not registered"

activateTopLevelNamesInSource :: Hint.Hint -> Source.Source -> App ()
activateTopLevelNamesInSource m source = do
  namesInSource <- lookupSourceNameMap m $ Source.sourceFilePath source
  modifyRef' nameMap $ Map.union namesInSource

saveCurrentNameSet :: Path Abs File -> [NA.NameArrow] -> App ()
saveCurrentNameSet currentPath nameArrowList = do
  insertToSourceNameMap currentPath $ map NA.reify nameArrowList
