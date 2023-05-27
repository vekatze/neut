module Context.Global
  ( registerStmtDefine,
    registerStmtDefineResource,
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
import Context.KeyArg qualified as KeyArg
import Context.Throw qualified as Throw
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Arity qualified as A
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalName
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Hint qualified as Hint
import Entity.IsConstLike
import Entity.Key
import Entity.NameArrow qualified as NA
import Entity.PrimOp.FromText qualified as PrimOp
import Entity.PrimType.FromText qualified as PT
import Entity.Source qualified as Source
import Entity.StmtKind qualified as SK
import Path
import Prelude hiding (lookup)

type NameMap = Map.HashMap DD.DefiniteDescription GN.GlobalName

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
  let arity = A.fromInt (AN.reify allArgNum)
  KeyArg.insert name arity expArgNames
  case stmtKind of
    SK.Normal _ ->
      registerTopLevelFunc isConstLike m name impArgNum allArgNum
    SK.Data dataName dataArgs consInfoList -> do
      registerData isConstLike m dataName dataArgs consInfoList
      registerAsEnumIfNecessary dataName dataArgs consInfoList
    SK.DataIntro {} ->
      return ()

registerAsEnumIfNecessary ::
  DD.DefiniteDescription ->
  [a] ->
  [(DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  App ()
registerAsEnumIfNecessary dataName dataArgs consInfoList =
  when (hasNoArgs dataArgs consInfoList) $ do
    Enum.insert dataName
    mapM_ (Enum.insert . (\(consName, _, _, _) -> consName)) consInfoList

hasNoArgs :: [a] -> [(DD.DefiniteDescription, b, [a], D.Discriminant)] -> Bool
hasNoArgs dataArgs consInfoList =
  null dataArgs && null (concatMap (\(_, _, consArgs, _) -> consArgs) consInfoList)

registerTopLevelFunc :: IsConstLike -> Hint -> DD.DefiniteDescription -> AN.ArgNum -> AN.ArgNum -> App ()
registerTopLevelFunc isConstLike m topLevelName impArgNum allArgNum = do
  let arity = A.fromInt (AN.reify allArgNum)
  registerTopLevelFunc' m topLevelName impArgNum $ GN.TopLevelFunc arity isConstLike

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
  [a] ->
  [(DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  App ()
registerData isConstLike m dataName dataArgs consInfoList = do
  topNameMap <- readRef' nameMap
  ensureFreshness m topNameMap dataName
  let dataArity = A.fromInt $ length dataArgs
  let consNameArrowList = map (toConsNameArrow dataArity) consInfoList
  let dataArgNum = AN.fromInt (length dataArgs)
  modifyRef' nameMap $ Map.insert dataName $ GN.Data dataArity consNameArrowList isConstLike
  forM_ consNameArrowList $ \(consDD, consGN) -> do
    topNameMap' <- readRef' nameMap
    ensureFreshness m topNameMap' consDD
    modifyRef' nameMap $ Map.insert consDD consGN
    Implicit.insert consDD dataArgNum

toConsNameArrow ::
  A.Arity ->
  (DD.DefiniteDescription, IsConstLike, [a], D.Discriminant) ->
  (DD.DefiniteDescription, GN.GlobalName)
toConsNameArrow dataArity (consDD, isConstLikeCons, consArgs, discriminant) = do
  let consArity = A.fromInt $ length consArgs
  (consDD, GN.DataIntro dataArity consArity discriminant isConstLikeCons)

registerStmtDefineResource :: Hint -> DD.DefiniteDescription -> App ()
registerStmtDefineResource m resourceName = do
  topNameMap <- readRef' nameMap
  ensureFreshness m topNameMap resourceName
  modifyRef' nameMap $ Map.insert resourceName GN.Resource

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

insertToSourceNameMap :: Path Abs File -> [NA.NameArrow] -> App ()
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
  insertToSourceNameMap currentPath nameArrowList
