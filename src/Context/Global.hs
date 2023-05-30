module Context.Global
  ( registerStmtDefine,
    registerStmtDefineResource,
    lookup,
    lookupStrict,
    initialize,
    activateTopLevelNamesInSource,
    clearSourceNameMap,
    getSourceNameMap,
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
import Entity.TopNameMap
import Path
import Prelude hiding (lookup)

type NameMap = Map.HashMap DD.DefiniteDescription (Hint, GN.GlobalName)

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
  [(Hint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant)] ->
  App ()
registerAsEnumIfNecessary dataName dataArgs consInfoList =
  when (hasNoArgs dataArgs consInfoList) $ do
    Enum.insert dataName
    mapM_ (Enum.insert . (\(_, consName, _, _, _) -> consName)) consInfoList

hasNoArgs :: [a] -> [(Hint, DD.DefiniteDescription, b, [a], D.Discriminant)] -> Bool
hasNoArgs dataArgs consInfoList =
  null dataArgs && null (concatMap (\(_, _, _, consArgs, _) -> consArgs) consInfoList)

registerTopLevelFunc :: IsConstLike -> Hint -> DD.DefiniteDescription -> AN.ArgNum -> AN.ArgNum -> App ()
registerTopLevelFunc isConstLike m topLevelName impArgNum allArgNum = do
  let arity = A.fromInt (AN.reify allArgNum)
  registerTopLevelFunc' m topLevelName impArgNum $ GN.TopLevelFunc arity isConstLike

registerTopLevelFunc' :: Hint -> DD.DefiniteDescription -> AN.ArgNum -> GN.GlobalName -> App ()
registerTopLevelFunc' m topLevelName impArgNum gn = do
  ensureFreshness m topLevelName
  insertToNameMap topLevelName Private m gn
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
  let dataArity = A.fromInt $ length dataArgs
  let consNameArrowList = map (toConsNameArrow dataArity) consInfoList
  let dataArgNum = AN.fromInt (length dataArgs)
  insertToNameMap dataName Private m $ GN.Data dataArity consNameArrowList isConstLike
  forM_ consNameArrowList $ \(consDD, consGN) -> do
    ensureFreshness m consDD
    uncurry (insertToNameMap consDD Private) consGN
    Implicit.insert consDD dataArgNum

toConsNameArrow ::
  A.Arity ->
  (Hint, DD.DefiniteDescription, IsConstLike, [a], D.Discriminant) ->
  (DD.DefiniteDescription, (Hint, GN.GlobalName))
toConsNameArrow dataArity (m, consDD, isConstLikeCons, consArgs, discriminant) = do
  let consArity = A.fromInt $ length consArgs
  (consDD, (m, GN.DataIntro dataArity consArity discriminant isConstLikeCons))

registerStmtDefineResource :: Hint -> DD.DefiniteDescription -> App ()
registerStmtDefineResource m resourceName = do
  ensureFreshness m resourceName
  insertToNameMap resourceName Private m GN.Resource

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

lookupStrict :: Hint.Hint -> DD.DefiniteDescription -> App (Hint, GlobalName)
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
  writeRef' localNameMap Map.empty

ensureFreshness :: Hint.Hint -> DD.DefiniteDescription -> App ()
ensureFreshness m name = do
  topNameMap <- getNameMap
  when (Map.member name topNameMap) $
    Throw.raiseError m $
      "`" <> DD.reify name <> "` is already defined"

getNameMap :: App NameMap
getNameMap =
  readRef' nameMap

getLocalNameMap :: App NameMap
getLocalNameMap =
  readRef' localNameMap

insertToNameMap :: DD.DefiniteDescription -> Visibility -> Hint -> GN.GlobalName -> App ()
insertToNameMap dd v m gn = do
  when (v == Private) $ do
    modifyRef' localNameMap $ Map.insert dd (m, gn)
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

activateTopLevelNamesInSource :: Hint.Hint -> Source.Source -> App ()
activateTopLevelNamesInSource m source = do
  namesInSource <- lookupSourceNameMap m $ Source.sourceFilePath source
  forM_ (Map.toList namesInSource) $ \(dd, (visibility, (mDef, gn))) ->
    case visibility of
      Public ->
        insertToNameMap dd Public mDef gn
      Private ->
        return ()

saveCurrentNameSet :: Path Abs File -> [NA.NameArrow] -> App ()
saveCurrentNameSet currentPath nameArrowList = do
  nmap <- getLocalNameMap
  let publicTopNameMap = Map.fromList $ map (\(dd, mgn) -> (dd, (Public, mgn))) nameArrowList
  let privateTopNameMap = Map.map (Private,) nmap
  let topNameMap = Map.union publicTopNameMap privateTopNameMap
  modifyRef' sourceNameMap $ Map.insert currentPath topNameMap
