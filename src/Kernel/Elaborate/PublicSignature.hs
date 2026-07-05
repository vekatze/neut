module Kernel.Elaborate.PublicSignature
  ( checkGeist,
    checkStmt,
    checkDefineMeta,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Module qualified as Module
import Kernel.Common.ReadableDD qualified as ReadableDD
import Language.Common.Availability qualified as AV
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Geist qualified as G
import Language.Common.ModuleID qualified as MID
import Language.Common.StmtKind qualified as SK
import Language.Term.Stmt qualified as Stmt
import Language.Term.Term qualified as TM
import Logger.Hint

checkStmt :: Global.Handle -> Module.Module -> Stmt.Stmt -> App ()
checkStmt globalHandle baseModule stmt =
  case stmt of
    Stmt.StmtDefine _ _ _ name impArgs expArgs defaultArgs codType _ -> do
      let binderList = impArgs ++ expArgs ++ map fst defaultArgs
      checkSignatureTypeList globalHandle baseModule name $ collectBinderTypes binderList ++ [codType]
    Stmt.StmtDefineType _ stmtKind _ name impArgs expArgs defaultArgs codType body -> do
      let binderList = impArgs ++ expArgs ++ map fst defaultArgs
      checkSignatureTypeList globalHandle baseModule name $
        collectBinderTypes binderList ++ [codType] ++ collectTypeBody stmtKind body
    Stmt.StmtDefineResource {} ->
      return ()
    Stmt.StmtTrope {} ->
      return ()
    Stmt.StmtVariadic {} ->
      return ()
    Stmt.StmtForeign _ ->
      return ()

checkGeist :: Global.Handle -> Module.Module -> G.Geist TM.Type TM.Term -> App ()
checkGeist globalHandle baseModule geist = do
  let defaultBinders = map fst $ G.defaultArgs geist
  checkSignatureTypeList globalHandle baseModule (G.name geist) $
    collectBinderTypes (G.impArgs geist ++ G.expArgs geist ++ defaultBinders)
      ++ [G.cod geist]

checkDefineMeta :: Global.Handle -> Module.Module -> DD.DefiniteDescription -> Stmt.DefineMeta -> App ()
checkDefineMeta globalHandle baseModule name defineMeta =
  checkSignatureTypeList globalHandle baseModule name $
    Stmt.defineMetaTargetArgs defineMeta
      ++ collectBinderTypes (Stmt.defineMetaExpArgs defineMeta)
      ++ [Stmt.defineMetaCodType defineMeta]

collectTypeBody :: SK.StmtKindType TM.Type -> TM.Type -> [TM.Type]
collectTypeBody stmtKind body =
  case stmtKind of
    SK.Alias ->
      [body]
    SK.AliasOpaque ->
      []
    SK.Data {} ->
      []

collectBinderTypes :: [BinderF TM.Type] -> [TM.Type]
collectBinderTypes binderList =
  flip map binderList $ \(_, _, _, t) -> t

checkSignatureTypeList :: Global.Handle -> Module.Module -> DD.DefiniteDescription -> [TM.Type] -> App ()
checkSignatureTypeList globalHandle baseModule ownerName typeList = do
  when (isSourceLevelName ownerName) $
    forM_ (concatMap collectTypeDDs typeList) $ \(m, depName) -> do
      unless (AV.allowsAsSignatureDependency ownerName depName) $
        raiseSignatureVisibilityError baseModule m ownerName depName
      isReachable <- isPubliclyReachableFrom globalHandle m baseModule depName
      unless isReachable $
        raiseSignatureVisibilityError baseModule m ownerName depName

isPubliclyReachableFrom ::
  Global.Handle ->
  Hint ->
  Module.Module ->
  DD.DefiniteDescription ->
  App Bool
isPubliclyReachableFrom globalHandle m ownerModule depName = do
  let (depModuleID, _) = DD.unconsDD depName
  isPubliclyReachable globalHandle m ownerModule depModuleID

isPubliclyReachable ::
  Global.Handle ->
  Hint ->
  Module.Module ->
  MID.ModuleID ->
  App Bool
isPubliclyReachable globalHandle m ownerModule targetModuleID = do
  let ownerModuleID = Module.moduleID ownerModule
  if targetModuleID == ownerModuleID || targetModuleID == MID.Base
    then return True
    else do
      reachabilityMap <- liftIO $ readIORef (Global.publicModuleReachabilityRef globalHandle)
      case Map.lookup ownerModuleID reachabilityMap of
        Just reachableModuleSet ->
          return $ S.member targetModuleID reachableModuleSet
        Nothing ->
          raiseError m $
            "No public reachability information is registered for module `"
              <> MID.reify ownerModuleID
              <> "`"

collectTypeDDs :: TM.Type -> [(Hint, DD.DefiniteDescription)]
collectTypeDDs typ =
  case typ of
    _ :< TM.Tau ->
      []
    _ :< TM.TVar {} ->
      []
    m :< TM.TVarGlobal _ name ->
      [(m, name)]
    _ :< TM.TyApp t args ->
      concatMap collectTypeDDs (t : args)
    _ :< TM.Pi _ impArgs expArgs defaultArgs codType ->
      concatMap collectTypeDDs $
        collectBinderTypes (impArgs ++ expArgs ++ defaultArgs) ++ [codType]
    m :< TM.Data _ name args ->
      (m, name) : concatMap collectTypeDDs args
    _ :< TM.Box t ->
      collectTypeDDs t
    _ :< TM.BoxNoema t ->
      collectTypeDDs t
    _ :< TM.Code t ->
      collectTypeDDs t
    _ :< TM.PrimType {} ->
      []
    _ :< TM.Void ->
      []
    m :< TM.Resource name _ ->
      [(m, name)]

raiseSignatureVisibilityError ::
  Module.Module ->
  Hint ->
  DD.DefiniteDescription ->
  DD.DefiniteDescription ->
  App a
raiseSignatureVisibilityError baseModule m ownerName depName = do
  let ownerName' = ReadableDD.readableDD' baseModule ownerName
  let depName' = ReadableDD.readableDD' baseModule depName
  raiseError m $
    "`"
      <> ownerName'
      <> "` is more public than `"
      <> depName'
      <> "`"

isSourceLevelName :: DD.DefiniteDescription -> Bool
isSourceLevelName name = do
  let rawName = DD.reify name
  not (";" `T.isInfixOf` rawName) && not ("#" `T.isInfixOf` rawName)
