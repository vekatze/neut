module Kernel.Elaborate.PublicSignature
  ( checkDefineMeta,
    checkGeist,
    checkStmt,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Data.Text qualified as T
import Kernel.Common.Module qualified as Module
import Kernel.Common.ReadableDD qualified as ReadableDD
import Language.Common.Availability qualified as AV
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Geist qualified as G
import Language.Common.StmtKind qualified as SK
import Language.Term.Stmt qualified as Stmt
import Language.Term.Term qualified as TM
import Logger.Hint

checkStmt :: Module.Module -> Stmt.Stmt -> App ()
checkStmt baseModule stmt =
  case stmt of
    Stmt.StmtDefine _ _ _ name impArgs expArgs defaultArgs codType _ -> do
      let defaultBinders = map fst defaultArgs
      checkSignatureTypeList baseModule name $ collectBinderTypes (impArgs ++ expArgs ++ defaultBinders) ++ [codType]
    Stmt.StmtDefineType _ stmtKind _ name impArgs expArgs defaultArgs codType body -> do
      let defaultBinders = map fst defaultArgs
      checkSignatureTypeList baseModule name $
        collectBinderTypes (impArgs ++ expArgs ++ defaultBinders)
          ++ [codType]
          ++ collectTypeBody stmtKind body
    Stmt.StmtDefineResource _ _ _ _ _ _ _ ->
      return ()
    Stmt.StmtTrope _ _ _ ->
      return ()
    Stmt.StmtVariadic _ _ _ ->
      return ()
    Stmt.StmtForeign _ ->
      return ()

checkGeist :: Module.Module -> G.Geist TM.Type TM.Term -> App ()
checkGeist baseModule geist = do
  let defaultBinders = map fst $ G.defaultArgs geist
  checkSignatureTypeList baseModule (G.name geist) $
    collectBinderTypes (G.impArgs geist ++ G.expArgs geist ++ defaultBinders)
      ++ [G.cod geist]

checkDefineMeta :: Module.Module -> DD.DefiniteDescription -> Stmt.DefineMeta -> App ()
checkDefineMeta baseModule name defineMeta =
  checkSignatureTypeList baseModule name $
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
    SK.Data _ _ _ _ _ ->
      []

collectBinderTypes :: [BinderF TM.Type] -> [TM.Type]
collectBinderTypes binderList =
  flip map binderList $ \(_, _, _, t) -> t

checkSignatureTypeList :: Module.Module -> DD.DefiniteDescription -> [TM.Type] -> App ()
checkSignatureTypeList baseModule ownerName typeList = do
  when (isSourceLevelName ownerName) $
    forM_ (concatMap collectTypeDDs typeList) $ \(m, depName) -> do
      unless (AV.allowsAsSignatureDependency ownerName depName) $
        raiseSignatureVisibilityError baseModule m ownerName depName

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
