module Move.Scene.Clarify.Utility
  ( toAffineApp,
    toRelevantApp,
    bindLet,
    bindLetWithReducibility,
    irreducibleBindLet,
    registerSwitcher,
    getEnumElim,
  )
where

import Control.Monad.IO.Class
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.Clarify
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Gensym qualified as Gensym
import Move.Language.Utility.Gensym qualified as GensymN
import Move.Scene.Comp.Subst qualified as C
import Rule.Comp qualified as C
import Rule.DefiniteDescription qualified as DD
import Rule.EnumCase
import Rule.EnumCase qualified as EC
import Rule.Ident
import Rule.Ident.Reify
import Rule.Opacity qualified as O
import Rule.PrimNumSize

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: GensymN.Handle -> Ident -> C.Comp -> EIO C.Comp
toAffineApp h x t = do
  (expVarName, expVar) <- liftIO $ GensymN.newValueVarLocalWith h "exp"
  baseSize <- Env.getBaseSize'
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize baseSize) 0, C.VarLocal x])

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: GensymN.Handle -> Ident -> C.Comp -> EIO C.Comp
toRelevantApp h x t = do
  (expVarName, expVar) <- liftIO $ GensymN.newValueVarLocalWith h "exp"
  baseSize <- Env.getBaseSize'
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize baseSize) 1, C.VarLocal x])

bindLet :: [(Ident, C.Comp)] -> C.Comp -> C.Comp
bindLet =
  bindLetWithReducibility True

irreducibleBindLet :: [(Ident, C.Comp)] -> C.Comp -> C.Comp
irreducibleBindLet =
  bindLetWithReducibility False

bindLetWithReducibility :: C.IsReducible -> [(Ident, C.Comp)] -> C.Comp -> C.Comp
bindLetWithReducibility isReducible binder cont =
  case binder of
    [] ->
      cont
    (x, e) : xes ->
      C.UpElim isReducible x e $ bindLetWithReducibility isReducible xes cont

makeSwitcher ::
  (C.Value -> App C.Comp) ->
  (C.Value -> App C.Comp) ->
  App ([Ident], C.Comp)
makeSwitcher compAff compRel = do
  (switchVarName, switchVar) <- Gensym.newValueVarLocalWith "switch"
  (argVarName, argVar) <- Gensym.newValueVarLocalWith "arg"
  aff <- compAff argVar
  rel <- compRel argVar
  enumElim <- getEnumElim [argVarName] switchVar rel [(EC.Int 0, aff)]
  return ([switchVarName, argVarName], enumElim)

registerSwitcher ::
  O.Opacity ->
  DD.DefiniteDescription ->
  (C.Value -> App C.Comp) ->
  (C.Value -> App C.Comp) ->
  App ()
registerSwitcher opacity name aff rel = do
  (args, e) <- makeSwitcher aff rel
  insertToAuxEnv name (opacity, args, e)

getEnumElim :: [Ident] -> C.Value -> C.Comp -> [(EnumCase, C.Comp)] -> App C.Comp
getEnumElim idents d defaultBranch branchList = do
  (newToOld, oldToNew) <- getSub idents
  let sub = IntMap.fromList oldToNew
  h <- C.new
  defaultBranch' <- liftIO $ C.subst h sub defaultBranch
  let (labels, clauses) = unzip branchList
  clauses' <- liftIO $ mapM (C.subst h sub) clauses
  return $ C.EnumElim newToOld d defaultBranch' (zip labels clauses')

getSub :: [Ident] -> App ([(Int, C.Value)], [(Int, C.Value)])
getSub idents = do
  newIdents <- mapM Gensym.newIdentFromIdent idents
  let newToOld = zip (map toInt newIdents) (map C.VarLocal idents)
  let oldToNew = zip (map toInt idents) (map C.VarLocal newIdents)
  return (newToOld, oldToNew)
