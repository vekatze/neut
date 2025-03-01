module Scene.Clarify.Utility
  ( toAffineApp,
    toRelevantApp,
    bindLet,
    bindLetWithReducibility,
    irreducibleBindLet,
    makeSwitcher,
    registerSwitcher,
    getEnumElim,
  )
where

import Context.App
import Context.Clarify
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Data.IntMap qualified as IntMap
import Entity.Comp qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.EnumCase
import Entity.EnumCase qualified as EC
import Entity.Ident
import Entity.Ident.Reify
import Entity.Opacity qualified as O
import Entity.PrimNumSize
import Scene.Comp.Subst qualified as C

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Ident -> C.Comp -> App C.Comp
toAffineApp x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith "exp"
  baseSize <- Env.getBaseSize'
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize baseSize) 0, C.VarLocal x])

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Ident -> C.Comp -> App C.Comp
toRelevantApp x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith "exp"
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
  defaultBranch' <- C.subst sub defaultBranch
  let (labels, clauses) = unzip branchList
  clauses' <- mapM (C.subst sub) clauses
  return $ C.EnumElim newToOld d defaultBranch' (zip labels clauses')

getSub :: [Ident] -> App ([(Int, C.Value)], [(Int, C.Value)])
getSub idents = do
  newIdents <- mapM Gensym.newIdentFromIdent idents
  let newToOld = zip (map toInt newIdents) (map C.VarLocal idents)
  let oldToNew = zip (map toInt idents) (map C.VarLocal newIdents)
  return (newToOld, oldToNew)
