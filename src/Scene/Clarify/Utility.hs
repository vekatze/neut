module Scene.Clarify.Utility where

import Context.App
import Context.Clarify
import Context.Gensym qualified as Gensym
import Entity.Comp qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.EnumCase qualified as EC
import Entity.Ident
import Entity.Opacity qualified as O
import Entity.PrimNumSize

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Ident -> C.Comp -> App C.Comp
toAffineApp x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize 64) 0, C.VarLocal x])

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Ident -> C.Comp -> App C.Comp
toRelevantApp x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith "exp"
  return $ C.UpElim True expVarName t (C.PiElimDownElim expVar [C.Int (IntSize 64) 1, C.VarLocal x])

bindLet :: [(Ident, C.Comp)] -> C.Comp -> C.Comp
bindLet binder cont =
  case binder of
    [] ->
      cont
    (x, e) : xes ->
      C.UpElim True x e $ bindLet xes cont

makeSwitcher ::
  (C.Value -> App C.Comp) ->
  (C.Value -> App C.Comp) ->
  App ([Ident], C.Comp)
makeSwitcher compAff compRel = do
  (switchVarName, switchVar) <- Gensym.newValueVarLocalWith "switch"
  (argVarName, argVar) <- Gensym.newValueVarLocalWith "arg"
  aff <- compAff argVar
  rel <- compRel argVar
  return
    ( [switchVarName, argVarName],
      C.EnumElim
        switchVar
        rel
        [(EC.Int 0, aff)]
    )

registerSwitcher ::
  DD.DefiniteDescription ->
  (C.Value -> App C.Comp) ->
  (C.Value -> App C.Comp) ->
  App ()
registerSwitcher name aff rel = do
  (args, e) <- makeSwitcher aff rel
  insertToAuxEnv name (O.Transparent, args, e)
