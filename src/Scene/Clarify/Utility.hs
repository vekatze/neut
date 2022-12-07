module Scene.Clarify.Utility where

import qualified Context.Gensym as Gensym
import Control.Comonad.Cofree
import qualified Entity.Comp as C
import qualified Entity.DefiniteDescription as DD
import qualified Entity.EnumCase as EC
import Entity.Ident
import qualified Entity.Opacity as O
import Scene.Clarify.Context

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Gensym.Context m => Ident -> C.Comp -> m C.Comp
toAffineApp x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith "exp"
  return $ C.UpElim expVarName t (C.Discard expVar x)

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Gensym.Context m => Ident -> C.Comp -> m C.Comp
toRelevantApp x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith "exp"
  return $ C.UpElim expVarName t (C.Copy expVar x)

bindLet :: [(Ident, C.Comp)] -> C.Comp -> C.Comp
bindLet binder cont =
  case binder of
    [] ->
      cont
    (x, e) : xes ->
      C.UpElim x e $ bindLet xes cont

makeSwitcher ::
  Gensym.Context m =>
  (C.Value -> m C.Comp) ->
  (C.Value -> m C.Comp) ->
  m ([Ident], C.Comp)
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
        [(() :< EC.Int 0, aff)]
    )

registerSwitcher ::
  Context m =>
  DD.DefiniteDescription ->
  (C.Value -> m C.Comp) ->
  (C.Value -> m C.Comp) ->
  m ()
registerSwitcher name aff rel = do
  (args, e) <- makeSwitcher aff rel
  insertToAuxEnv name (O.Transparent, args, e)
