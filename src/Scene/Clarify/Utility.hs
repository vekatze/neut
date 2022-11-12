module Scene.Clarify.Utility where

import qualified Context.Gensym as Gensym
import Control.Comonad.Cofree
import qualified Entity.Comp as C
import qualified Entity.DefiniteDescription as DD
import qualified Entity.EnumCase as EC
import Entity.Ident
import qualified Entity.Opacity as O
import Entity.PrimNumSize
import Scene.Clarify.Context

toApp :: Gensym.Context m => Integer -> Ident -> C.Comp -> m C.Comp
toApp switcher x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith "exp"
  return $
    C.UpElim
      expVarName
      t
      ( C.PiElimDownElim
          expVar
          [C.Int (IntSize 64) switcher, C.VarLocal x]
      )

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Gensym.Context m => Ident -> C.Comp -> m C.Comp
toAffineApp =
  toApp 0

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Gensym.Context m => Ident -> C.Comp -> m C.Comp
toRelevantApp =
  toApp 1

bindLet :: [(Ident, C.Comp)] -> C.Comp -> C.Comp
bindLet binder cont =
  case binder of
    [] ->
      cont
    (x, e) : xes ->
      C.UpElim x e $ bindLet xes cont

switch :: C.Comp -> C.Comp -> [(EC.CompEnumCase, C.Comp)]
switch e1 e2 =
  [(() :< EC.Int 0, e1), (() :< EC.Default, e2)]

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
        (switch aff rel)
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
