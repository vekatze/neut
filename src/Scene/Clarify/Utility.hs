module Scene.Clarify.Utility where

import qualified Context.Gensym as Gensym
import Control.Comonad.Cofree
import Entity.Comp
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.Ident
import Entity.Opacity
import Entity.PrimNumSize
import Scene.Clarify.Context

toApp :: Gensym.Context m => Integer -> Ident -> Comp -> m Comp
toApp switcher x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith "exp"
  return $
    CompUpElim
      expVarName
      t
      ( CompPiElimDownElim
          expVar
          [ValueInt (IntSize 64) switcher, ValueVarLocal x]
      )

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Gensym.Context m => Ident -> Comp -> m Comp
toAffineApp =
  toApp 0

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Gensym.Context m => Ident -> Comp -> m Comp
toRelevantApp =
  toApp 1

bindLet :: [(Ident, Comp)] -> Comp -> Comp
bindLet binder cont =
  case binder of
    [] ->
      cont
    (x, e) : xes ->
      CompUpElim x e $ bindLet xes cont

switch :: Comp -> Comp -> [(CompEnumCase, Comp)]
switch e1 e2 =
  [(() :< EnumCaseInt 0, e1), (() :< EnumCaseDefault, e2)]

makeSwitcher ::
  Gensym.Context m =>
  (Value -> m Comp) ->
  (Value -> m Comp) ->
  m ([Ident], Comp)
makeSwitcher compAff compRel = do
  (switchVarName, switchVar) <- Gensym.newValueVarLocalWith "switch"
  (argVarName, argVar) <- Gensym.newValueVarLocalWith "arg"
  aff <- compAff argVar
  rel <- compRel argVar
  return
    ( [switchVarName, argVarName],
      CompEnumElim
        switchVar
        (switch aff rel)
    )

registerSwitcher ::
  Context m =>
  DD.DefiniteDescription ->
  (Value -> m Comp) ->
  (Value -> m Comp) ->
  m ()
registerSwitcher name aff rel = do
  (args, e) <- makeSwitcher aff rel
  insertToAuxEnv name (OpacityTransparent, args, e)
