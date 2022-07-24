module Scene.Clarify.Utility where

import qualified Context.App as App
import qualified Context.Gensym as Gensym
import Control.Comonad.Cofree
import Entity.Comp
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.Ident
import Entity.Opacity
import Entity.PrimNumSize
import Scene.Clarify.Context

toApp :: Gensym.Context -> Integer -> Ident -> Comp -> IO Comp
toApp ctx switcher x t = do
  (expVarName, expVar) <- Gensym.newValueVarLocalWith ctx "exp"
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
toAffineApp :: Gensym.Context -> Ident -> Comp -> IO Comp
toAffineApp ctx =
  toApp ctx 0

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Gensym.Context -> Ident -> Comp -> IO Comp
toRelevantApp ctx =
  toApp ctx 1

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
  Gensym.Context ->
  (Value -> IO Comp) ->
  (Value -> IO Comp) ->
  IO ([Ident], Comp)
makeSwitcher ctx compAff compRel = do
  (switchVarName, switchVar) <- Gensym.newValueVarLocalWith ctx "switch"
  (argVarName, argVar) <- Gensym.newValueVarLocalWith ctx "arg"
  aff <- compAff argVar
  rel <- compRel argVar
  return
    ( [switchVarName, argVarName],
      CompEnumElim
        switchVar
        (switch aff rel)
    )

registerSwitcher ::
  Context ->
  DD.DefiniteDescription ->
  (Value -> IO Comp) ->
  (Value -> IO Comp) ->
  IO ()
registerSwitcher ctx name aff rel = do
  (args, e) <- makeSwitcher (App.gensym (base ctx)) aff rel
  insertToAuxEnv ctx name (OpacityTransparent, args, e)
