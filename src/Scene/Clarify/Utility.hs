module Scene.Clarify.Utility where

import Context.Gensym
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Comp
import Entity.EnumCase
import Entity.Global
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.Opacity
import Entity.PrimNumSize

toApp :: Context -> Integer -> Ident -> Comp -> IO Comp
toApp ctx switcher x t = do
  (expVarName, expVar) <- newValueVarLocalWith ctx "exp"
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
toAffineApp :: Context -> Ident -> Comp -> IO Comp
toAffineApp ctx =
  toApp ctx 0

-- toApp boolFalse

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Context -> Ident -> Comp -> IO Comp
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

tryCache :: T.Text -> IO () -> IO Value
tryCache key doInsertion = do
  compDefEnv <- readIORef compDefEnvRef
  unless (Map.member key compDefEnv) doInsertion
  return $ ValueVarGlobal key

makeSwitcher ::
  Context ->
  (Value -> IO Comp) ->
  (Value -> IO Comp) ->
  IO ([Ident], Comp)
makeSwitcher ctx compAff compRel = do
  (switchVarName, switchVar) <- newValueVarLocalWith ctx "switch"
  (argVarName, argVar) <- newValueVarLocalWith ctx "arg"
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
  T.Text ->
  (Value -> IO Comp) ->
  (Value -> IO Comp) ->
  IO ()
registerSwitcher ctx name aff rel = do
  (args, e) <- makeSwitcher ctx aff rel
  insDefEnv name OpacityTransparent args e

insDefEnv :: T.Text -> Opacity -> [Ident] -> Comp -> IO ()
insDefEnv name opacity args e =
  modifyIORef' compDefEnvRef $ Map.insert name (opacity, args, e)

{-# INLINE toConstructorLabelName #-}
toConstructorLabelName :: Ident -> T.Text
toConstructorLabelName x =
  wrapWithQuote $ Ident.toText x

{-# INLINE wrapWithQuote #-}
wrapWithQuote :: T.Text -> T.Text
wrapWithQuote x =
  "\"" <> x <> "\""
