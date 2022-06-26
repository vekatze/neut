module Scene.Clarify.Utility where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Basic
import Entity.Comp
import Entity.Global
import Entity.PrimNumSize

toApp :: Integer -> Ident -> Comp -> IO Comp
toApp switcher x t = do
  (expVarName, expVar) <- newValueVarLocalWith "exp"
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
toAffineApp :: Ident -> Comp -> IO Comp
toAffineApp =
  toApp 0

-- toApp boolFalse

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Ident -> Comp -> IO Comp
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

tryCache :: T.Text -> IO () -> IO Value
tryCache key doInsertion = do
  compDefEnv <- readIORef compDefEnvRef
  unless (Map.member key compDefEnv) doInsertion
  return $ ValueVarGlobal key

makeSwitcher ::
  (Value -> IO Comp) ->
  (Value -> IO Comp) ->
  IO ([Ident], Comp)
makeSwitcher compAff compRel = do
  (switchVarName, switchVar) <- newValueVarLocalWith "switch"
  (argVarName, argVar) <- newValueVarLocalWith "arg"
  aff <- compAff argVar
  rel <- compRel argVar
  return
    ( [switchVarName, argVarName],
      CompEnumElim
        switchVar
        (switch aff rel)
    )

registerSwitcher ::
  T.Text ->
  (Value -> IO Comp) ->
  (Value -> IO Comp) ->
  IO ()
registerSwitcher name aff rel = do
  (args, e) <- makeSwitcher aff rel
  insDefEnv name OpacityTransparent args e

insDefEnv :: T.Text -> Opacity -> [Ident] -> Comp -> IO ()
insDefEnv name opacity args e =
  modifyIORef' compDefEnvRef $ Map.insert name (opacity, args, e)

{-# INLINE toConstructorLabelName #-}
toConstructorLabelName :: Ident -> T.Text
toConstructorLabelName x =
  wrapWithQuote $ asText x

{-# INLINE wrapWithQuote #-}
wrapWithQuote :: T.Text -> T.Text
wrapWithQuote x =
  "\"" <> x <> "\""
