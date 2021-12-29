module Clarify.Utility where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (unless)
import Data.Basic
  ( CompEnumCase,
    EnumCaseF (EnumCaseDefault, EnumCaseLabel),
    Ident,
    asText,
  )
import Data.Comp
  ( Comp (CompEnumElim, CompPiElimDownElim, CompUpElim),
    Value (ValueEnumIntro, ValueVarGlobal, ValueVarLocal),
  )
import Data.Global (boolFalse, boolTrue, compDefEnvRef, newValueVarLocalWith)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import qualified Data.Text as T

toApp :: T.Text -> Ident -> Comp -> IO Comp
toApp switcher x t = do
  (expVarName, expVar) <- newValueVarLocalWith "exp"
  return $
    CompUpElim
      expVarName
      t
      ( CompPiElimDownElim
          expVar
          [ValueEnumIntro switcher, ValueVarLocal x]
      )

-- toAffineApp meta x t ~>
--   bind exp := t in
--   exp @ (0, x)
toAffineApp :: Ident -> Comp -> IO Comp
toAffineApp =
  toApp boolFalse

-- toRelevantApp meta x t ~>
--   bind exp := t in
--   exp @ (1, x)
toRelevantApp :: Ident -> Comp -> IO Comp
toRelevantApp =
  toApp boolTrue

bindLet :: [(Ident, Comp)] -> Comp -> Comp
bindLet binder cont =
  case binder of
    [] ->
      cont
    (x, e) : xes ->
      CompUpElim x e $ bindLet xes cont

switch :: Comp -> Comp -> [(CompEnumCase, Comp)]
switch e1 e2 =
  [(() :< EnumCaseLabel boolFalse, e1), (() :< EnumCaseDefault, e2)]

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
  insDefEnv name True args e

insDefEnv :: T.Text -> Bool -> [Ident] -> Comp -> IO ()
insDefEnv name isReducible args e =
  modifyIORef' compDefEnvRef $ Map.insert name (isReducible, args, Just e)

insDefEnv' :: T.Text -> Bool -> [Ident] -> IO ()
insDefEnv' name isReducible args =
  modifyIORef' compDefEnvRef $ Map.insert name (isReducible, args, Nothing)

{-# INLINE toConstructorLabelName #-}
toConstructorLabelName :: Ident -> T.Text
toConstructorLabelName x =
  wrapWithQuote $ asText x

{-# INLINE wrapWithQuote #-}
wrapWithQuote :: T.Text -> T.Text
wrapWithQuote x =
  "\"" <> x <> "\""
