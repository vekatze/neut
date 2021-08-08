module Clarify.Utility where

import Control.Monad
import Data.Basic
import Data.Comp
import Data.Global
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import Data.Namespace
import qualified Data.Text as T
import Path

toApp :: T.Text -> Ident -> Comp -> IO Comp
toApp switcher x t = do
  (expVarName, expVar) <- newValueVarLocalWith "exp"
  path <- toFilePath <$> getExecPath
  return $
    CompUpElim
      expVarName
      t
      ( CompPiElimDownElim
          expVar
          [ValueEnumIntro path switcher, ValueVarLocal x]
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

switch :: FilePath -> Comp -> Comp -> [(EnumCase, Comp)]
switch path e1 e2 =
  -- [(EnumCaseInt 0, e1), (EnumCaseDefault, e2)]
  [(EnumCaseLabel path boolFalse, e1), (EnumCaseDefault, e2)]

tryCache :: T.Text -> IO () -> IO Value
tryCache key doInsertion = do
  denv <- readIORef defEnv
  when (not $ Map.member key denv) doInsertion
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
  path <- toFilePath <$> getExecPath
  return
    ( [switchVarName, argVarName],
      ( CompEnumElim
          switchVar
          (switch path aff rel)
      )
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
  modifyIORef' defEnv $ \env -> Map.insert name (isReducible, args, Just e) env

insDefEnv' :: T.Text -> Bool -> [Ident] -> IO ()
insDefEnv' name isReducible args =
  modifyIORef' defEnv $ \env -> Map.insert name (isReducible, args, Nothing) env

{-# INLINE boolTrue #-}
boolTrue :: T.Text
boolTrue =
  "bool" <> nsSep <> "true"

{-# INLINE boolFalse #-}
boolFalse :: T.Text
boolFalse =
  "bool" <> nsSep <> "false"

{-# INLINE toGlobalVarName #-}
toGlobalVarName :: FilePath -> T.Text -> T.Text
toGlobalVarName path x =
  wrapWithQuote $ x <> ";" <> T.pack path

-- {-# INLINE toGlobalVarName #-}
-- toGlobalVarName :: FilePath -> Ident -> T.Text
-- toGlobalVarName path x =
--   wrapWithQuote $ asText x <> ";" <> T.pack path

-- {-# INLINE toGlobalVarName #-}
-- toGlobalVarName :: Ident -> T.Text
-- toGlobalVarName x =
--   wrapWithQuote $ "neut:" <> asText x

{-# INLINE toConstructorLabelName #-}
toConstructorLabelName :: Ident -> T.Text
toConstructorLabelName x =
  wrapWithQuote $ asText x

{-# INLINE wrapWithQuote #-}
wrapWithQuote :: T.Text -> T.Text
wrapWithQuote x =
  "\"" <> x <> "\""
