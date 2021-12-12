module Clarify.Linearize
  ( linearize,
  )
where

import Clarify.Utility (toAffineApp, toRelevantApp)
import Control.Monad (forM)
import Data.Basic (Ident)
import Data.Comp
  ( Comp (..),
    Primitive (..),
    Value (ValueSigmaIntro, ValueVarLocal),
  )
import Data.Global (count, newIdentFromIdent, newIdentFromText)
import Data.IORef (readIORef, writeIORef)

linearize ::
  [(Ident, Comp)] -> -- [(x1, t1), ..., (xn, tn)]  (closed chain)
  Comp -> -- a term that can contain non-linear occurrences of xi
  IO Comp -- a term in which all the variables in the closed chain occur linearly
linearize binder e =
  case binder of
    [] ->
      return e
    (x, t) : xts -> do
      e' <- linearize xts e
      (newNameList, e'') <- distinguishComp x e'
      case newNameList of
        [] ->
          insertFooter x t e''
        z : zs ->
          insertHeader x z zs t e''

insertFooter :: Ident -> Comp -> Comp -> IO Comp
insertFooter x t e = do
  ans <- newIdentFromText "answer"
  hole <- newIdentFromText "unit"
  discardUnusedVar <- toAffineApp x t
  return $
    CompUpElim ans e $
      CompUpElim hole discardUnusedVar $
        CompUpIntro (ValueVarLocal ans)

insertHeader ::
  Ident ->
  Ident ->
  [Ident] ->
  Comp ->
  Comp ->
  IO Comp
insertHeader x z1 zs t e = do
  case zs of
    [] ->
      return $ CompUpElim z1 (CompUpIntro (ValueVarLocal x)) e
    z2 : rest -> do
      e' <- insertHeader x z2 rest t e
      copyRelevantVar <- toRelevantApp x t
      return $ CompUpElim z1 copyRelevantVar e'

distinguishValue :: Ident -> Value -> IO ([Ident], Value)
distinguishValue z term =
  case term of
    ValueVarLocal x ->
      if x /= z
        then return ([], term)
        else do
          x' <- newIdentFromIdent x
          return ([x'], ValueVarLocal x')
    ValueSigmaIntro ds -> do
      (vss, ds') <- unzip <$> mapM (distinguishValue z) ds
      return (concat vss, ValueSigmaIntro ds')
    _ ->
      return ([], term)

distinguishComp :: Ident -> Comp -> IO ([Ident], Comp)
distinguishComp z term =
  case term of
    CompPrimitive theta -> do
      (vs, theta') <- distinguishPrimitive z theta
      return (vs, CompPrimitive theta')
    CompPiElimDownElim d ds -> do
      (vs, d') <- distinguishValue z d
      (vss, ds') <- unzip <$> mapM (distinguishValue z) ds
      return (concat $ vs : vss, CompPiElimDownElim d' ds')
    CompSigmaElim b xs d e -> do
      (vs1, d') <- distinguishValue z d
      (vs2, e') <- distinguishComp z e
      return (vs1 ++ vs2, CompSigmaElim b xs d' e')
    CompUpIntro d -> do
      (vs, d') <- distinguishValue z d
      return (vs, CompUpIntro d')
    CompUpElim x e1 e2 -> do
      (vs1, e1') <- distinguishComp z e1
      (vs2, e2') <- distinguishComp z e2
      return (vs1 ++ vs2, CompUpElim x e1' e2')
    CompEnumElim d branchList -> do
      (vs, d') <- distinguishValue z d
      case branchList of
        [] ->
          return (vs, CompEnumElim d' [])
        _ -> do
          let (cs, es) = unzip branchList
          countBefore <- readIORef count
          (vss, es') <- fmap unzip $
            forM es $ \e -> do
              writeIORef count countBefore
              distinguishComp z e
          return (vs ++ head vss, CompEnumElim d' (zip cs es'))
    CompIgnore _ ->
      return ([], term)

distinguishPrimitive :: Ident -> Primitive -> IO ([Ident], Primitive)
distinguishPrimitive z term =
  case term of
    PrimitivePrimOp op ds -> do
      (vss, ds') <- unzip <$> mapM (distinguishValue z) ds
      return (concat vss, PrimitivePrimOp op ds')
    PrimitiveDerangement k ds -> do
      (vss, ds') <- unzip <$> mapM (distinguishValue z) ds
      return (concat vss, PrimitiveDerangement k ds')
