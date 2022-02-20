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
    Value (..),
  )
import Data.Global (countRef, newIdentFromIdent, newIdentFromText)
import Data.IORef (readIORef, writeIORef)
import Data.LowType (Magic (..))

data Occurrence
  = OccurrenceNormal Ident
  | OccurrenceIdeal Ident
  deriving (Show)

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
        [] -> do
          hole <- newIdentFromText "unit"
          discardUnusedVar <- toAffineApp x t
          return $ CompUpElim hole discardUnusedVar e''
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
  Occurrence ->
  [Occurrence] ->
  Comp ->
  Comp ->
  IO Comp
insertHeader x occurrence zs t e = do
  case zs of
    [] ->
      case occurrence of
        OccurrenceNormal z1 ->
          return $ CompUpElim z1 (CompUpIntro (ValueVarLocal x)) e
        OccurrenceIdeal z1 -> do
          e' <- insertFooter z1 t e
          return $ CompUpElim z1 (CompUpIntro (ValueVarLocal x)) e'
    z2 : rest -> do
      case occurrence of
        OccurrenceNormal z1 -> do
          e' <- insertHeader x z2 rest t e
          copyRelevantVar <- toRelevantApp x t
          return $ CompUpElim z1 copyRelevantVar e'
        OccurrenceIdeal z1 -> do
          e' <- insertHeader x z2 rest t e
          return $ CompUpElim z1 (CompUpIntro (ValueVarLocal x)) e'

distinguishValue :: Ident -> Value -> IO ([Occurrence], Value)
distinguishValue z term =
  case term of
    ValueVarLocal x ->
      if x /= z
        then return ([], term)
        else do
          x' <- newIdentFromIdent x
          return ([OccurrenceNormal x'], ValueVarLocal x')
    ValueVarLocalIdeal x ->
      if x /= z
        then return ([], term)
        else do
          x' <- newIdentFromIdent x
          return ([OccurrenceIdeal x'], ValueVarLocal x')
    ValueSigmaIntro ds -> do
      (vss, ds') <- unzip <$> mapM (distinguishValue z) ds
      return (concat vss, ValueSigmaIntro ds')
    _ ->
      return ([], term)

distinguishComp :: Ident -> Comp -> IO ([Occurrence], Comp)
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
    CompArrayAccess elemType array index -> do
      (vs1, array') <- distinguishValue z array
      (vs2, index') <- distinguishValue z index
      return (vs1 ++ vs2, CompArrayAccess elemType array' index')
    CompEnumElim d branchList -> do
      (vs, d') <- distinguishValue z d
      case branchList of
        [] ->
          return (vs, CompEnumElim d' [])
        _ -> do
          let (cs, es) = unzip branchList
          countBefore <- readIORef countRef
          (vss, es') <- fmap unzip $
            forM es $ \e -> do
              writeIORef countRef countBefore
              distinguishComp z e
          return (vs ++ head vss, CompEnumElim d' (zip cs es'))

distinguishPrimitive :: Ident -> Primitive -> IO ([Occurrence], Primitive)
distinguishPrimitive z term =
  case term of
    PrimitivePrimOp op ds -> do
      (vss, ds') <- unzip <$> mapM (distinguishValue z) ds
      return (concat vss, PrimitivePrimOp op ds')
    PrimitiveMagic der -> do
      case der of
        MagicCast from to value -> do
          (vs1, from') <- distinguishValue z from
          (vs2, to') <- distinguishValue z to
          (vs3, value') <- distinguishValue z value
          return (vs1 <> vs2 <> vs3, PrimitiveMagic (MagicCast from' to' value'))
        MagicStore lt pointer value -> do
          (vs1, pointer') <- distinguishValue z pointer
          (vs2, value') <- distinguishValue z value
          return (vs1 <> vs2, PrimitiveMagic (MagicStore lt pointer' value'))
        MagicLoad lt pointer -> do
          (vs, pointer') <- distinguishValue z pointer
          return (vs, PrimitiveMagic (MagicLoad lt pointer'))
        MagicSyscall syscallNum args -> do
          (vss, args') <- unzip <$> mapM (distinguishValue z) args
          return (concat vss, PrimitiveMagic (MagicSyscall syscallNum args'))
        MagicExternal extFunName args -> do
          (vss, args') <- unzip <$> mapM (distinguishValue z) args
          return (concat vss, PrimitiveMagic (MagicExternal extFunName args'))
