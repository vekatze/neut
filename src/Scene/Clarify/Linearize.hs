module Scene.Clarify.Linearize
  ( linearize,
  )
where

import Context.Gensym
import Control.Monad
import Entity.Comp
import Entity.Ident
import Entity.Magic
import Scene.Clarify.Utility

data Occurrence
  = OccurrenceNormal Ident
  | OccurrenceIdeal Ident
  deriving (Show)

linearize ::
  Context ->
  [(Ident, Comp)] -> -- [(x1, t1), ..., (xn, tn)]  (closed chain)
  Comp -> -- a term that can contain non-linear occurrences of xi
  IO Comp -- a term in which all the variables in the closed chain occur linearly
linearize ctx binder e =
  case binder of
    [] ->
      return e
    (x, t) : xts -> do
      e' <- linearize ctx xts e
      (newNameList, e'') <- distinguishComp ctx x e'
      case newNameList of
        [] -> do
          hole <- newIdentFromText ctx "unit"
          discardUnusedVar <- toAffineApp ctx x t
          return $ CompUpElim hole discardUnusedVar e''
        z : zs ->
          insertHeader ctx x z zs t e''

insertFooter :: Context -> Ident -> Comp -> Comp -> IO Comp
insertFooter ctx x t e = do
  ans <- newIdentFromText ctx "answer"
  hole <- newIdentFromText ctx "unit"
  discardUnusedVar <- toAffineApp ctx x t
  return $
    CompUpElim ans e $
      CompUpElim hole discardUnusedVar $
        CompUpIntro (ValueVarLocal ans)

insertHeader ::
  Context ->
  Ident ->
  Occurrence ->
  [Occurrence] ->
  Comp ->
  Comp ->
  IO Comp
insertHeader ctx x occurrence zs t e = do
  case zs of
    [] ->
      case occurrence of
        OccurrenceNormal z1 ->
          return $ CompUpElim z1 (CompUpIntro (ValueVarLocal x)) e
        OccurrenceIdeal z1 -> do
          e' <- insertFooter ctx z1 t e
          return $ CompUpElim z1 (CompUpIntro (ValueVarLocal x)) e'
    z2 : rest -> do
      case occurrence of
        OccurrenceNormal z1 -> do
          e' <- insertHeader ctx x z2 rest t e
          copyRelevantVar <- toRelevantApp ctx x t
          return $ CompUpElim z1 copyRelevantVar e'
        OccurrenceIdeal z1 -> do
          e' <- insertHeader ctx x z2 rest t e
          return $ CompUpElim z1 (CompUpIntro (ValueVarLocal x)) e'

distinguishValue :: Context -> Ident -> Value -> IO ([Occurrence], Value)
distinguishValue ctx z term =
  case term of
    ValueVarLocal x ->
      if x /= z
        then return ([], term)
        else do
          x' <- newIdentFromIdent ctx x
          return ([OccurrenceNormal x'], ValueVarLocal x')
    ValueVarLocalIdeal x ->
      if x /= z
        then return ([], term)
        else do
          x' <- newIdentFromIdent ctx x
          return ([OccurrenceIdeal x'], ValueVarLocal x')
    ValueSigmaIntro ds -> do
      (vss, ds') <- unzip <$> mapM (distinguishValue ctx z) ds
      return (concat vss, ValueSigmaIntro ds')
    _ ->
      return ([], term)

distinguishComp :: Context -> Ident -> Comp -> IO ([Occurrence], Comp)
distinguishComp ctx z term =
  case term of
    CompPrimitive theta -> do
      (vs, theta') <- distinguishPrimitive ctx z theta
      return (vs, CompPrimitive theta')
    CompPiElimDownElim d ds -> do
      (vs, d') <- distinguishValue ctx z d
      (vss, ds') <- unzip <$> mapM (distinguishValue ctx z) ds
      return (concat $ vs : vss, CompPiElimDownElim d' ds')
    CompSigmaElim b xs d e -> do
      (vs1, d') <- distinguishValue ctx z d
      (vs2, e') <- distinguishComp ctx z e
      return (vs1 ++ vs2, CompSigmaElim b xs d' e')
    CompUpIntro d -> do
      (vs, d') <- distinguishValue ctx z d
      return (vs, CompUpIntro d')
    CompUpElim x e1 e2 -> do
      (vs1, e1') <- distinguishComp ctx z e1
      (vs2, e2') <- distinguishComp ctx z e2
      return (vs1 ++ vs2, CompUpElim x e1' e2')
    CompArrayAccess elemType array index -> do
      (vs1, array') <- distinguishValue ctx z array
      (vs2, index') <- distinguishValue ctx z index
      return (vs1 ++ vs2, CompArrayAccess elemType array' index')
    CompEnumElim d branchList -> do
      (vs, d') <- distinguishValue ctx z d
      case branchList of
        [] ->
          return (vs, CompEnumElim d' [])
        _ -> do
          let (cs, es) = unzip branchList
          -- countBefore <- readIORef countRef
          countBefore <- readCount ctx
          (vss, es') <- fmap unzip $
            forM es $ \e -> do
              writeCount ctx countBefore
              -- writeIORef countRef countBefore
              distinguishComp ctx z e
          return (vs ++ head vss, CompEnumElim d' (zip cs es'))

distinguishPrimitive :: Context -> Ident -> Primitive -> IO ([Occurrence], Primitive)
distinguishPrimitive ctx z term =
  case term of
    PrimitivePrimOp op ds -> do
      (vss, ds') <- unzip <$> mapM (distinguishValue ctx z) ds
      return (concat vss, PrimitivePrimOp op ds')
    PrimitiveMagic der -> do
      case der of
        MagicCast from to value -> do
          (vs1, from') <- distinguishValue ctx z from
          (vs2, to') <- distinguishValue ctx z to
          (vs3, value') <- distinguishValue ctx z value
          return (vs1 <> vs2 <> vs3, PrimitiveMagic (MagicCast from' to' value'))
        MagicStore lt pointer value -> do
          (vs1, pointer') <- distinguishValue ctx z pointer
          (vs2, value') <- distinguishValue ctx z value
          return (vs1 <> vs2, PrimitiveMagic (MagicStore lt pointer' value'))
        MagicLoad lt pointer -> do
          (vs, pointer') <- distinguishValue ctx z pointer
          return (vs, PrimitiveMagic (MagicLoad lt pointer'))
        MagicSyscall syscallNum args -> do
          (vss, args') <- unzip <$> mapM (distinguishValue ctx z) args
          return (concat vss, PrimitiveMagic (MagicSyscall syscallNum args'))
        MagicExternal extFunName args -> do
          (vss, args') <- unzip <$> mapM (distinguishValue ctx z) args
          return (concat vss, PrimitiveMagic (MagicExternal extFunName args'))
