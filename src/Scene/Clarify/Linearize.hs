module Scene.Clarify.Linearize
  ( linearize,
  )
where

import Context.Gensym
import Control.Monad
import Debug.Trace
import qualified Entity.Comp as C
import Entity.Ident
import qualified Entity.Magic as M
import Scene.Clarify.Utility

type Occurrence = Ident

linearize ::
  Context m =>
  [(Ident, C.Comp)] -> -- [(x1, t1), ..., (xn, tn)]  (closed chain)
  C.Comp -> -- a term that can contain non-linear occurrences of xi
  m C.Comp -- a term in which all the variables in the closed chain occur linearly
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
          return $ C.UpElim hole discardUnusedVar e''
        z : zs ->
          insertHeader x z zs t e''

insertHeader ::
  Context m =>
  Ident ->
  Occurrence ->
  [Occurrence] ->
  C.Comp ->
  C.Comp ->
  m C.Comp
insertHeader x z1 zs t e = do
  case zs of
    [] ->
      return $ C.UpElim z1 (C.UpIntro (C.VarLocal x)) e
    z2 : rest -> do
      e' <- insertHeader x z2 rest t e
      copyRelevantVar <- toRelevantApp x t
      return $ C.UpElim z1 copyRelevantVar e'

distinguishVar :: Context m => Ident -> Ident -> m ([Occurrence], Ident)
distinguishVar z x =
  if x /= z
    then return ([], x)
    else do
      x' <- newIdentFromIdent x
      return ([x'], x')

distinguishValue :: Context m => Ident -> C.Value -> m ([Occurrence], C.Value)
distinguishValue z term =
  case term of
    C.VarLocal x -> do
      (vs, x') <- distinguishVar z x
      return (vs, C.VarLocal x')
    C.SigmaIntro ds -> do
      (vss, ds') <- mapAndUnzipM (distinguishValue z) ds
      return (concat vss, C.SigmaIntro ds')
    _ ->
      return ([], term)

distinguishComp :: Context m => Ident -> C.Comp -> m ([Occurrence], C.Comp)
distinguishComp z term =
  case term of
    C.Primitive theta -> do
      (vs, theta') <- distinguishPrimitive z theta
      return (vs, C.Primitive theta')
    C.PiElimDownElim d ds -> do
      (vs, d') <- distinguishValue z d
      (vss, ds') <- mapAndUnzipM (distinguishValue z) ds
      return (concat $ vs : vss, C.PiElimDownElim d' ds')
    C.SigmaElim shouldDeallocate xs d e -> do
      (vs1, d') <- distinguishValue z d
      (vs2, e') <- distinguishComp z e
      return (vs1 ++ vs2, C.SigmaElim shouldDeallocate xs d' e')
    C.UpIntro d -> do
      (vs, d') <- distinguishValue z d
      return (vs, C.UpIntro d')
    C.UpElim x e1 e2 -> do
      (vs1, e1') <- distinguishComp z e1
      (vs2, e2') <- distinguishComp z e2
      return (vs1 ++ vs2, C.UpElim x e1' e2')
    C.EnumElim d defaultBranch branchList -> do
      (vs, d') <- distinguishValue z d
      let (cs, es) = unzip branchList
      countBefore <- readCount
      (vss, es') <- fmap unzip $
        forM es $ \e -> do
          writeCount countBefore
          distinguishComp z e
      writeCount countBefore
      (_, defaultBranch') <- distinguishComp z defaultBranch
      return (vs ++ head vss, C.EnumElim d' defaultBranch' (zip cs es'))
    C.Discard d x -> do
      (vs, x') <- distinguishVar z x
      return (vs, C.Discard d x')
    C.Copy d x -> do
      (vs, x') <- distinguishVar z x
      return (vs, C.Copy d x')
    C.Unreachable ->
      return ([], term)

distinguishPrimitive :: Context m => Ident -> C.Primitive -> m ([Occurrence], C.Primitive)
distinguishPrimitive z term =
  case term of
    C.PrimOp op ds -> do
      (vss, ds') <- mapAndUnzipM (distinguishValue z) ds
      return (concat vss, C.PrimOp op ds')
    C.Magic der -> do
      case der of
        M.Cast from to value -> do
          (vs1, from') <- distinguishValue z from
          (vs2, to') <- distinguishValue z to
          (vs3, value') <- distinguishValue z value
          return (vs1 <> vs2 <> vs3, C.Magic (M.Cast from' to' value'))
        M.Store lt pointer value -> do
          (vs1, pointer') <- distinguishValue z pointer
          (vs2, value') <- distinguishValue z value
          return (vs1 <> vs2, C.Magic (M.Store lt pointer' value'))
        M.Load lt pointer -> do
          (vs, pointer') <- distinguishValue z pointer
          return (vs, C.Magic (M.Load lt pointer'))
        M.Syscall syscallNum args -> do
          (vss, args') <- mapAndUnzipM (distinguishValue z) args
          return (concat vss, C.Magic (M.Syscall syscallNum args'))
        M.External extFunName args -> do
          (vss, args') <- mapAndUnzipM (distinguishValue z) args
          return (concat vss, C.Magic (M.External extFunName args'))
