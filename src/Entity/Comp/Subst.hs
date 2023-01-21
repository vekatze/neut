module Entity.Comp.Subst (subst) where

import Context.Gensym
import Data.IntMap qualified as IntMap
import Entity.Comp qualified as C
import Entity.Ident.Reify qualified as Ident

subst :: Context m => C.SubstValue -> C.Comp -> m C.Comp
subst =
  substComp

substComp :: Context m => C.SubstValue -> C.Comp -> m C.Comp
substComp sub term =
  case term of
    C.Primitive theta -> do
      let theta' = substPrimitive sub theta
      return $ C.Primitive theta'
    C.PiElimDownElim v ds -> do
      let v' = substValue sub v
      let ds' = map (substValue sub) ds
      return $ C.PiElimDownElim v' ds'
    C.SigmaElim b xs v e -> do
      let v' = substValue sub v
      xs' <- mapM newIdentFromIdent xs
      let sub' = IntMap.union (IntMap.fromList (zip (map Ident.toInt xs) (map C.VarLocal xs'))) sub
      e' <- substComp sub' e
      return $ C.SigmaElim b xs' v' e'
    C.UpIntro v -> do
      let v' = substValue sub v
      return $ C.UpIntro v'
    C.UpElim isReducible x e1 e2 -> do
      e1' <- substComp sub e1
      x' <- newIdentFromIdent x
      let sub' = IntMap.insert (Ident.toInt x) (C.VarLocal x') sub
      e2' <- substComp sub' e2
      return $ C.UpElim isReducible x' e1' e2'
    C.EnumElim v defaultBranch branchList -> do
      let v' = substValue sub v
      defaultBranch' <- substComp sub defaultBranch
      let (cs, es) = unzip branchList
      es' <- mapM (substComp sub) es
      return $ C.EnumElim v' defaultBranch' (zip cs es')
    C.Unreachable ->
      return term

substValue :: C.SubstValue -> C.Value -> C.Value
substValue sub term =
  case term of
    C.VarLocal x
      | Just e <- IntMap.lookup (Ident.toInt x) sub ->
          e
      | otherwise ->
          term
    C.VarGlobal {} ->
      term
    C.SigmaIntro vs -> do
      let vs' = map (substValue sub) vs
      C.SigmaIntro vs'
    C.Int {} ->
      term
    C.Float {} ->
      term

substPrimitive :: C.SubstValue -> C.Primitive -> C.Primitive
substPrimitive sub c =
  case c of
    C.PrimOp op vs -> do
      let vs' = map (substValue sub) vs
      C.PrimOp op vs'
    C.Magic der -> do
      let der' = fmap (substValue sub) der
      C.Magic der'
