module Entity.Comp.Subst (subst) where

import Context.Gensym
import qualified Data.IntMap as IntMap
import qualified Entity.Comp as C
import Entity.Ident
import qualified Entity.Ident.Reify as Ident

type NameEnv = IntMap.IntMap Ident

subst :: Context m => C.SubstValue -> NameEnv -> C.Comp -> m C.Comp
subst =
  substComp

substComp :: Context m => C.SubstValue -> NameEnv -> C.Comp -> m C.Comp
substComp sub nenv term =
  case term of
    C.Primitive theta -> do
      let theta' = substPrimitive sub nenv theta
      return $ C.Primitive theta'
    C.PiElimDownElim v ds -> do
      let v' = substValue sub nenv v
      let ds' = map (substValue sub nenv) ds
      return $ C.PiElimDownElim v' ds'
    C.SigmaElim b xs v e -> do
      let v' = substValue sub nenv v
      xs' <- mapM newIdentFromIdent xs
      let nenv' = IntMap.union (IntMap.fromList (zip (map Ident.toInt xs) xs')) nenv
      e' <- substComp sub nenv' e
      return $ C.SigmaElim b xs' v' e'
    C.UpIntro v -> do
      let v' = substValue sub nenv v
      return $ C.UpIntro v'
    C.UpIntroLocal v -> do
      let v' = substValue sub nenv v
      return $ C.UpIntroLocal v'
    C.UpElim x e1 e2 -> do
      e1' <- substComp sub nenv e1
      x' <- newIdentFromIdent x
      let nenv' = IntMap.insert (Ident.toInt x) x' nenv
      e2' <- substComp sub nenv' e2
      return $ C.UpElim x' e1' e2'
    C.EnumElim v defaultBranch branchList -> do
      let v' = substValue sub nenv v
      defaultBranch' <- substComp sub nenv defaultBranch
      let (cs, es) = unzip branchList
      es' <- mapM (substComp sub nenv) es
      return $ C.EnumElim v' defaultBranch' (zip cs es')
    C.Unreachable ->
      return term

substValue :: C.SubstValue -> NameEnv -> C.Value -> C.Value
substValue sub nenv term =
  case term of
    C.VarLocal x
      | Just x' <- IntMap.lookup (Ident.toInt x) nenv ->
          C.VarLocal x'
      | Just e <- IntMap.lookup (Ident.toInt x) sub ->
          e
      | otherwise ->
          term
    C.VarGlobal {} ->
      term
    C.SigmaIntro vs -> do
      let vs' = map (substValue sub nenv) vs
      C.SigmaIntro vs'
    C.Int {} ->
      term
    C.Float {} ->
      term

substPrimitive :: C.SubstValue -> NameEnv -> C.Primitive -> C.Primitive
substPrimitive sub nenv c =
  case c of
    C.PrimOp op vs -> do
      let vs' = map (substValue sub nenv) vs
      C.PrimOp op vs'
    C.Magic der -> do
      let der' = fmap (substValue sub nenv) der
      C.Magic der'
