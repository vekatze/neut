module Entity.Comp.Subst (subst) where

import qualified Data.IntMap as IntMap
import Entity.Basic
import Entity.Comp
import Entity.Global

type NameEnv = IntMap.IntMap Ident

subst :: SubstValue -> NameEnv -> Comp -> IO Comp
subst =
  substComp

substComp :: SubstValue -> NameEnv -> Comp -> IO Comp
substComp sub nenv term =
  case term of
    CompPrimitive theta -> do
      let theta' = substPrimitive sub nenv theta
      return $ CompPrimitive theta'
    CompPiElimDownElim v ds -> do
      let v' = substValue sub nenv v
      let ds' = map (substValue sub nenv) ds
      return $ CompPiElimDownElim v' ds'
    CompSigmaElim b xs v e -> do
      let v' = substValue sub nenv v
      xs' <- mapM newIdentFromIdent xs
      let nenv' = IntMap.union (IntMap.fromList (zip (map asInt xs) xs')) nenv
      e' <- subst sub nenv' e
      return $ CompSigmaElim b xs' v' e'
    CompUpIntro v -> do
      let v' = substValue sub nenv v
      return $ CompUpIntro v'
    CompUpElim x e1 e2 -> do
      e1' <- subst sub nenv e1
      x' <- newIdentFromIdent x
      let nenv' = IntMap.insert (asInt x) x' nenv
      e2' <- subst sub nenv' e2
      return $ CompUpElim x' e1' e2'
    CompEnumElim v branchList -> do
      let v' = substValue sub nenv v
      let (cs, es) = unzip branchList
      es' <- mapM (subst sub nenv) es
      return $ CompEnumElim v' (zip cs es')
    CompArrayAccess primNum v index ->
      return $ CompArrayAccess primNum (substValue sub nenv v) (substValue sub nenv index)

substValue :: SubstValue -> NameEnv -> Value -> Value
substValue sub nenv term =
  case term of
    ValueVarLocal x
      | Just x' <- IntMap.lookup (asInt x) nenv ->
        ValueVarLocal x'
      | Just e <- IntMap.lookup (asInt x) sub ->
        e
      | otherwise ->
        term
    ValueVarLocalIdeal x
      | Just x' <- IntMap.lookup (asInt x) nenv ->
        ValueVarLocalIdeal x'
      | Just e <- IntMap.lookup (asInt x) sub ->
        e
      | otherwise ->
        term
    ValueVarGlobal {} ->
      term
    ValueSigmaIntro vs -> do
      let vs' = map (substValue sub nenv) vs
      ValueSigmaIntro vs'
    ValueArrayIntro elemType vs -> do
      let vs' = map (substValue sub nenv) vs
      ValueArrayIntro elemType vs'
    ValueInt {} ->
      term
    ValueFloat {} ->
      term
    ValueEnumIntro {} ->
      term

substPrimitive :: SubstValue -> NameEnv -> Primitive -> Primitive
substPrimitive sub nenv c =
  case c of
    PrimitivePrimOp op vs -> do
      let vs' = map (substValue sub nenv) vs
      PrimitivePrimOp op vs'
    PrimitiveMagic der -> do
      let der' = fmap (substValue sub nenv) der
      PrimitiveMagic der'
