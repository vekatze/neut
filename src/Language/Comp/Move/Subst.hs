module Language.Comp.Move.Subst
  ( Handle,
    new,
    subst,
  )
where

import Data.IntMap qualified as IntMap
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Comp.Rule.Comp qualified as C
import Library.Gensym.Rule.Handle qualified as Gensym

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

subst :: Handle -> C.SubstValue -> C.Comp -> IO C.Comp
subst =
  substComp

substComp :: Handle -> C.SubstValue -> C.Comp -> IO C.Comp
substComp h sub term =
  case term of
    C.PiElimDownElim v ds -> do
      let v' = substValue sub v
      let ds' = map (substValue sub) ds
      return $ C.PiElimDownElim v' ds'
    C.SigmaElim b xs v e -> do
      let v' = substValue sub v
      xs' <- mapM (Gensym.newIdentFromIdent (gensymHandle h)) xs
      let sub' = IntMap.union (IntMap.fromList (zip (map Ident.toInt xs) (map C.VarLocal xs'))) sub
      e' <- substComp h sub' e
      return $ C.SigmaElim b xs' v' e'
    C.UpIntro v -> do
      let v' = substValue sub v
      return $ C.UpIntro v'
    C.UpElim isReducible x e1 e2 -> do
      e1' <- substComp h sub e1
      x' <- Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (C.VarLocal x') sub
      e2' <- substComp h sub' e2
      return $ C.UpElim isReducible x' e1' e2'
    C.EnumElim fvInfo v defaultBranch branchList -> do
      let (is, ds) = unzip fvInfo
      let ds' = map (substValue sub) ds
      let v' = substValue sub v
      return $ C.EnumElim (zip is ds') v' defaultBranch branchList
    C.Primitive theta -> do
      let theta' = substPrimitive sub theta
      return $ C.Primitive theta'
    C.Free x size cont -> do
      let x' = substValue sub x
      cont' <- substComp h sub cont
      return $ C.Free x' size cont'
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
    C.VarStaticText {} ->
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
