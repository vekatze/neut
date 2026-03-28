module Language.Comp.Subst
  ( Handle,
    new,
    subst,
    substValue,
    substPrimitive,
  )
where

import Data.IntMap qualified as IntMap
import Gensym.Handle qualified as Gensym
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.Ident.Reify qualified as Ident
import Language.Comp.Comp qualified as C

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
    C.PiElimDownElim forceInline v ds -> do
      let v' = substValue sub v
      let ds' = map (substValue sub) ds
      return $ C.PiElimDownElim forceInline v' ds'
    C.SigmaElim b xs v e -> do
      let v' = substValue sub v
      xs' <- mapM (Gensym.newIdentFromIdent (gensymHandle h)) xs
      let sub' = IntMap.union (IntMap.fromList (zip (map Ident.toInt xs) (map C.VarLocal xs'))) sub
      e' <- substComp h sub' e
      return $ C.SigmaElim b xs' v' e'
    C.UpIntro v -> do
      let v' = substValue sub v
      return $ C.UpIntro v'
    C.UpIntroVoid ->
      return C.UpIntroVoid
    C.UpElim isReducible x e1 e2 -> do
      e1' <- substComp h sub e1
      x' <- Gensym.newIdentFromIdent (gensymHandle h) x
      let sub' = IntMap.insert (Ident.toInt x) (C.VarLocal x') sub
      e2' <- substComp h sub' e2
      return $ C.UpElim isReducible x' e1' e2'
    C.UpElimCallVoid f vs e2 -> do
      let f' = substValue sub f
      let vs' = map (substValue sub) vs
      e2' <- substComp h sub e2
      return $ C.UpElimCallVoid f' vs' e2'
    C.EnumElim fvInfo v defaultBranch branchList -> do
      let (is, ds) = unzip fvInfo
      let ds' = map (substValue sub) ds
      let sub' = foldr IntMap.delete sub is
      let v' = substValue sub v
      defaultBranch' <- substComp h sub' defaultBranch
      branchList' <-
        mapM
          (\(tag, branch) -> do
             branch' <- substComp h sub' branch
             return (tag, branch'))
          branchList
      return $ C.EnumElim (zip is ds') v' defaultBranch' branchList'
    C.DestCall sizeComp f vs -> do
      sizeComp' <- substComp h sub sizeComp
      let f' = substValue sub f
      let vs' = map (substValue sub) vs
      return $ C.DestCall sizeComp' f' vs'
    C.WriteToDest dest sizeComp result cont -> do
      let dest' = substValue sub dest
      sizeComp' <- substComp h sub sizeComp
      result' <- substComp h sub result
      cont' <- substComp h sub cont
      return $ C.WriteToDest dest' sizeComp' result' cont'
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
    C.SigmaIntro size vs -> do
      let vs' = map (substValue sub) vs
      C.SigmaIntro size vs'
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
    C.ShiftPointer v size index ->
      C.ShiftPointer (substValue sub v) size index
    C.Calloc num size ->
      C.Calloc (substValue sub num) (substValue sub size)
    C.Alloc size ->
      C.Alloc (substValue sub size)
    C.Realloc ptr size ->
      C.Realloc (substValue sub ptr) (substValue sub size)
    C.Memcpy dest src size ->
      C.Memcpy (substValue sub dest) (substValue sub src) (substValue sub size)
    C.Magic der -> do
      let der' = fmap (substValue sub) der
      C.Magic der'
