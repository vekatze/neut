module Language.Comp.Move.Reduce
  ( Handle,
    new,
    reduce,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Common.Rule.Magic qualified as M
import Language.Common.Rule.Opacity qualified as O
import Language.Comp.Move.Subst qualified as Subst
import Language.Comp.Rule.Comp qualified as C
import Language.Comp.Rule.EnumCase qualified as EC

data Handle = Handle
  { substHandle :: Subst.Handle,
    gensymHandle :: Gensym.Handle,
    defMap :: C.DefMap
  }

new :: Subst.Handle -> Gensym.Handle -> C.DefMap -> Handle
new substHandle gensymHandle defMap = do
  Handle {..}

reduce :: Handle -> C.Comp -> IO C.Comp
reduce h term =
  case term of
    C.Primitive prim ->
      case prim of
        C.Magic (M.Cast _ _ value) ->
          return $ C.UpIntro value
        _ ->
          return term
    C.PiElimDownElim v ds -> do
      case v of
        C.VarGlobal x _ -> do
          case Map.lookup x (defMap h) of
            Just (O.Clear, xs, body) -> do
              let sub = IntMap.fromList (zip (map Ident.toInt xs) ds)
              Subst.subst (substHandle h) sub body >>= reduce h
            _ ->
              return term
        _ ->
          return term
    C.SigmaElim shouldDeallocate xs v e ->
      if not shouldDeallocate
        then do
          e' <- reduce h e
          return $ C.SigmaElim shouldDeallocate xs v e'
        else do
          case v of
            C.SigmaIntro ds
              | length ds == length xs -> do
                  let sub = IntMap.fromList (zip (map Ident.toInt xs) ds)
                  Subst.subst (substHandle h) sub e >>= reduce h
            _ -> do
              e' <- reduce h e
              case e' of
                C.UpIntro (C.SigmaIntro ds)
                  | Just ys <- mapM extractIdent ds,
                    xs == ys ->
                      return $ C.UpIntro v -- eta-reduce
                C.Unreachable ->
                  return C.Unreachable
                _ ->
                  case xs of
                    [] ->
                      return e'
                    _ ->
                      return $ C.SigmaElim shouldDeallocate xs v e'
    C.UpIntro _ ->
      return term
    C.UpElim isReducible x e1 e2 -> do
      e1' <- reduce h e1
      case e1' of
        C.UpIntro v
          | isReducible -> do
              let sub = IntMap.fromList [(Ident.toInt x, v)]
              Subst.subst (substHandle h) sub e2 >>= reduce h
        C.UpElim isReducible' y ey1 ey2 -> do
          y' <- Gensym.newIdentFromIdent (gensymHandle h) y
          let sub = IntMap.fromList [(Ident.toInt y, C.VarLocal y')]
          ey2' <- Subst.subst (substHandle h) sub ey2
          reduce h $ C.UpElim isReducible' y' ey1 $ C.UpElim isReducible x ey2' e2 -- commutative conversion
        C.SigmaElim b ys vy ey -> do
          ys' <- mapM (Gensym.newIdentFromIdent (gensymHandle h)) ys
          let intList = map Ident.toInt ys
          let sub = IntMap.fromList $ zip intList (map C.VarLocal ys')
          ey' <- Subst.subst (substHandle h) sub ey
          reduce h $ C.SigmaElim b ys' vy $ C.UpElim isReducible x ey' e2 -- commutative conversion
        _ -> do
          e2' <- reduce h e2
          case e2' of
            C.UpIntro (C.VarLocal y)
              | x == y,
                isReducible ->
                  return e1' -- eta-reduce
            C.Unreachable ->
              return C.Unreachable
            _ ->
              return $ C.UpElim isReducible x e1' e2'
    C.EnumElim fvInfo _ (_, defaultBranch) [] phiVarList _ cont -> do
      graftReduce h term fvInfo defaultBranch phiVarList cont
    C.EnumElim fvInfo v (defaultLabel, defaultBranch) cles phiVarList label cont -> do
      case v of
        C.Int _ l
          | Just (_, body) <- lookup (EC.Int (fromInteger l)) cles -> do
              graftReduce h term fvInfo body phiVarList cont
          | otherwise -> do
              graftReduce h term fvInfo defaultBranch phiVarList cont
        _ -> do
          let (cs, les) = unzip cles
          let (ls, es) = unzip les
          defaultBranch' <- reduce h defaultBranch
          es' <- mapM (reduce h) es
          cont' <- reduce h cont
          return $ C.EnumElim fvInfo v (defaultLabel, defaultBranch') (zip cs (zip ls es')) phiVarList label cont'
    C.Free x size cont -> do
      cont' <- reduce h cont
      return $ C.Free x size cont'
    C.Unreachable ->
      return term
    C.Phi {} ->
      return term

graftReduce ::
  Handle ->
  C.Comp ->
  [(Int, C.Value)] ->
  C.Comp ->
  [Ident] ->
  C.Comp ->
  IO C.Comp
graftReduce h origTerm fvInfo e phiVarList cont =
  case C.graft e phiVarList cont of
    Just e' -> do
      Subst.subst (substHandle h) (IntMap.fromList fvInfo) e' >>= reduce h
    Nothing ->
      return origTerm -- unreachable

extractIdent :: C.Value -> Maybe Ident
extractIdent term =
  case term of
    C.VarLocal x ->
      Just x
    _ ->
      Nothing
