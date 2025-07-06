module Language.Comp.Reduce
  ( Handle,
    new,
    reduce,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Gensym.Handle qualified as Gensym
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.Magic qualified as M
import Language.Common.Opacity qualified as O
import Language.Comp.Comp qualified as C
import Language.Comp.EnumCase qualified as EC
import Language.Comp.Subst qualified as Subst

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
          case e' of
            C.Unreachable ->
              return C.Unreachable
            _ ->
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
        C.Unreachable ->
          return C.Unreachable
        _ -> do
          e2' <- reduce h e2
          case e2' of
            C.Unreachable ->
              return C.Unreachable
            C.UpIntro (C.VarLocal y)
              | x == y,
                isReducible ->
                  return e1' -- eta-reduce
            _ ->
              return $ C.UpElim isReducible x e1' e2'
    C.EnumElim fvInfo _ defaultBranch [] phiVarList cont -> do
      graftReduce h term fvInfo defaultBranch phiVarList cont
    C.EnumElim fvInfo v defaultBranch ces phiVarList cont -> do
      case v of
        C.Int _ l
          | Just body <- lookup (EC.Int (fromInteger l)) ces -> do
              graftReduce h term fvInfo body phiVarList cont
          | otherwise -> do
              graftReduce h term fvInfo defaultBranch phiVarList cont
        _ -> do
          let (cs, es) = unzip ces
          defaultBranch' <- reduce h defaultBranch
          es' <- mapM (reduce h) es
          cont' <- reduce h cont
          return $ C.EnumElim fvInfo v defaultBranch' (zip cs es') phiVarList cont'
    C.Free x size cont -> do
      cont' <- reduce h cont
      case cont' of
        C.Unreachable ->
          return C.Unreachable
        _ ->
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
