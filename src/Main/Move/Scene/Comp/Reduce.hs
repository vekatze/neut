module Main.Move.Scene.Comp.Reduce
  ( Handle,
    new,
    reduce,
  )
where

import Data.IntMap qualified as IntMap
import Main.Move.Language.Utility.Gensym qualified as Gensym
import Main.Move.Scene.Clarify.Handle.CompDef qualified as CompDef
import Main.Move.Scene.Comp.Subst qualified as Subst
import Main.Rule.Comp qualified as C
import Main.Rule.EnumCase qualified as EC
import Main.Rule.Ident
import Main.Rule.Ident.Reify qualified as Ident
import Main.Rule.Magic qualified as M
import Main.Rule.Opacity qualified as O

data Handle
  = Handle
  { compDefHandle :: CompDef.Handle,
    substHandle :: Subst.Handle,
    gensymHandle :: Gensym.Handle
  }

new :: CompDef.Handle -> Subst.Handle -> Gensym.Handle -> Handle
new compDefHandle substHandle gensymHandle = do
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
          mDefValue <- CompDef.lookup (compDefHandle h) x
          case mDefValue of
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
    C.EnumElim fvInfo _ defaultBranch [] -> do
      Subst.subst (substHandle h) (IntMap.fromList fvInfo) defaultBranch >>= reduce h
    C.EnumElim fvInfo v defaultBranch les -> do
      case v of
        C.Int _ l
          | Just body <- lookup (EC.Int (fromInteger l)) les -> do
              Subst.subst (substHandle h) (IntMap.fromList fvInfo) body >>= reduce h
          | otherwise -> do
              Subst.subst (substHandle h) (IntMap.fromList fvInfo) defaultBranch >>= reduce h
        _ -> do
          let (ls, es) = unzip les
          defaultBranch' <- reduce h defaultBranch
          es' <- mapM (reduce h) es
          return $ C.EnumElim fvInfo v defaultBranch' (zip ls es')
    C.Free x size cont -> do
      cont' <- reduce h cont
      return $ C.Free x size cont'
    C.Unreachable ->
      return term

extractIdent :: C.Value -> Maybe Ident
extractIdent term =
  case term of
    C.VarLocal x ->
      Just x
    _ ->
      Nothing
