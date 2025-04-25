module Move.Scene.Comp.Reduce
  ( reduce,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.CompDefinition qualified as CompDefinition
import Move.Context.Gensym qualified as Gensym
import Move.Scene.Comp.Subst qualified as Subst
import Rule.Comp qualified as C
import Rule.EnumCase qualified as EC
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Magic qualified as M
import Rule.Opacity qualified as O

reduce :: C.Comp -> App C.Comp
reduce term =
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
          mDefValue <- CompDefinition.lookup x
          case mDefValue of
            Just (O.Clear, xs, body) -> do
              let sub = IntMap.fromList (zip (map Ident.toInt xs) ds)
              h <- Subst.new
              liftIO (Subst.subst h sub body) >>= reduce
            _ ->
              return term
        _ ->
          return term
    C.SigmaElim shouldDeallocate xs v e ->
      if not shouldDeallocate
        then do
          e' <- reduce e
          return $ C.SigmaElim shouldDeallocate xs v e'
        else do
          case v of
            C.SigmaIntro ds
              | length ds == length xs -> do
                  let sub = IntMap.fromList (zip (map Ident.toInt xs) ds)
                  h <- Subst.new
                  liftIO (Subst.subst h sub e) >>= reduce
            _ -> do
              e' <- reduce e
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
      e1' <- reduce e1
      case e1' of
        C.UpIntro v
          | isReducible -> do
              let sub = IntMap.fromList [(Ident.toInt x, v)]
              h <- Subst.new
              liftIO (Subst.subst h sub e2) >>= reduce
        C.UpElim isReducible' y ey1 ey2 -> do
          y' <- Gensym.newIdentFromIdent y
          let sub = IntMap.fromList [(Ident.toInt y, C.VarLocal y')]
          h <- Subst.new
          ey2' <- liftIO $ Subst.subst h sub ey2
          reduce $ C.UpElim isReducible' y' ey1 $ C.UpElim isReducible x ey2' e2 -- commutative conversion
        C.SigmaElim b ys vy ey -> do
          ys' <- mapM Gensym.newIdentFromIdent ys
          let intList = map Ident.toInt ys
          let sub = IntMap.fromList $ zip intList (map C.VarLocal ys')
          h <- Subst.new
          ey' <- liftIO $ Subst.subst h sub ey
          reduce $ C.SigmaElim b ys' vy $ C.UpElim isReducible x ey' e2 -- commutative conversion
        _ -> do
          e2' <- reduce e2
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
      h <- Subst.new
      liftIO (Subst.subst h (IntMap.fromList fvInfo) defaultBranch) >>= reduce
    C.EnumElim fvInfo v defaultBranch les -> do
      case v of
        C.Int _ l
          | Just body <- lookup (EC.Int (fromInteger l)) les -> do
              h <- Subst.new
              liftIO (Subst.subst h (IntMap.fromList fvInfo) body) >>= reduce
          | otherwise -> do
              h <- Subst.new
              liftIO (Subst.subst h (IntMap.fromList fvInfo) defaultBranch) >>= reduce
        _ -> do
          let (ls, es) = unzip les
          defaultBranch' <- reduce defaultBranch
          es' <- mapM reduce es
          return $ C.EnumElim fvInfo v defaultBranch' (zip ls es')
    C.Free x size cont -> do
      cont' <- reduce cont
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
