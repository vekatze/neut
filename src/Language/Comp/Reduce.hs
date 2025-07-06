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
    defMap :: C.DefMap,
    subst :: C.SubstValue
  }

new :: Subst.Handle -> Gensym.Handle -> C.DefMap -> Handle
new substHandle gensymHandle defMap = do
  let subst = IntMap.empty
  Handle {..}

unionSubst :: Handle -> C.SubstValue -> Handle
unionSubst (Handle {..}) newSubst = do
  let subst' = IntMap.union newSubst subst
  Handle {subst = subst', ..}

reduce :: Handle -> C.Comp -> IO C.Comp
reduce h term = do
  case term of
    C.PiElimDownElim v ds -> do
      let v' = Subst.substValue (subst h) v
      let ds' = map (Subst.substValue (subst h)) ds
      case v' of
        C.VarGlobal x _ -> do
          case Map.lookup x (defMap h) of
            Just (O.Clear, xs, body) -> do
              let sub = IntMap.fromList (zip (map Ident.toInt xs) ds')
              reduce (unionSubst h sub) body
            _ ->
              return $ C.PiElimDownElim v' ds'
        _ ->
          return $ C.PiElimDownElim v' ds'
    C.SigmaElim shouldDeallocate xs v e -> do
      let v' = Subst.substValue (subst h) v
      if not shouldDeallocate
        then do
          xs' <- mapM (Gensym.newIdentFromIdent (gensymHandle h)) xs
          let h' = unionSubst h (IntMap.fromList (zip (map Ident.toInt xs) (map C.VarLocal xs')))
          e' <- reduce h' e
          case e' of
            C.Unreachable ->
              return C.Unreachable
            _ ->
              return $ C.SigmaElim shouldDeallocate xs' v' e'
        else do
          case v' of
            C.SigmaIntro ds
              | length ds == length xs -> do
                  let h' = unionSubst h (IntMap.fromList (zip (map Ident.toInt xs) ds))
                  reduce h' e
            _ -> do
              xs' <- mapM (Gensym.newIdentFromIdent (gensymHandle h)) xs
              let h' = unionSubst h (IntMap.fromList (zip (map Ident.toInt xs) (map C.VarLocal xs')))
              e' <- reduce h' e
              case e' of
                C.UpIntro (C.SigmaIntro ds)
                  | Just ys <- mapM extractIdent ds,
                    xs' == ys ->
                      return $ C.UpIntro v -- eta-reduce
                C.Unreachable ->
                  return C.Unreachable
                _ ->
                  case xs' of
                    [] ->
                      return e'
                    _ ->
                      return $ C.SigmaElim shouldDeallocate xs' v' e'
    C.UpIntro d -> do
      return $ C.UpIntro $ Subst.substValue (subst h) d
    C.UpElim isReducible x e1 e2 -> do
      e1' <- reduce h e1
      case e1' of
        C.UpIntro v
          | isReducible -> do
              let h' = unionSubst h $ IntMap.singleton (Ident.toInt x) v
              reduce h' e2
        C.UpElim isReducible' y ey1 ey2 -> do
          -- commutative conversion
          e2' <- reduce h e2
          return $ C.UpElim isReducible' y ey1 $ C.UpElim isReducible x ey2 e2'
        C.SigmaElim b ys vy ey -> do
          -- commutative conversion
          e2' <- reduce h e2
          return $ C.SigmaElim b ys vy $ C.UpElim isReducible x ey e2'
        C.Unreachable ->
          return C.Unreachable
        _ -> do
          x' <- Gensym.newIdentFromIdent (gensymHandle h) x
          let h' = unionSubst h $ IntMap.singleton (Ident.toInt x) (C.VarLocal x')
          e2' <- reduce h' e2
          case e2' of
            C.Unreachable ->
              return C.Unreachable
            C.UpIntro (C.VarLocal y)
              | x' == y,
                isReducible ->
                  return e1' -- eta-reduce
            _ ->
              return $ C.UpElim isReducible x' e1' e2'
    C.EnumElim fvInfo v defaultBranch [] phiVarList cont -> do
      let fvInfo' = substFvInfo h fvInfo
      let v' = Subst.substValue (subst h) v
      let term' = C.EnumElim fvInfo' v' defaultBranch [] phiVarList cont
      graftReduce h term' fvInfo' defaultBranch phiVarList cont
    C.EnumElim fvInfo v defaultBranch ces phiVarList cont -> do
      let fvInfo' = substFvInfo h fvInfo
      let v' = Subst.substValue (subst h) v
      let term' = C.EnumElim fvInfo' v' defaultBranch [] phiVarList cont
      case v' of
        C.Int _ l
          | Just body <- lookup (EC.Int (fromInteger l)) ces -> do
              graftReduce h term' fvInfo' body phiVarList cont
          | otherwise -> do
              graftReduce h term' fvInfo' defaultBranch phiVarList cont
        _ -> do
          let (cs, es) = unzip ces
          defaultBranch' <- reduce h defaultBranch
          es' <- mapM (reduce h) es
          phiVarList' <- mapM (Gensym.newIdentFromIdent (gensymHandle h)) phiVarList
          let h' = unionSubst h $ IntMap.fromList (zip (map Ident.toInt phiVarList) (map C.VarLocal phiVarList'))
          cont' <- reduce h' cont
          return $ C.EnumElim fvInfo' v' defaultBranch' (zip cs es') phiVarList' cont'
    C.Primitive prim -> do
      case prim of
        C.Magic (M.Cast _ _ value) ->
          return $ C.UpIntro $ Subst.substValue (subst h) value
        _ ->
          return $ C.Primitive $ Subst.substPrimitive (subst h) prim
    C.Free x size cont -> do
      let x' = Subst.substValue (subst h) x
      cont' <- reduce h cont
      case cont' of
        C.Unreachable ->
          return C.Unreachable
        _ ->
          return $ C.Free x' size cont'
    C.Unreachable -> do
      return C.Unreachable
    C.Phi ds -> do
      return $ C.Phi $ map (Subst.substValue (subst h)) ds

substFvInfo :: Handle -> [(Int, C.Value)] -> [(Int, C.Value)]
substFvInfo h fvInfo = do
  let (is, ds) = unzip fvInfo
  let ds' = map (Subst.substValue (subst h)) ds
  zip is ds'

graftReduce ::
  Handle ->
  C.Comp ->
  [(Int, C.Value)] ->
  C.Comp ->
  [Ident] ->
  C.Comp ->
  IO C.Comp
graftReduce h origTerm fvInfo e phiVarList cont = do
  case C.graft e phiVarList cont of
    Just e' -> do
      let h' = unionSubst h $ IntMap.fromList fvInfo
      reduce h' e'
    Nothing ->
      return origTerm -- unreachable

extractIdent :: C.Value -> Maybe Ident
extractIdent term =
  case term of
    C.VarLocal x ->
      Just x
    _ ->
      Nothing
