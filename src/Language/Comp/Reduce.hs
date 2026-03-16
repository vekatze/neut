module Language.Comp.Reduce
  ( Handle,
    new,
    reduce,
  )
where

import Control.Monad (forM)
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Gensym.Handle qualified as Gensym
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LowMagic qualified as LM
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
    C.PiElimDownElim forceInline v ds -> do
      let v' = Subst.substValue (subst h) v
      let ds' = map (Subst.substValue (subst h)) ds
      case v' of
        C.VarGlobal x _ _ -> do
          case Map.lookup x (defMap h) of
            Just (opacity, xs, body)
              | forceInline || opacity == O.Clear -> do
                  let sub = IntMap.fromList (zip (map Ident.toInt xs) ds')
                  reduce (unionSubst h sub) body
            _ ->
              return $ C.PiElimDownElim forceInline v' ds'
        _ ->
          return $ C.PiElimDownElim forceInline v' ds'
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
            C.SigmaDataIntro _ ds
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
                C.UpIntro (C.SigmaDataIntro _ ds)
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
    C.UpIntroVoid -> do
      return C.UpIntroVoid
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
          reduce h $ C.UpElim isReducible' y ey1 $ C.UpElim isReducible x ey2 e2'
        C.SigmaElim b ys vy ey -> do
          -- commutative conversion
          e2' <- reduce h e2
          reduce h $ C.SigmaElim b ys vy $ C.UpElim isReducible x ey e2'
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
    C.UpElimCallVoid f vs e2 -> do
      let f' = Subst.substValue (subst h) f
      let vs' = map (Subst.substValue (subst h)) vs
      case f' of
        C.VarGlobal x _ _ -> do
          case Map.lookup x (defMap h) of
            Just (opacity, xs, body)
              | opacity == O.Clear -> do
                  let sub = IntMap.fromList (zip (map Ident.toInt xs) vs')
                  body' <- reduce (unionSubst h sub) body
                  graftVoidReduce h body' e2
            _ -> do
              e2' <- reduce h e2
              case e2' of
                C.Unreachable ->
                  return C.Unreachable
                _ ->
                  return $ C.UpElimCallVoid f' vs' e2'
        _ -> do
          e2' <- reduce h e2
          case e2' of
            C.Unreachable ->
              return C.Unreachable
            _ ->
              return $ C.UpElimCallVoid f' vs' e2'
    C.EnumElim kind fvInfo v defaultBranch [] phiVarList cont -> do
      let fvInfo' = substFvInfo h fvInfo
      let v' = Subst.substValue (subst h) v
      let term' = C.EnumElim kind fvInfo' v' defaultBranch [] phiVarList cont
      graftReduce h term' fvInfo' defaultBranch phiVarList cont
    C.EnumElim kind fvInfo v defaultBranch ces phiVarList cont -> do
      let fvInfo' = substFvInfo h fvInfo
      let v' = Subst.substValue (subst h) v
      let term' = C.EnumElim kind fvInfo' v' defaultBranch [] phiVarList cont
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
          return $ C.EnumElim kind fvInfo' v' defaultBranch' (zip cs es') phiVarList' cont'
    C.DestCall sizeComp f vs -> do
      sizeComp' <- reduce h sizeComp
      let f' = Subst.substValue (subst h) f
      let vs' = map (Subst.substValue (subst h)) vs
      return $ C.DestCall sizeComp' f' vs'
    C.WriteToDest dest sizeComp result cont -> do
      let dest' = Subst.substValue (subst h) dest
      sizeComp' <- reduce h sizeComp
      result' <- reduce h result
      cont' <- reduce h cont
      case result' of
        C.DestCall _ f args ->
          reduce h $ C.UpElimCallVoid f (dest' : args) cont'
        C.UpElim flag x e1 e2 ->
          reduce h $ C.UpElim flag x e1 (C.WriteToDest dest' sizeComp' e2 cont')
        C.UpElimCallVoid f vs e ->
          reduce h $ C.UpElimCallVoid f vs (C.WriteToDest dest' sizeComp' e cont')
        C.SigmaElim shouldDeallocate ys v e ->
          reduce h $ C.SigmaElim shouldDeallocate ys v (C.WriteToDest dest' sizeComp' e cont')
        C.Free x size e ->
          reduce h $ C.Free x size (C.WriteToDest dest' sizeComp' e cont')
        C.EnumElim C.CanonicalJoin fvInfo disc defaultBranch caseList phiVarList enumCont -> do
          mDefaultBranch' <- rewriteWriteToDestBranch dest' sizeComp' defaultBranch
          caseList' <-
            forM caseList $ \(tag, branch) -> do
              mBranch' <- rewriteWriteToDestBranch dest' sizeComp' branch
              return $ fmap (\branch' -> (tag, branch')) mBranch'
          case (mDefaultBranch', sequence caseList') of
            (Just defaultBranch', Just caseList'') -> do
              ignoredVar <- Gensym.newIdentFromText (gensymHandle h) "_"
              reduce h $
                C.UpElim
                  True
                  ignoredVar
                  (C.EnumElim C.CanonicalJoin fvInfo disc defaultBranch' caseList'' phiVarList enumCont)
                  cont'
            _ ->
              return $ C.WriteToDest dest' sizeComp' result' cont'
        C.Unreachable ->
          return C.Unreachable
        _ ->
          return $ C.WriteToDest dest' sizeComp' result' cont'
    C.Primitive prim -> do
      case prim of
        C.Magic (LM.Cast _ _ value) ->
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

rewriteWriteToDestBranch ::
  C.Value ->
  C.Comp ->
  C.Comp ->
  IO (Maybe C.Comp)
rewriteWriteToDestBranch dest sizeComp branch =
  case branch of
    C.UpElim True phiVar body (C.Phi [C.VarLocal phiVar'])
      | phiVar == phiVar' ->
          return . Just $ C.WriteToDest dest sizeComp body (C.Phi [C.null])
    C.Phi [v] ->
      return . Just $ C.WriteToDest dest sizeComp (C.UpIntro v) (C.Phi [C.null])
    C.UpElim flag x e1 e2 -> do
      mBranch' <- rewriteWriteToDestBranch dest sizeComp e2
      return $ C.UpElim flag x e1 <$> mBranch'
    C.UpElimCallVoid f vs e -> do
      mBranch' <- rewriteWriteToDestBranch dest sizeComp e
      return $ C.UpElimCallVoid f vs <$> mBranch'
    C.SigmaElim shouldDeallocate ys v e -> do
      mBranch' <- rewriteWriteToDestBranch dest sizeComp e
      return $ C.SigmaElim shouldDeallocate ys v <$> mBranch'
    C.Free v size e -> do
      mBranch' <- rewriteWriteToDestBranch dest sizeComp e
      return $ C.Free v size <$> mBranch'
    _ ->
      return Nothing

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

graftVoidReduce :: Handle -> C.Comp -> C.Comp -> IO C.Comp
graftVoidReduce h e cont = do
  case graftVoid e cont of
    Just e' ->
      reduce h e'
    Nothing ->
      return e

graftVoid :: C.Comp -> C.Comp -> Maybe C.Comp
graftVoid e cont =
  case e of
    C.UpIntroVoid ->
      return cont
    C.SigmaElim flag ys v e2 -> do
      e2' <- graftVoid e2 cont
      return $ C.SigmaElim flag ys v e2'
    C.UpElim flag x e1 e2 -> do
      e2' <- graftVoid e2 cont
      return $ C.UpElim flag x e1 e2'
    C.UpElimCallVoid f vs e2 -> do
      e2' <- graftVoid e2 cont
      return $ C.UpElimCallVoid f vs e2'
    C.EnumElim kind fvInfo v defaultBranch branchList ys e2 -> do
      e2' <- graftVoid e2 cont
      return $ C.EnumElim kind fvInfo v defaultBranch branchList ys e2'
    C.DestCall {} ->
      Nothing
    C.WriteToDest dest sizeComp result e2 -> do
      e2' <- graftVoid e2 cont
      return $ C.WriteToDest dest sizeComp result e2'
    C.Free v size e2 -> do
      e2' <- graftVoid e2 cont
      return $ C.Free v size e2'
    C.Unreachable ->
      return C.Unreachable
    _ ->
      Nothing

extractIdent :: C.Value -> Maybe Ident
extractIdent term =
  case term of
    C.VarLocal x ->
      Just x
    _ ->
      Nothing
