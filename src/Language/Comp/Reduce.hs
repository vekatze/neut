module Language.Comp.Reduce
  ( Handle,
    new,
    reduce,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.List (foldl')
import Gensym.Handle qualified as Gensym
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.Opacity qualified as O
import Language.Common.CellLayout qualified as CL
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
  let subst' = IntMap.foldlWithKey' insertSubst subst newSubst
  Handle {subst = subst', ..}

insertSubst :: C.SubstValue -> Int -> C.Value -> C.SubstValue
insertSubst currentSubst ident value = do
  IntMap.insert ident value currentSubst

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
                  instantiate h xs ds' body
            _ ->
              return $ C.PiElimDownElim forceInline v' ds'
        _ ->
          return $ C.PiElimDownElim forceInline v' ds'
    C.SigmaElim shouldDeallocate slotIndex layout xs v e -> do
      let v' = Subst.substValue (subst h) v
      reduceSigmaElim h shouldDeallocate slotIndex layout xs v' e
    C.UpIntro d -> do
      return $ C.UpIntro $ Subst.substValue (subst h) d
    C.UpElim isReducible x e1 e2 -> do
      e1' <- reduce h e1
      reduceUpElim h isReducible x e1' e2
    C.EnumElim fvInfo _ defaultBranch [] -> do
      let fvInfo' = substFvInfo h fvInfo
      reduce (unionSubst h $ IntMap.fromList fvInfo') defaultBranch
    C.EnumElim fvInfo v defaultBranch ces -> do
      let fvInfo' = substFvInfo h fvInfo
      let v' = Subst.substValue (subst h) v
      case valueToEnumInt v' of
        Just l
          | Just body <- lookup (EC.Int (fromInteger l)) ces -> do
              reduce (unionSubst h $ IntMap.fromList fvInfo') body
          | otherwise -> do
              reduce (unionSubst h $ IntMap.fromList fvInfo') defaultBranch
        _ -> do
          let (cs, es) = unzip ces
          defaultBranch' <- reduce h defaultBranch
          es' <- mapM (reduce h) es
          return $ C.EnumElim fvInfo' v' defaultBranch' (zip cs es')
    C.OutputProvide dest sizeComp result -> do
      let dest' = Subst.substValue (subst h) dest
      sizeComp' <- reduce h sizeComp
      result' <- reduce h result
      reduceOutputProvide h dest' sizeComp' result'
    C.OutputRequest sizeComp f vs -> do
      sizeComp' <- reduce h sizeComp
      let f' = Subst.substValue (subst h) f
      let vs' = map (Subst.substValue (subst h)) vs
      return $ C.OutputRequest sizeComp' f' vs'
    C.Primitive prim -> do
      case prim of
        C.Magic (LM.Cast _ _ value) ->
          return $ C.UpIntro $ Subst.substValue (subst h) value
        C.Magic (LM.CallType func arg1 arg2 arg3) -> do
          reduce h $ C.PiElimDownElim True func [arg1, arg2, arg3]
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

reduceSigmaElim :: Handle -> Bool -> Int -> CL.CellLayout -> [Ident] -> C.Value -> C.Comp -> IO C.Comp
reduceSigmaElim h shouldDeallocate slotIndex layout xs v e = do
  case v of
    C.SigmaIntro introLayout ds
      | introLayout == layout,
        length ds >= slotIndex + length xs -> do
          let ds' = take (length xs) $ drop slotIndex ds
          let h' = unionSubst h (IntMap.fromList (zip (map Ident.toInt xs) ds'))
          reduce h' e
    _ -> do
      let h' = deleteSubstList h xs
      e' <- reduce h' e
      case e' of
        C.UpIntro (C.SigmaIntro introLayout ds)
          | introLayout == layout,
            slotIndex == 0,
            length xs == length (CL.cellSlots layout),
            Just ys <- mapM extractIdent ds,
            xs == ys ->
              return $ C.UpIntro v
        C.Unreachable ->
          return C.Unreachable
        _ -> do
          case xs of
            [] ->
              return e'
            _ ->
              return $ C.SigmaElim shouldDeallocate slotIndex layout xs v e'

reduceOutputProvide :: Handle -> C.Value -> C.Comp -> C.Comp -> IO C.Comp
reduceOutputProvide h dest sizeComp result = do
  case result of
    C.OutputRequest _ f args ->
      reduce h $ C.PiElimDownElim False f (dest : args)
    C.UpElim flag x e1 e2 ->
      reduce h $ C.UpElim flag x e1 (C.OutputProvide dest sizeComp e2)
    C.SigmaElim shouldDeallocate slotIndex layout ys v e ->
      reduce h $ C.SigmaElim shouldDeallocate slotIndex layout ys v (C.OutputProvide dest sizeComp e)
    C.Free x size e ->
      reduce h $ C.Free x size (C.OutputProvide dest sizeComp e)
    C.EnumElim fvInfo disc defaultBranch caseList -> do
      let wrap branch = C.OutputProvide dest sizeComp branch
      let rewritten =
            C.EnumElim
              fvInfo
              disc
              (wrap defaultBranch)
              (map (fmap wrap) caseList)
      refreshed <- Subst.refresh (substHandle h) rewritten
      reduce h refreshed
    C.Unreachable ->
      return C.Unreachable
    _ ->
      return $ C.OutputProvide dest sizeComp result

reduceUpElim :: Handle -> C.IsReducible -> Ident -> C.Comp -> C.Comp -> IO C.Comp
reduceUpElim h isReducible x e1 e2 = do
  case e1 of
    C.UpIntro v
      | isReducible -> do
          let h' = unionSubst h $ IntMap.singleton (Ident.toInt x) v
          reduce h' e2
    C.UpElim isReducible' y ey1 ey2 -> do
      e2' <- reduce h e2
      reduceUpElim h isReducible' y ey1 $ C.UpElim isReducible x ey2 e2'
    C.SigmaElim shouldDeallocate slotIndex layout ys vy ey -> do
      e2' <- reduce h e2
      reduceSigmaElim h shouldDeallocate slotIndex layout ys vy $ C.UpElim isReducible x ey e2'
    C.Unreachable ->
      return C.Unreachable
    _ -> do
      let h' = deleteSubst h x
      e2' <- reduce h' e2
      case e2' of
        C.Unreachable ->
          return C.Unreachable
        C.UpIntro (C.VarLocal y)
          | x == y,
            isReducible ->
              return e1
        _ ->
          return $ C.UpElim isReducible x e1 e2'

deleteSubst :: Handle -> Ident -> Handle
deleteSubst h ident = do
  let subst' = IntMap.delete (Ident.toInt ident) (subst h)
  h {subst = subst'}

deleteSubstList :: Handle -> [Ident] -> Handle
deleteSubstList h identList = do
  let subst' = foldl' deleteSubstByIdent (subst h) identList
  h {subst = subst'}

deleteSubstByIdent :: C.SubstValue -> Ident -> C.SubstValue
deleteSubstByIdent currentSubst ident = do
  IntMap.delete (Ident.toInt ident) currentSubst

substFvInfo :: Handle -> [(Int, C.Value)] -> [(Int, C.Value)]
substFvInfo h fvInfo = do
  let (is, ds) = unzip fvInfo
  let ds' = map (Subst.substValue (subst h)) ds
  zip is ds'

valueToEnumInt :: C.Value -> Maybe Integer
valueToEnumInt value = do
  case value of
    C.Int _ l ->
      Just l
    C.SigmaIntro _ [] ->
      Just 0
    _ ->
      Nothing

instantiate :: Handle -> [Ident] -> [C.Value] -> C.Comp -> IO C.Comp
instantiate h xs values body = do
  let formalSubst = IntMap.fromList $ zip (map Ident.toInt xs) values
  let fullSubst = IntMap.union formalSubst (subst h)
  body' <- Subst.instantiate (substHandle h) fullSubst body
  reduce (h {subst = IntMap.empty}) body'

extractIdent :: C.Value -> Maybe Ident
extractIdent term =
  case term of
    C.VarLocal x ->
      Just x
    _ ->
      Nothing
