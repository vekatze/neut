module Elaborate.Analyze
  ( analyze
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.PQueue.Min as Q
import System.Timeout
import qualified Text.Show.Pretty as Pr

import Data.Basic
import Data.Constraint
import Data.Env
import Data.WeakTerm
import Reduce.WeakTerm

analyze :: [PreConstraint] -> WithEnv ConstraintQueue
analyze cs = Q.fromList <$> simp cs

simp :: [PreConstraint] -> WithEnv [EnrichedConstraint]
simp [] = return []
simp ((e1, e2):cs) = do
  me1' <- reduceWeakTermPlus e1 >>= liftIO . timeout 5000000 . return
  me2' <- reduceWeakTermPlus e2 >>= liftIO . timeout 5000000 . return
  case (me1', me2') of
    (Just e1', Just e2') -> simp' $ (e1', e2') : cs
    _ -> throwError $ "cannot simplify [TIMEOUT]:\n" ++ Pr.ppShow (e1, e2)

simp' :: [PreConstraint] -> WithEnv [EnrichedConstraint]
simp' [] = return []
simp' (((_, WeakTermTau), (_, WeakTermTau)):cs) = simp cs
simp' (((_, WeakTermUpsilon x1), (_, WeakTermUpsilon x2)):cs)
  | x1 == x2 = simp cs
simp' (((_, WeakTermPi xts1 cod1), (_, WeakTermPi xts2 cod2)):cs)
  | length xts1 == length xts2 = do simpBinder xts1 cod1 xts2 cod2 cs
simp' (((_, WeakTermPiIntro xts1 e1), (_, WeakTermPiIntro xts2 e2)):cs)
  | length xts1 == length xts2 = do simpBinder xts1 e1 xts2 e2 cs
simp' (((_, WeakTermPiIntro xts body1@(m1, _)), e2@(_, _)):cs) = do
  let vs = map (uncurry toVar) xts
  simp $ (body1, (m1, WeakTermPiElim e2 vs)) : cs
simp' ((e1, e2@(_, WeakTermPiIntro {})):cs) = simp' $ (e2, e1) : cs
simp' (((_, WeakTermMu xt1 e1), (_, WeakTermMu xt2 e2)):cs)
  | fst xt1 == fst xt2 = simpBinder [xt1] e1 [xt2] e2 cs
simp' (((_, WeakTermZeta x), (_, WeakTermZeta y)):cs)
  | x == y = simp cs
simp' (((_, WeakTermConst x1), (_, WeakTermConst x2)):cs)
  | x1 == x2 = simp cs
simp' (((_, WeakTermConstDecl xt1 e1), (_, WeakTermConstDecl xt2 e2)):cs) = do
  simpBinder [xt1] e1 [xt2] e2 cs
simp' (((_, WeakTermIntS size1 l1), (_, WeakTermIntS size2 l2)):cs)
  | size1 == size2
  , l1 == l2 = simp cs
simp' (((_, WeakTermIntU size1 l1), (_, WeakTermIntU size2 l2)):cs)
  | size1 == size2
  , l1 == l2 = simp cs
simp' (((_, WeakTermInt l1), (_, WeakTermIntS _ l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermIntS _ l1), (_, WeakTermInt l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermInt l1), (_, WeakTermIntU _ l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermIntU _ l1), (_, WeakTermInt l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermInt l1), (_, WeakTermInt l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermFloat16 l1), (_, WeakTermFloat16 l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermFloat32 l1), (_, WeakTermFloat32 l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermFloat64 l1), (_, WeakTermFloat64 l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermFloat l1), (_, WeakTermFloat16 l2)):cs)
  | show l1 == show l2 = simp cs
simp' (((_, WeakTermFloat16 l1), (_, WeakTermFloat l2)):cs)
  | show l1 == show l2 = simp cs
simp' (((_, WeakTermFloat l1), (_, WeakTermFloat32 l2)):cs)
  | show l1 == show l2 = simp cs
simp' (((_, WeakTermFloat32 l1), (_, WeakTermFloat l2)):cs)
  | show l1 == show l2 = simp cs
simp' (((_, WeakTermFloat l1), (_, WeakTermFloat64 l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermFloat64 l1), (_, WeakTermFloat l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermFloat l1), (_, WeakTermFloat l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermEnum l1), (_, WeakTermEnum l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermEnumIntro l1), (_, WeakTermEnumIntro l2)):cs)
  | l1 == l2 = simp cs
simp' (((_, WeakTermArray k1 indexType1), (_, WeakTermArray k2 indexType2)):cs)
  | k1 == k2 = simp $ (indexType1, indexType2) : cs
simp' (((_, WeakTermArrayIntro k1 les1), (_, WeakTermArrayIntro k2 les2)):cs)
  | k1 == k2 = do
    csArray <- simpArrayIntro les1 les2
    csCont <- simp cs
    return $ csArray ++ csCont
simp' ((e1, e2):cs) = do
  let ms1 = asStuckedTerm e1
  let ms2 = asStuckedTerm e2
  let hs = catMaybes [ms1 >>= stuckReasonOf, ms2 >>= stuckReasonOf]
  sub <- gets substEnv
  let isAnalyzable = any (`elem` map fst sub) hs
  case (isAnalyzable, takeFreeAppArgs e1 e2 sub) of
    (True, _) -> do
      cs' <- simp' cs
      let c = Enriched (e1, e2) hs $ ConstraintAnalyzable
      return $ c : cs'
    (_, Just (es1, es2)) -> simp $ zip es1 es2 ++ cs
    _ -> do
      case (ms1, ms2) of
        (Just (StuckPiElimStrict h1 ies1), _)
          | onesided h1 e2
          , xs <- concatMap getVarList ies1
          , subsume e2 xs
          , isDisjoint xs -> simpPattern h1 ies1 e1 e2 cs
        (_, Just (StuckPiElimStrict h2 ies2))
          | onesided h2 e1
          , xs <- concatMap getVarList ies2
          , subsume e1 xs
          , isDisjoint xs -> simpPattern h2 ies2 e2 e1 cs
        (Just (DeltaPiElim h1 mess1), _)
          | Just body <- lookup h1 sub -> simpDelta body mess1 e1 e2 cs
        (_, Just (DeltaPiElim h2 mess2))
          | Just body <- lookup h2 sub -> simpDelta body mess2 e2 e1 cs
        (Just (StuckPiElimStrict h1 ies1), _)
          | onesided h1 e2
          , xs <- concatMap getVarList ies1
          , subsume e2 xs -> simpQuasiPattern h1 ies1 e1 e2 cs
        (_, Just (StuckPiElimStrict h2 ies2))
          | onesided h2 e1
          , xs <- concatMap getVarList ies2
          , subsume e1 xs -> simpQuasiPattern h2 ies2 e2 e1 cs
        (Just (StuckPiElim h1 ies1), Nothing)
          | onesided h1 e2
          , xs <- concatMap getVarList ies1
          , subsume e2 xs -> simpFlexRigid h1 ies1 e1 e2 cs
        (Nothing, Just (StuckPiElim h2 ies2))
          | onesided h2 e1
          , xs <- concatMap getVarList ies2
          , subsume e1 xs -> simpFlexRigid h2 ies2 e2 e1 cs
        _ -> do
          let fmvs = (concatMap holeWeakTermPlus [e1, e2])
          simpOther e1 e2 fmvs cs

takeFreeAppArgs ::
     WeakTermPlus
  -> WeakTermPlus
  -> [(Identifier, WeakTermPlus)]
  -> Maybe ([WeakTermPlus], [WeakTermPlus])
takeFreeAppArgs e1 e2 sub
  | (_, WeakTermPiElim (_, WeakTermConst f) es1) <- e1
  , (_, WeakTermPiElim (_, WeakTermConst g) es2) <- e2
  , f == g
  , f `notElem` map fst sub
  , length es1 == length es2 = Just (es1, es2)
takeFreeAppArgs _ _ _ = Nothing

simpBinder ::
     [(Identifier, WeakTermPlus)]
  -> WeakTermPlus
  -> [(Identifier, WeakTermPlus)]
  -> WeakTermPlus
  -> [(WeakTermPlus, WeakTermPlus)]
  -> WithEnv [EnrichedConstraint]
simpBinder [] cod1 [] cod2 cs = simp $ (cod1, cod2) : cs
simpBinder ((x1, t1):xts1) cod1 ((x2, t2):xts2) cod2 cs = do
  let var1 = toVar x1 t1
  let (xts2', cod2') = substWeakTermPlusBindingsWithBody [(x2, var1)] xts2 cod2
  cst <- simp [(t1, t2)]
  cs' <- simpBinder xts1 cod1 xts2' cod2' cs
  return $ cst ++ cs'
simpBinder _ _ _ _ _ = throwError "cannot simplify (simpBinder)"

simpArrayIntro ::
     [(EnumValue, WeakTermPlus)]
  -> [(EnumValue, WeakTermPlus)]
  -> WithEnv [EnrichedConstraint]
simpArrayIntro les1 les2 = do
  let les1' = sortBy (\x y -> fst x `compare` fst y) les1
  let les2' = sortBy (\x y -> fst x `compare` fst y) les2
  let (ls1, es1) = unzip les1'
  let (ls2, es2) = unzip les2'
  if ls1 /= ls2
    then throwError "simpArrayIntro"
    else simp $ zip es1 es2

simpPattern ::
     Identifier
  -> [[WeakTermPlus]]
  -> WeakTermPlus
  -> WeakTermPlus
  -> [(WeakTermPlus, WeakTermPlus)]
  -> WithEnv [EnrichedConstraint]
simpPattern h1 ies1 e1 e2 cs = do
  cs' <- simp cs
  return $ Enriched (e1, e2) [h1] (ConstraintPattern h1 ies1 e2) : cs'

simpDelta ::
     WeakTermPlus
  -> [(PreMeta, [WeakTermPlus])]
  -> WeakTermPlus
  -> WeakTermPlus
  -> [(WeakTermPlus, WeakTermPlus)]
  -> WithEnv [EnrichedConstraint]
simpDelta body mes1 e1 e2 cs = do
  cs' <- simp cs
  return $ Enriched (e1, e2) [] (ConstraintDelta body mes1 e2) : cs'

simpQuasiPattern ::
     Identifier
  -> [[WeakTermPlus]]
  -> WeakTermPlus
  -> WeakTermPlus
  -> [(WeakTermPlus, WeakTermPlus)]
  -> WithEnv [EnrichedConstraint]
simpQuasiPattern h1 ies1 e1 e2 cs = do
  cs' <- simp cs
  let fmvs = (concatMap holeWeakTermPlus [e1, e2])
  return $ Enriched (e1, e2) fmvs (ConstraintQuasiPattern h1 ies1 e2) : cs'

simpFlexRigid ::
     Hole
  -> [[WeakTermPlus]]
  -> WeakTermPlus
  -> WeakTermPlus
  -> [PreConstraint]
  -> WithEnv [EnrichedConstraint]
simpFlexRigid h1 ies1 e1 e2 cs = do
  cs' <- simp cs
  let fmvs = (concatMap holeWeakTermPlus [e1, e2])
  return $ Enriched (e1, e2) fmvs (ConstraintFlexRigid h1 ies1 e2) : cs'

simpOther ::
     WeakTermPlus
  -> WeakTermPlus
  -> [Hole]
  -> [PreConstraint]
  -> WithEnv [EnrichedConstraint]
simpOther e1 e2 fmvs cs = do
  cs' <- simp cs
  let c = Enriched (e1, e2) fmvs $ ConstraintOther
  return $ c : cs'

data Stuck
  = StuckPiElim Hole [[WeakTermPlus]]
  | StuckPiElimStrict Hole [[WeakTermPlus]]
  | DeltaPiElim Identifier [(PreMeta, [WeakTermPlus])] -- ここでmetaを保持。

asStuckedTerm :: WeakTermPlus -> Maybe Stuck
asStuckedTerm (_, WeakTermPiElim (_, WeakTermZeta h) es)
  | Just _ <- mapM asUpsilon es = Just $ StuckPiElimStrict h [es]
asStuckedTerm (_, WeakTermPiElim (_, WeakTermZeta h) es) =
  Just $ StuckPiElim h [es]
asStuckedTerm (m, WeakTermPiElim (_, WeakTermUpsilon x) es) =
  Just $ DeltaPiElim x [(m, es)]
asStuckedTerm (m, WeakTermPiElim e es)
  | Just _ <- mapM asUpsilon es =
    case asStuckedTerm e of
      Just (StuckPiElim h iess) -> Just $ StuckPiElim h (iess ++ [es])
      Just (StuckPiElimStrict h iexss) ->
        Just $ StuckPiElimStrict h $ iexss ++ [es]
      Just (DeltaPiElim x ess) -> Just $ DeltaPiElim x $ ess ++ [(m, es)]
      Nothing -> Nothing
asStuckedTerm (m, WeakTermPiElim e es) =
  case asStuckedTerm e of
    Just (StuckPiElim h iess) -> Just $ StuckPiElim h $ iess ++ [es]
    Just (StuckPiElimStrict h iess) -> do
      Just $ StuckPiElim h $ iess ++ [es]
    Just (DeltaPiElim x ess) -> Just $ DeltaPiElim x $ ess ++ [(m, es)]
    Nothing -> Nothing
asStuckedTerm _ = Nothing

stuckReasonOf :: Stuck -> Maybe Hole
stuckReasonOf (StuckPiElim h _) = Just h
stuckReasonOf (StuckPiElimStrict h _) = Just h
stuckReasonOf (DeltaPiElim _ _) = Nothing

onesided :: Identifier -> WeakTermPlus -> Bool
onesided h e = h `notElem` holeWeakTermPlus e

subsume :: WeakTermPlus -> [Identifier] -> Bool
subsume e xs = all (`elem` xs) $ varWeakTermPlus e

isDisjoint :: [Identifier] -> Bool
isDisjoint xs = xs == nub xs

getVarList :: [WeakTermPlus] -> [Identifier]
getVarList xs = catMaybes $ map asUpsilon xs

toVar :: Identifier -> WeakTermPlus -> WeakTermPlus
toVar x t = (PreMetaNonTerminal t emptyMeta, WeakTermUpsilon x)

asUpsilon :: WeakTermPlus -> Maybe Identifier
asUpsilon (_, WeakTermUpsilon x) = Just x
asUpsilon _ = Nothing
