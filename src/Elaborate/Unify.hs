{-# LANGUAGE TupleSections #-}

module Elaborate.Unify
  ( unify,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Exception.Safe (throw)
import Control.Monad (forM)
import Data.Basic
  ( BinderF,
    Hint,
    Ident,
    LamKindF (LamKindCons, LamKindFix, LamKindNormal),
    Opacity (OpacityTransparent),
    asInt,
    getPosInfo,
  )
import Data.Global
  ( constraintListRef,
    newIdentFromText,
    substRef,
    suspendedConstraintQueueRef,
    termDefEnvRef,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import qualified Data.IntMap as IntMap
import Data.Log (Error (Error), logError)
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Text as T
import Data.WeakTerm
  ( Constraint,
    ConstraintKind (ConstraintKindDelta, ConstraintKindOther),
    SubstWeakTerm,
    SuspendedConstraint (SuspendedConstraint),
    WeakTerm,
    WeakTermF (..),
    asVar,
    asterWeakTerm,
    metaOf,
    toText,
    varWeakTerm,
  )
import Reduce.WeakTerm (reduceWeakTerm, substWeakTerm)

data Stuck
  = StuckPiElimVarLocal Ident [(Hint, [WeakTerm])]
  | StuckPiElimVarGlobal T.Text [(Hint, [WeakTerm])]
  | StuckPiElimAster Int [[WeakTerm]]

unify :: IO ()
unify =
  analyze >> synthesize

analyze :: IO ()
analyze = do
  constraintList <- readIORef constraintListRef
  modifyIORef' constraintListRef $ const []
  simplify $ zip constraintList constraintList

synthesize :: IO ()
synthesize = do
  suspendedConstraintQueue <- readIORef suspendedConstraintQueueRef
  case Q.minView suspendedConstraintQueue of
    Nothing ->
      return ()
    Just (SuspendedConstraint (_, ConstraintKindDelta c, (_, orig)), cs') -> do
      modifyIORef' suspendedConstraintQueueRef $ const cs'
      simplify [(c, orig)]
      synthesize
    Just (SuspendedConstraint (_, ConstraintKindOther, _), _) ->
      throwTypeErrors

throwTypeErrors :: IO a
throwTypeErrors = do
  suspendedConstraintQueue <- readIORef suspendedConstraintQueueRef
  subst <- readIORef substRef
  errorList <- forM (Q.toList suspendedConstraintQueue) $ \(SuspendedConstraint (_, _, (_, (expected, actual)))) -> do
    -- p' foo
    -- p $ T.unpack $ toText l
    -- p $ T.unpack $ toText r
    -- p' (expected, actual)
    -- p' sub
    expected' <- substWeakTerm subst expected >>= reduceWeakTerm
    actual' <- substWeakTerm subst actual >>= reduceWeakTerm
    -- expected' <- substWeakTerm sub l >>= reduceWeakTerm
    -- actual' <- substWeakTerm sub r >>= reduceWeakTerm
    return $ logError (getPosInfo (metaOf actual)) $ constructErrorMsg actual' expected'
  throw $ Error errorList

constructErrorMsg :: WeakTerm -> WeakTerm -> T.Text
constructErrorMsg e1 e2 =
  "couldn't verify the definitional equality of the following two terms:\n- "
    <> toText e1
    <> "\n- "
    <> toText e2

simplify :: [(Constraint, Constraint)] -> IO ()
simplify constraintList =
  case constraintList of
    [] ->
      return ()
    headConstraint@(c, orig) : cs -> do
      expected <- reduceWeakTerm $ fst c
      actual <- reduceWeakTerm $ snd c
      case (expected, actual) of
        (_ :< WeakTermTau, _ :< WeakTermTau) ->
          simplify cs
        (_ :< WeakTermVar x1, _ :< WeakTermVar x2)
          | x1 == x2 ->
            simplify cs
        (_ :< WeakTermVarGlobal g1, _ :< WeakTermVarGlobal g2)
          | g1 == g2 ->
            simplify cs
        (m1 :< WeakTermPi xts1 cod1, m2 :< WeakTermPi xts2 cod2)
          | length xts1 == length xts2 -> do
            xt1 <- asWeakBinder m1 cod1
            xt2 <- asWeakBinder m2 cod2
            cs' <- simplifyBinder orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
            simplify $ cs' ++ cs
        (m1 :< WeakTermPiIntro kind1 xts1 e1, m2 :< WeakTermPiIntro kind2 xts2 e2)
          | LamKindFix xt1@(_, x1, _) <- kind1,
            LamKindFix xt2@(_, x2, _) <- kind2,
            x1 == x2,
            length xts1 == length xts2 -> do
            yt1 <- asWeakBinder m1 e1
            yt2 <- asWeakBinder m2 e2
            cs' <- simplifyBinder orig (xt1 : xts1 ++ [yt1]) (xt2 : xts2 ++ [yt2])
            simplify $ cs' ++ cs
          | LamKindNormal <- kind1,
            LamKindNormal <- kind2,
            length xts1 == length xts2 -> do
            xt1 <- asWeakBinder m1 e1
            xt2 <- asWeakBinder m2 e2
            cs' <- simplifyBinder orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
            simplify $ cs' ++ cs
          | LamKindCons dataName1 consName1 consNumber1 dataType1 <- kind1,
            LamKindCons dataName2 consName2 consNumber2 dataType2 <- kind2,
            dataName1 == dataName2,
            consName1 == consName2,
            consNumber1 == consNumber2,
            length xts1 == length xts2 -> do
            xt1 <- asWeakBinder m1 e1
            xt2 <- asWeakBinder m2 e2
            cs' <- simplifyBinder orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
            simplify $ ((dataType1, dataType2), orig) : cs' ++ cs
        (_ :< WeakTermSigma xts1, _ :< WeakTermSigma xts2)
          | length xts1 == length xts2 -> do
            cs' <- simplifyBinder orig xts1 xts2
            simplify $ cs' ++ cs
        (_ :< WeakTermSigmaIntro es1, _ :< WeakTermSigmaIntro es2)
          | length es1 == length es2 -> do
            simplify $ zipWith (curry (orig,)) es1 es2 ++ cs
        (_ :< WeakTermAster h1, _ :< WeakTermAster h2)
          | h1 == h2 ->
            simplify cs
        (_ :< WeakTermConst a1, _ :< WeakTermConst a2)
          | a1 == a2 ->
            simplify cs
        (_ :< WeakTermInt t1 l1, _ :< WeakTermInt t2 l2)
          | l1 == l2 ->
            simplify $ ((t1, t2), orig) : cs
        (_ :< WeakTermFloat t1 l1, _ :< WeakTermFloat t2 l2)
          | l1 == l2 ->
            simplify $ ((t1, t2), orig) : cs
        (_ :< WeakTermEnum a1, _ :< WeakTermEnum a2)
          | a1 == a2 ->
            simplify cs
        (_ :< WeakTermEnumIntro a1, _ :< WeakTermEnumIntro a2)
          | a1 == a2 ->
            simplify cs
        (_ :< WeakTermQuestion e1 t1, _ :< WeakTermQuestion e2 t2) ->
          simplify $ ((e1, e2), orig) : ((t1, t2), orig) : cs
        (_ :< WeakTermNoema s1 e1, _ :< WeakTermNoema s2 e2) ->
          simplify $ ((s1, s2), orig) : ((e1, e2), orig) : cs
        (_ :< WeakTermNoemaIntro s1 e1, _ :< WeakTermNoemaIntro s2 e2)
          | s1 == s2 ->
            simplify $ ((e1, e2), orig) : cs
        (_ :< WeakTermArray elemType1, _ :< WeakTermArray elemType2) ->
          simplify $ ((elemType1, elemType2), orig) : cs
        (_ :< WeakTermArrayIntro elemType1 elems1, _ :< WeakTermArrayIntro elemType2 elems2) ->
          simplify $ ((elemType1, elemType2), orig) : zipWith (curry (orig,)) elems1 elems2 ++ cs
        (_ :< WeakTermText, _ :< WeakTermText) ->
          simplify cs
        (_ :< WeakTermTextIntro text1, _ :< WeakTermTextIntro text2)
          | text1 == text2 ->
            simplify cs
        (e1@(m1 :< _), e2@(m2 :< _)) -> do
          subst <- readIORef substRef
          termDefEnv <- readIORef termDefEnvRef
          let fvs1 = varWeakTerm e1
          let fvs2 = varWeakTerm e2
          let fmvs1 = asterWeakTerm e1
          let fmvs2 = asterWeakTerm e2
          let fmvs = S.union fmvs1 fmvs2 -- fmvs: free meta-variables
          case (lookupAny (S.toList fmvs1) subst, lookupAny (S.toList fmvs2) subst) of
            (Just (h1, body1), Just (h2, body2)) -> do
              let s1 = IntMap.singleton h1 body1
              let s2 = IntMap.singleton h2 body2
              e1' <- substWeakTerm s1 e1
              e2' <- substWeakTerm s2 e2
              simplify $ ((e1', e2'), orig) : cs
            (Just (h1, body1), Nothing) -> do
              let s1 = IntMap.singleton h1 body1
              e1' <- substWeakTerm s1 e1
              simplify $ ((e1', e2), orig) : cs
            (Nothing, Just (h2, body2)) -> do
              let s2 = IntMap.singleton h2 body2
              e2' <- substWeakTerm s2 e2
              simplify $ ((e1, e2'), orig) : cs
            (Nothing, Nothing) -> do
              case (asStuckedTerm e1, asStuckedTerm e2) of
                (Just (StuckPiElimAster h1 ies1), _)
                  | Just xss1 <- mapM asIdentList ies1,
                    Just argSet1 <- toLinearIdentSet xss1,
                    h1 `S.notMember` fmvs2,
                    fvs2 `S.isSubsetOf` argSet1 ->
                    resolveHole h1 xss1 e2 cs
                (_, Just (StuckPiElimAster h2 ies2))
                  | Just xss2 <- mapM asIdentList ies2,
                    Just argSet2 <- toLinearIdentSet xss2,
                    h2 `S.notMember` fmvs1,
                    fvs1 `S.isSubsetOf` argSet2 ->
                    resolveHole h2 xss2 e1 cs
                (Just (StuckPiElimVarLocal x1 mess1), Just (StuckPiElimVarLocal x2 mess2))
                  | x1 == x2,
                    Just pairList <- asPairList (map snd mess1) (map snd mess2) ->
                    simplify $ map (,orig) pairList ++ cs
                (Just (StuckPiElimVarGlobal g1 mess1), Just (StuckPiElimVarGlobal g2 mess2))
                  | g1 == g2,
                    Nothing <- lookupDefinition m1 g1 termDefEnv,
                    Just pairList <- asPairList (map snd mess1) (map snd mess2) ->
                    simplify $ map (,orig) pairList ++ cs
                (Just (StuckPiElimVarGlobal g1 mess1), Just (StuckPiElimVarGlobal g2 mess2))
                  | g1 == g2,
                    Just lam <- lookupDefinition m1 g1 termDefEnv ->
                    simplify $ ((toPiElim lam mess1, toPiElim lam mess2), orig) : cs
                (Just (StuckPiElimVarGlobal g1 mess1), Just (StuckPiElimVarGlobal g2 mess2))
                  | Just lam1 <- lookupDefinition m1 g1 termDefEnv,
                    Just lam2 <- lookupDefinition m2 g2 termDefEnv ->
                    simplify $ ((toPiElim lam1 mess1, toPiElim lam2 mess2), orig) : cs
                (Just (StuckPiElimVarGlobal g1 mess1), Just StuckPiElimAster {})
                  | Just lam <- lookupDefinition m1 g1 termDefEnv -> do
                    let uc = SuspendedConstraint (fmvs, ConstraintKindDelta (toPiElim lam mess1, e2), headConstraint)
                    modifyIORef' suspendedConstraintQueueRef $ Q.insert uc
                    simplify cs
                (Just StuckPiElimAster {}, Just (StuckPiElimVarGlobal g2 mess2))
                  | Just lam <- lookupDefinition m2 g2 termDefEnv -> do
                    let uc = SuspendedConstraint (fmvs, ConstraintKindDelta (e1, toPiElim lam mess2), headConstraint)
                    modifyIORef' suspendedConstraintQueueRef $ Q.insert uc
                    simplify cs
                (Just (StuckPiElimVarGlobal g1 mess1), _)
                  | Just lam <- lookupDefinition m1 g1 termDefEnv ->
                    simplify $ ((toPiElim lam mess1, e2), orig) : cs
                (_, Just (StuckPiElimVarGlobal g2 mess2))
                  | Just lam <- lookupDefinition m2 g2 termDefEnv ->
                    simplify $ ((e1, toPiElim lam mess2), orig) : cs
                _ -> do
                  let uc = SuspendedConstraint (fmvs, ConstraintKindOther, headConstraint)
                  modifyIORef' suspendedConstraintQueueRef $ Q.insert uc
                  simplify cs

{-# INLINE resolveHole #-}
resolveHole :: Int -> [[BinderF WeakTerm]] -> WeakTerm -> [(Constraint, Constraint)] -> IO ()
resolveHole h1 xss e2' cs = do
  modifyIORef' substRef $ IntMap.insert h1 (toPiIntro xss e2')
  suspendedConstraintQueue <- readIORef suspendedConstraintQueueRef
  let (sus1, sus2) = Q.partition (\(SuspendedConstraint (hs, _, _)) -> S.member h1 hs) suspendedConstraintQueue
  modifyIORef' suspendedConstraintQueueRef $ const sus2
  let sus1' = map (\(SuspendedConstraint (_, _, c)) -> c) $ Q.toList sus1
  simplify $ sus1' ++ cs

simplifyBinder :: Constraint -> [BinderF WeakTerm] -> [BinderF WeakTerm] -> IO [(Constraint, Constraint)]
simplifyBinder orig =
  simplifyBinder' orig IntMap.empty

simplifyBinder' :: Constraint -> SubstWeakTerm -> [BinderF WeakTerm] -> [BinderF WeakTerm] -> IO [(Constraint, Constraint)]
simplifyBinder' orig sub args1 args2 =
  case (args1, args2) of
    ((m1, x1, t1) : xts1, (_, x2, t2) : xts2) -> do
      t2' <- substWeakTerm sub t2
      let sub' = IntMap.insert (asInt x2) (m1 :< WeakTermVar x1) sub
      rest <- simplifyBinder' orig sub' xts1 xts2
      return $ ((t1, t2'), orig) : rest
    _ ->
      return []

asWeakBinder :: Hint -> WeakTerm -> IO (BinderF WeakTerm)
asWeakBinder m t = do
  h <- newIdentFromText "aster"
  return (m, h, t)

asPairList ::
  [[WeakTerm]] ->
  [[WeakTerm]] ->
  Maybe [(WeakTerm, WeakTerm)]
asPairList list1 list2 =
  case (list1, list2) of
    ([], []) ->
      Just []
    (es1 : mess1, es2 : mess2)
      | length es1 /= length es2 ->
        Nothing
      | otherwise -> do
        pairList <- asPairList mess1 mess2
        return $ zip es1 es2 ++ pairList
    _ ->
      Nothing

asStuckedTerm :: WeakTerm -> Maybe Stuck
asStuckedTerm term =
  case term of
    (_ :< WeakTermVar x) ->
      Just $ StuckPiElimVarLocal x []
    (_ :< WeakTermVarGlobal g) ->
      Just $ StuckPiElimVarGlobal g []
    (_ :< WeakTermAster h) ->
      Just $ StuckPiElimAster h []
    (m :< WeakTermPiElim e es) ->
      case asStuckedTerm e of
        Just (StuckPiElimVarLocal x ess) ->
          Just $ StuckPiElimVarLocal x $ ess ++ [(m, es)]
        Just (StuckPiElimVarGlobal g ess) ->
          Just $ StuckPiElimVarGlobal g $ ess ++ [(m, es)]
        Just (StuckPiElimAster h iexss)
          | Just _ <- mapM asVar es ->
            Just $ StuckPiElimAster h $ iexss ++ [es]
        _ ->
          Nothing
    _ ->
      Nothing

toPiIntro :: [[BinderF WeakTerm]] -> WeakTerm -> WeakTerm
toPiIntro args e =
  case args of
    [] ->
      e
    xts : xtss -> do
      let e' = toPiIntro xtss e
      metaOf e' :< WeakTermPiIntro LamKindNormal xts e'

toPiElim :: WeakTerm -> [(Hint, [WeakTerm])] -> WeakTerm
toPiElim e args =
  case args of
    [] ->
      e
    (m, es) : ess ->
      toPiElim (m :< WeakTermPiElim e es) ess

asIdentList :: [WeakTerm] -> Maybe [BinderF WeakTerm]
asIdentList termList =
  case termList of
    [] ->
      return []
    e : es
      | (m :< WeakTermVar x) <- e -> do
        let t = m :< WeakTermTau -- don't care
        xts <- asIdentList es
        return $ (m, x, t) : xts
      | otherwise ->
        Nothing

{-# INLINE toLinearIdentSet #-}
toLinearIdentSet :: [[BinderF WeakTerm]] -> Maybe (S.Set Ident)
toLinearIdentSet xtss =
  toLinearIdentSet' xtss S.empty

toLinearIdentSet' :: [[BinderF WeakTerm]] -> S.Set Ident -> Maybe (S.Set Ident)
toLinearIdentSet' xtss acc =
  case xtss of
    [] ->
      return acc
    [] : rest ->
      toLinearIdentSet' rest acc
    ((_, x, _) : rest1) : rest2
      | x `S.member` acc ->
        Nothing
      | otherwise ->
        toLinearIdentSet' (rest1 : rest2) (S.insert x acc)

lookupAny :: [Int] -> IntMap.IntMap a -> Maybe (Int, a)
lookupAny is sub =
  case is of
    [] ->
      Nothing
    j : js ->
      case IntMap.lookup j sub of
        Just v ->
          Just (j, v)
        _ ->
          lookupAny js sub

{-# INLINE lookupDefinition #-}
lookupDefinition :: Hint -> T.Text -> Map.HashMap T.Text (Opacity, [BinderF WeakTerm], WeakTerm) -> Maybe WeakTerm
lookupDefinition m name termDefEnv =
  case Map.lookup name termDefEnv of
    Just (OpacityTransparent, xts, e) ->
      return $ m :< WeakTermPiIntro LamKindNormal xts e
    _ ->
      Nothing
