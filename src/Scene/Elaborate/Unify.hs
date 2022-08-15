module Scene.Elaborate.Unify
  ( unify,
    Context (..),
  )
where

import qualified Context.Env as Env
import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap as IntMap
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.Constraint
import qualified Entity.DefiniteDescription as DD
import Entity.FilePos
import Entity.Hint
import qualified Entity.HoleID as HID
import qualified Entity.HoleSubst as HS
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Log
import Entity.WeakTerm
import Entity.WeakTerm.Fill
import Entity.WeakTerm.FreeVars
import Entity.WeakTerm.Holes
import Entity.WeakTerm.Reduce
import qualified Entity.WeakTerm.Subst as Subst
import Entity.WeakTerm.ToText

class (Subst.Context m, Gensym.Context m, Throw.Context m, Env.Context m) => Context m where
  insertSubst :: HID.HoleID -> [Ident] -> WeakTerm -> m ()
  setConstraintQueue :: Q.MinQueue SuspendedConstraint -> m ()
  insertConstraint :: SuspendedConstraint -> m ()
  getConstraintQueue :: m SuspendedConstraintQueue
  getDefMap :: m DefMap

type DefMap = Map.HashMap DD.DefiniteDescription WeakTerm

data Stuck
  = StuckPiElimVarLocal Ident [(Hint, [WeakTerm])]
  | StuckPiElimVarGlobal DD.DefiniteDescription [(Hint, [WeakTerm])]
  | StuckPiElimAster HID.HoleID [WeakTerm]

unify :: Context m => [Constraint] -> m HS.HoleSubst
unify constraintList = do
  analyze constraintList >> synthesize
  Env.getHoleSubst

analyze :: Context m => [Constraint] -> m ()
analyze constraintList =
  simplify $ zip constraintList constraintList

synthesize :: Context m => m ()
synthesize = do
  suspendedConstraintQueue <- getConstraintQueue
  case Q.minView suspendedConstraintQueue of
    Nothing ->
      return ()
    Just (SuspendedConstraint (_, ConstraintKindDelta c, (_, orig)), cs') -> do
      setConstraintQueue cs'
      simplify [(c, orig)]
      synthesize
    Just (SuspendedConstraint (_, ConstraintKindOther, _), _) ->
      throwTypeErrors

throwTypeErrors :: Context m => m a
throwTypeErrors = do
  suspendedConstraintQueue <- getConstraintQueue
  sub <- Env.getHoleSubst
  errorList <- forM (Q.toList suspendedConstraintQueue) $ \(SuspendedConstraint (_, _, (_, (expected, actual)))) -> do
    -- p' foo
    -- p $ T.unpack $ toText l
    -- p $ T.unpack $ toText r
    -- p' (expected, actual)
    -- p' sub
    expected' <- fill sub expected >>= reduce
    actual' <- fill sub actual >>= reduce
    -- expected' <- subst sub l >>= reduce
    -- actual' <- subst sub r >>= reduce
    return $ logError (fromHint (metaOf actual)) $ constructErrorMsg actual' expected'
  Throw.throw $ Error errorList

constructErrorMsg :: WeakTerm -> WeakTerm -> T.Text
constructErrorMsg e1 e2 =
  "couldn't verify the definitional equality of the following two terms:\n- "
    <> toText e1
    <> "\n- "
    <> toText e2

simplify :: Context m => [(Constraint, Constraint)] -> m ()
simplify constraintList =
  case constraintList of
    [] ->
      return ()
    headConstraint@(c, orig) : cs -> do
      expected <- reduce $ fst c
      actual <- reduce $ snd c
      case (expected, actual) of
        (_ :< WeakTermTau, _ :< WeakTermTau) ->
          simplify cs
        (_ :< WeakTermVar x1, _ :< WeakTermVar x2)
          | x1 == x2 ->
            simplify cs
        (_ :< WeakTermVarGlobal g1 _, _ :< WeakTermVarGlobal g2 _)
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
        (_ :< WeakTermPrim a1, _ :< WeakTermPrim a2)
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
        (_ :< WeakTermEnumIntro label1, _ :< WeakTermEnumIntro label2)
          | label1 == label2 ->
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
        (_ :< WeakTermCell contentType1, _ :< WeakTermCell contentType2) ->
          simplify $ ((contentType1, contentType2), orig) : cs
        (_ :< WeakTermCellIntro contentType1 content1, _ :< WeakTermCellIntro contentType2 content2) ->
          simplify $ ((contentType1, contentType2), orig) : ((content1, content2), orig) : cs
        (_ :< WeakTermResourceType name1, _ :< WeakTermResourceType name2)
          | name1 == name2 ->
            simplify cs
        (e1, e2) -> do
          sub <- Env.getHoleSubst
          let fvs1 = freeVars e1
          let fvs2 = freeVars e2
          let fmvs1 = holes e1 -- fmvs: free meta-variables
          let fmvs2 = holes e2
          case (lookupAny (S.toList fmvs1) sub, lookupAny (S.toList fmvs2) sub) of
            (Just (h1, (xs1, body1)), Just (h2, (xs2, body2))) -> do
              let s1 = HS.singleton h1 xs1 body1
              let s2 = HS.singleton h2 xs2 body2
              e1' <- fill s1 e1
              e2' <- fill s2 e2
              simplify $ ((e1', e2'), orig) : cs
            (Just (h1, (xs1, body1)), Nothing) -> do
              let s1 = HS.singleton h1 xs1 body1
              e1' <- fill s1 e1
              simplify $ ((e1', e2), orig) : cs
            (Nothing, Just (h2, (xs2, body2))) -> do
              let s2 = HS.singleton h2 xs2 body2
              e2' <- fill s2 e2
              simplify $ ((e1, e2'), orig) : cs
            (Nothing, Nothing) -> do
              defMap <- getDefMap
              let fmvs = S.union fmvs1 fmvs2
              case (asStuckedTerm e1, asStuckedTerm e2) of
                (Just (StuckPiElimAster h1 ies1), _)
                  | Just xss1 <- mapM asIdent ies1,
                    Just argSet1 <- toLinearIdentSet xss1,
                    h1 `S.notMember` fmvs2,
                    fvs2 `S.isSubsetOf` argSet1 ->
                    resolveHole h1 xss1 e2 cs
                (_, Just (StuckPiElimAster h2 ies2))
                  | Just xss2 <- mapM asIdent ies2,
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
                    Nothing <- Map.lookup g1 defMap,
                    Just pairList <- asPairList (map snd mess1) (map snd mess2) ->
                    simplify $ map (,orig) pairList ++ cs
                  | g1 == g2,
                    Just lam <- Map.lookup g1 defMap ->
                    simplify $ ((toPiElim lam mess1, toPiElim lam mess2), orig) : cs
                  | Just lam1 <- Map.lookup g1 defMap,
                    Just lam2 <- Map.lookup g2 defMap ->
                    simplify $ ((toPiElim lam1 mess1, toPiElim lam2 mess2), orig) : cs
                (Just (StuckPiElimVarGlobal g1 mess1), Just StuckPiElimAster {})
                  | Just lam <- Map.lookup g1 defMap -> do
                    let uc = SuspendedConstraint (fmvs, ConstraintKindDelta (toPiElim lam mess1, e2), headConstraint)
                    insertConstraint uc
                    simplify cs
                (Just StuckPiElimAster {}, Just (StuckPiElimVarGlobal g2 mess2))
                  | Just lam <- Map.lookup g2 defMap -> do
                    let uc = SuspendedConstraint (fmvs, ConstraintKindDelta (e1, toPiElim lam mess2), headConstraint)
                    insertConstraint uc
                    simplify cs
                (Just (StuckPiElimVarGlobal g1 mess1), _)
                  | Just lam <- Map.lookup g1 defMap ->
                    simplify $ ((toPiElim lam mess1, e2), orig) : cs
                (_, Just (StuckPiElimVarGlobal g2 mess2))
                  | Just lam <- Map.lookup g2 defMap ->
                    simplify $ ((e1, toPiElim lam mess2), orig) : cs
                _ -> do
                  let uc = SuspendedConstraint (fmvs, ConstraintKindOther, headConstraint)
                  insertConstraint uc
                  simplify cs

{-# INLINE resolveHole #-}
resolveHole :: Context m => HID.HoleID -> [Ident] -> WeakTerm -> [(Constraint, Constraint)] -> m ()
resolveHole h1 xs e2' cs = do
  insertSubst h1 xs e2'
  suspendedConstraintQueue <- getConstraintQueue
  let (sus1, sus2) = Q.partition (\(SuspendedConstraint (hs, _, _)) -> S.member h1 hs) suspendedConstraintQueue
  setConstraintQueue sus2
  let sus1' = map (\(SuspendedConstraint (_, _, c)) -> c) $ Q.toList sus1
  simplify $ sus1' ++ cs

simplifyBinder ::
  Context m =>
  Constraint ->
  [BinderF WeakTerm] ->
  [BinderF WeakTerm] ->
  m [(Constraint, Constraint)]
simplifyBinder orig =
  simplifyBinder' orig IntMap.empty

simplifyBinder' ::
  Context m =>
  Constraint ->
  SubstWeakTerm ->
  [BinderF WeakTerm] ->
  [BinderF WeakTerm] ->
  m [(Constraint, Constraint)]
simplifyBinder' orig sub args1 args2 =
  case (args1, args2) of
    ((m1, x1, t1) : xts1, (_, x2, t2) : xts2) -> do
      t2' <- Subst.subst sub t2
      let sub' = IntMap.insert (Ident.toInt x2) (m1 :< WeakTermVar x1) sub
      rest <- simplifyBinder' orig sub' xts1 xts2
      return $ ((t1, t2'), orig) : rest
    _ ->
      return []

asWeakBinder :: Context m => Hint -> WeakTerm -> m (BinderF WeakTerm)
asWeakBinder m t = do
  h <- Gensym.newIdentFromText "aster"
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
    (_ :< WeakTermVarGlobal g _) ->
      Just $ StuckPiElimVarGlobal g []
    (_ :< WeakTermAster h es) ->
      Just $ StuckPiElimAster h es
    (m :< WeakTermPiElim e es) ->
      case asStuckedTerm e of
        Just (StuckPiElimVarLocal x ess) ->
          Just $ StuckPiElimVarLocal x $ ess ++ [(m, es)]
        Just (StuckPiElimVarGlobal g ess) ->
          Just $ StuckPiElimVarGlobal g $ ess ++ [(m, es)]
        _ ->
          Nothing
    _ ->
      Nothing

toPiElim :: WeakTerm -> [(Hint, [WeakTerm])] -> WeakTerm
toPiElim e args =
  case args of
    [] ->
      e
    (m, es) : ess ->
      toPiElim (m :< WeakTermPiElim e es) ess

asIdent :: WeakTerm -> Maybe Ident
asIdent e =
  case e of
    _ :< WeakTermVar x ->
      return x
    _ ->
      Nothing

{-# INLINE toLinearIdentSet #-}
toLinearIdentSet :: [Ident] -> Maybe (S.Set Ident)
toLinearIdentSet xs =
  toLinearIdentSet' xs S.empty

toLinearIdentSet' :: [Ident] -> S.Set Ident -> Maybe (S.Set Ident)
toLinearIdentSet' xs acc =
  case xs of
    [] ->
      return acc
    x : rest
      | x `S.member` acc ->
        Nothing
      | otherwise ->
        toLinearIdentSet' rest (S.insert x acc)

lookupAny :: [HID.HoleID] -> HS.HoleSubst -> Maybe (HID.HoleID, ([Ident], WeakTerm))
lookupAny is sub =
  case is of
    [] ->
      Nothing
    j : js ->
      case HS.lookup j sub of
        Just v ->
          Just (j, v)
        _ ->
          lookupAny js sub
