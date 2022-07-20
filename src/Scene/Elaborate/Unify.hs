module Scene.Elaborate.Unify
  ( unify,
    specialize,
  )
where

import qualified Context.App as App
import qualified Context.Definition as Definition
import qualified Context.Gensym as Gensym
import Control.Comonad.Cofree
import Control.Exception.Safe
import Control.Monad
import Data.IORef
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
import Entity.WeakTerm.Subst
import Entity.WeakTerm.ToText

data Context = Context
  { gensym :: Gensym.Context,
    definition :: Definition.Context,
    suspendedConstraintQueueRef :: IORef SuspendedConstraintQueue,
    substRef :: IORef HS.HoleSubst
  }

specialize :: App.Context -> IO Context
specialize ctx = do
  suspendedConstraintQueue <- newIORef Q.empty
  sub <- newIORef HS.empty
  return $
    Context
      { gensym = App.gensym ctx,
        definition = App.definition ctx,
        suspendedConstraintQueueRef = suspendedConstraintQueue,
        substRef = sub
      }

data Stuck
  = StuckPiElimVarLocal Ident [(Hint, [WeakTerm])]
  | StuckPiElimVarGlobal DD.DefiniteDescription [(Hint, [WeakTerm])]
  | StuckPiElimAster HID.HoleID [WeakTerm]

unify :: Context -> [Constraint] -> IO HS.HoleSubst
unify ctx constraintList = do
  analyze ctx constraintList >> synthesize ctx
  readIORef (substRef ctx)

analyze :: Context -> [Constraint] -> IO ()
analyze ctx constraintList =
  simplify ctx $ zip constraintList constraintList

synthesize :: Context -> IO ()
synthesize ctx = do
  suspendedConstraintQueue <- readIORef $ suspendedConstraintQueueRef ctx
  case Q.minView suspendedConstraintQueue of
    Nothing ->
      return ()
    Just (SuspendedConstraint (_, ConstraintKindDelta c, (_, orig)), cs') -> do
      modifyIORef' (suspendedConstraintQueueRef ctx) $ const cs'
      simplify ctx [(c, orig)]
      synthesize ctx
    Just (SuspendedConstraint (_, ConstraintKindOther, _), _) ->
      throwTypeErrors ctx

throwTypeErrors :: Context -> IO a
throwTypeErrors ctx = do
  suspendedConstraintQueue <- readIORef $ suspendedConstraintQueueRef ctx
  sub <- readIORef $ substRef ctx
  errorList <- forM (Q.toList suspendedConstraintQueue) $ \(SuspendedConstraint (_, _, (_, (expected, actual)))) -> do
    -- p' foo
    -- p $ T.unpack $ toText l
    -- p $ T.unpack $ toText r
    -- p' (expected, actual)
    -- p' sub
    expected' <- fill (gensym ctx) sub expected >>= reduce (gensym ctx)
    actual' <- fill (gensym ctx) sub actual >>= reduce (gensym ctx)
    -- expected' <- subst sub l >>= reduce
    -- actual' <- subst sub r >>= reduce
    return $ logError (fromHint (metaOf actual)) $ constructErrorMsg actual' expected'
  throw $ Error errorList

constructErrorMsg :: WeakTerm -> WeakTerm -> T.Text
constructErrorMsg e1 e2 =
  "couldn't verify the definitional equality of the following two terms:\n- "
    <> toText e1
    <> "\n- "
    <> toText e2

simplify :: Context -> [(Constraint, Constraint)] -> IO ()
simplify ctx constraintList =
  case constraintList of
    [] ->
      return ()
    headConstraint@(c, orig) : cs -> do
      expected <- reduce (gensym ctx) $ fst c
      actual <- reduce (gensym ctx) $ snd c
      case (expected, actual) of
        (_ :< WeakTermTau, _ :< WeakTermTau) ->
          simplify ctx cs
        (_ :< WeakTermVar x1, _ :< WeakTermVar x2)
          | x1 == x2 ->
            simplify ctx cs
        (_ :< WeakTermVarGlobal g1, _ :< WeakTermVarGlobal g2)
          | g1 == g2 ->
            simplify ctx cs
        (m1 :< WeakTermPi xts1 cod1, m2 :< WeakTermPi xts2 cod2)
          | length xts1 == length xts2 -> do
            xt1 <- asWeakBinder ctx m1 cod1
            xt2 <- asWeakBinder ctx m2 cod2
            cs' <- simplifyBinder ctx orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
            simplify ctx $ cs' ++ cs
        (m1 :< WeakTermPiIntro kind1 xts1 e1, m2 :< WeakTermPiIntro kind2 xts2 e2)
          | LamKindFix xt1@(_, x1, _) <- kind1,
            LamKindFix xt2@(_, x2, _) <- kind2,
            x1 == x2,
            length xts1 == length xts2 -> do
            yt1 <- asWeakBinder ctx m1 e1
            yt2 <- asWeakBinder ctx m2 e2
            cs' <- simplifyBinder ctx orig (xt1 : xts1 ++ [yt1]) (xt2 : xts2 ++ [yt2])
            simplify ctx $ cs' ++ cs
          | LamKindNormal <- kind1,
            LamKindNormal <- kind2,
            length xts1 == length xts2 -> do
            xt1 <- asWeakBinder ctx m1 e1
            xt2 <- asWeakBinder ctx m2 e2
            cs' <- simplifyBinder ctx orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
            simplify ctx $ cs' ++ cs
          | LamKindCons dataName1 consName1 consNumber1 dataType1 <- kind1,
            LamKindCons dataName2 consName2 consNumber2 dataType2 <- kind2,
            dataName1 == dataName2,
            consName1 == consName2,
            consNumber1 == consNumber2,
            length xts1 == length xts2 -> do
            xt1 <- asWeakBinder ctx m1 e1
            xt2 <- asWeakBinder ctx m2 e2
            cs' <- simplifyBinder ctx orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
            simplify ctx $ ((dataType1, dataType2), orig) : cs' ++ cs
        (_ :< WeakTermSigma xts1, _ :< WeakTermSigma xts2)
          | length xts1 == length xts2 -> do
            cs' <- simplifyBinder ctx orig xts1 xts2
            simplify ctx $ cs' ++ cs
        (_ :< WeakTermSigmaIntro es1, _ :< WeakTermSigmaIntro es2)
          | length es1 == length es2 -> do
            simplify ctx $ zipWith (curry (orig,)) es1 es2 ++ cs
        (_ :< WeakTermPrim a1, _ :< WeakTermPrim a2)
          | a1 == a2 ->
            simplify ctx cs
        (_ :< WeakTermInt t1 l1, _ :< WeakTermInt t2 l2)
          | l1 == l2 ->
            simplify ctx $ ((t1, t2), orig) : cs
        (_ :< WeakTermFloat t1 l1, _ :< WeakTermFloat t2 l2)
          | l1 == l2 ->
            simplify ctx $ ((t1, t2), orig) : cs
        (_ :< WeakTermEnum a1, _ :< WeakTermEnum a2)
          | a1 == a2 ->
            simplify ctx cs
        (_ :< WeakTermEnumIntro label1, _ :< WeakTermEnumIntro label2)
          | label1 == label2 ->
            simplify ctx cs
        (_ :< WeakTermQuestion e1 t1, _ :< WeakTermQuestion e2 t2) ->
          simplify ctx $ ((e1, e2), orig) : ((t1, t2), orig) : cs
        (_ :< WeakTermNoema s1 e1, _ :< WeakTermNoema s2 e2) ->
          simplify ctx $ ((s1, s2), orig) : ((e1, e2), orig) : cs
        (_ :< WeakTermNoemaIntro s1 e1, _ :< WeakTermNoemaIntro s2 e2)
          | s1 == s2 ->
            simplify ctx $ ((e1, e2), orig) : cs
        (_ :< WeakTermArray elemType1, _ :< WeakTermArray elemType2) ->
          simplify ctx $ ((elemType1, elemType2), orig) : cs
        (_ :< WeakTermArrayIntro elemType1 elems1, _ :< WeakTermArrayIntro elemType2 elems2) ->
          simplify ctx $ ((elemType1, elemType2), orig) : zipWith (curry (orig,)) elems1 elems2 ++ cs
        (_ :< WeakTermText, _ :< WeakTermText) ->
          simplify ctx cs
        (_ :< WeakTermTextIntro text1, _ :< WeakTermTextIntro text2)
          | text1 == text2 ->
            simplify ctx cs
        (_ :< WeakTermCell contentType1, _ :< WeakTermCell contentType2) ->
          simplify ctx $ ((contentType1, contentType2), orig) : cs
        (_ :< WeakTermCellIntro contentType1 content1, _ :< WeakTermCellIntro contentType2 content2) ->
          simplify ctx $ ((contentType1, contentType2), orig) : ((content1, content2), orig) : cs
        (_ :< WeakTermResourceType name1, _ :< WeakTermResourceType name2)
          | name1 == name2 ->
            simplify ctx cs
        (e1, e2) -> do
          sub <- readIORef $ substRef ctx
          let fvs1 = freeVars e1
          let fvs2 = freeVars e2
          let fmvs1 = holes e1 -- fmvs: free meta-variables
          let fmvs2 = holes e2
          case (lookupAny (S.toList fmvs1) sub, lookupAny (S.toList fmvs2) sub) of
            (Just (h1, (xs1, body1)), Just (h2, (xs2, body2))) -> do
              let s1 = HS.singleton h1 xs1 body1
              let s2 = HS.singleton h2 xs2 body2
              e1' <- fill (gensym ctx) s1 e1
              e2' <- fill (gensym ctx) s2 e2
              simplify ctx $ ((e1', e2'), orig) : cs
            (Just (h1, (xs1, body1)), Nothing) -> do
              let s1 = HS.singleton h1 xs1 body1
              e1' <- fill (gensym ctx) s1 e1
              simplify ctx $ ((e1', e2), orig) : cs
            (Nothing, Just (h2, (xs2, body2))) -> do
              let s2 = HS.singleton h2 xs2 body2
              e2' <- fill (gensym ctx) s2 e2
              simplify ctx $ ((e1, e2'), orig) : cs
            (Nothing, Nothing)
              | Definition.Context
                  { Definition.read = defMapProducer,
                    Definition.lookup = defMapConsumer
                  } <-
                  definition ctx -> do
                defMap <- defMapProducer
                let fmvs = S.union fmvs1 fmvs2
                case (asStuckedTerm e1, asStuckedTerm e2) of
                  (Just (StuckPiElimAster h1 ies1), _)
                    | Just xss1 <- mapM asIdent ies1,
                      Just argSet1 <- toLinearIdentSet xss1,
                      h1 `S.notMember` fmvs2,
                      fvs2 `S.isSubsetOf` argSet1 ->
                      resolveHole ctx h1 xss1 e2 cs
                  (_, Just (StuckPiElimAster h2 ies2))
                    | Just xss2 <- mapM asIdent ies2,
                      Just argSet2 <- toLinearIdentSet xss2,
                      h2 `S.notMember` fmvs1,
                      fvs1 `S.isSubsetOf` argSet2 ->
                      resolveHole ctx h2 xss2 e1 cs
                  (Just (StuckPiElimVarLocal x1 mess1), Just (StuckPiElimVarLocal x2 mess2))
                    | x1 == x2,
                      Just pairList <- asPairList (map snd mess1) (map snd mess2) ->
                      simplify ctx $ map (,orig) pairList ++ cs
                  (Just (StuckPiElimVarGlobal g1 mess1), Just (StuckPiElimVarGlobal g2 mess2))
                    | g1 == g2,
                      Nothing <- defMapConsumer g1 defMap,
                      Just pairList <- asPairList (map snd mess1) (map snd mess2) ->
                      simplify ctx $ map (,orig) pairList ++ cs
                    | g1 == g2,
                      Just lam <- defMapConsumer g1 defMap ->
                      simplify ctx $ ((toPiElim lam mess1, toPiElim lam mess2), orig) : cs
                    | Just lam1 <- defMapConsumer g1 defMap,
                      Just lam2 <- defMapConsumer g2 defMap ->
                      simplify ctx $ ((toPiElim lam1 mess1, toPiElim lam2 mess2), orig) : cs
                  (Just (StuckPiElimVarGlobal g1 mess1), Just StuckPiElimAster {})
                    | Just lam <- defMapConsumer g1 defMap -> do
                      let uc = SuspendedConstraint (fmvs, ConstraintKindDelta (toPiElim lam mess1, e2), headConstraint)
                      modifyIORef' (suspendedConstraintQueueRef ctx) $ Q.insert uc
                      simplify ctx cs
                  (Just StuckPiElimAster {}, Just (StuckPiElimVarGlobal g2 mess2))
                    | Just lam <- defMapConsumer g2 defMap -> do
                      let uc = SuspendedConstraint (fmvs, ConstraintKindDelta (e1, toPiElim lam mess2), headConstraint)
                      modifyIORef' (suspendedConstraintQueueRef ctx) $ Q.insert uc
                      simplify ctx cs
                  (Just (StuckPiElimVarGlobal g1 mess1), _)
                    | Just lam <- defMapConsumer g1 defMap ->
                      simplify ctx $ ((toPiElim lam mess1, e2), orig) : cs
                  (_, Just (StuckPiElimVarGlobal g2 mess2))
                    | Just lam <- defMapConsumer g2 defMap ->
                      simplify ctx $ ((e1, toPiElim lam mess2), orig) : cs
                  _ -> do
                    let uc = SuspendedConstraint (fmvs, ConstraintKindOther, headConstraint)
                    modifyIORef' (suspendedConstraintQueueRef ctx) $ Q.insert uc
                    simplify ctx cs

{-# INLINE resolveHole #-}
resolveHole :: Context -> HID.HoleID -> [Ident] -> WeakTerm -> [(Constraint, Constraint)] -> IO ()
resolveHole ctx h1 xs e2' cs = do
  modifyIORef' (substRef ctx) $ HS.insert h1 xs e2'
  suspendedConstraintQueue <- readIORef $ suspendedConstraintQueueRef ctx
  let (sus1, sus2) = Q.partition (\(SuspendedConstraint (hs, _, _)) -> S.member h1 hs) suspendedConstraintQueue
  modifyIORef' (suspendedConstraintQueueRef ctx) $ const sus2
  let sus1' = map (\(SuspendedConstraint (_, _, c)) -> c) $ Q.toList sus1
  simplify ctx $ sus1' ++ cs

simplifyBinder ::
  Context ->
  Constraint ->
  [BinderF WeakTerm] ->
  [BinderF WeakTerm] ->
  IO [(Constraint, Constraint)]
simplifyBinder ctx orig =
  simplifyBinder' ctx orig IntMap.empty

simplifyBinder' ::
  Context ->
  Constraint ->
  SubstWeakTerm ->
  [BinderF WeakTerm] ->
  [BinderF WeakTerm] ->
  IO [(Constraint, Constraint)]
simplifyBinder' ctx orig sub args1 args2 =
  case (args1, args2) of
    ((m1, x1, t1) : xts1, (_, x2, t2) : xts2) -> do
      t2' <- subst (gensym ctx) sub t2
      let sub' = IntMap.insert (Ident.toInt x2) (m1 :< WeakTermVar x1) sub
      rest <- simplifyBinder' ctx orig sub' xts1 xts2
      return $ ((t1, t2'), orig) : rest
    _ ->
      return []

asWeakBinder :: Context -> Hint -> WeakTerm -> IO (BinderF WeakTerm)
asWeakBinder ctx m t = do
  h <- Gensym.newIdentFromText (gensym ctx) "aster"
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
