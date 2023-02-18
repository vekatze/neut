module Scene.Elaborate.Unify
  ( unify,
  )
where

import Context.App
import Context.Definition qualified as Definition
import Context.Elaborate
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.PQueue.Min qualified as Q
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Binder
import Entity.Constraint qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.FilePos
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.HoleSubst qualified as HS
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Log qualified as L
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.FreeVars
import Entity.WeakTerm.Holes
import Entity.WeakTerm.ToText
import Scene.WeakTerm.Fill
import Scene.WeakTerm.Reduce
import Scene.WeakTerm.Subst qualified as Subst

data Stuck
  = StuckPiElimVarLocal Ident [(Hint, [WT.WeakTerm])]
  | StuckPiElimVarGlobal DD.DefiniteDescription [(Hint, [WT.WeakTerm])]
  | StuckPiElimHole HID.HoleID [WT.WeakTerm]

unify :: [C.Constraint] -> App HS.HoleSubst
unify constraintList = do
  analyze constraintList >> synthesize
  getHoleSubst

analyze :: [C.Constraint] -> App ()
analyze constraintList =
  simplify $ zip constraintList constraintList

synthesize :: App ()
synthesize = do
  suspendedConstraintQueue <- getConstraintQueue
  case Q.minView suspendedConstraintQueue of
    Nothing ->
      return ()
    Just (C.SuspendedConstraint (_, C.Delta c, (_, orig)), cs') -> do
      setConstraintQueue cs'
      simplify [(c, orig)]
      synthesize
    Just (C.SuspendedConstraint (_, C.Other, _), _) ->
      throwTypeErrors

throwTypeErrors :: App a
throwTypeErrors = do
  suspendedConstraintQueue <- getConstraintQueue
  sub <- getHoleSubst
  errorList <- forM (Q.toList suspendedConstraintQueue) $ \(C.SuspendedConstraint (_, _, (_, (expected, actual)))) -> do
    expected' <- fill sub expected >>= reduce
    actual' <- fill sub actual >>= reduce
    return $ L.logError (fromHint (WT.metaOf actual)) $ constructErrorMsg actual' expected'
  Throw.throw $ L.MakeError errorList

constructErrorMsg :: WT.WeakTerm -> WT.WeakTerm -> T.Text
constructErrorMsg e1 e2 =
  "couldn't verify the definitional equality of the following two terms:\n- "
    <> toText e1
    <> "\n- "
    <> toText e2

simplify :: [(C.Constraint, C.Constraint)] -> App ()
simplify constraintList =
  case constraintList of
    [] ->
      return ()
    headConstraint@(c, orig) : cs -> do
      expected <- reduce $ fst c
      actual <- reduce $ snd c
      case (expected, actual) of
        (_ :< WT.Tau, _ :< WT.Tau) ->
          simplify cs
        (_ :< WT.Var x1, _ :< WT.Var x2)
          | x1 == x2 ->
              simplify cs
        (_ :< WT.VarGlobal g1 _, _ :< WT.VarGlobal g2 _)
          | g1 == g2 ->
              simplify cs
        (m1 :< WT.Pi xts1 cod1, m2 :< WT.Pi xts2 cod2)
          | length xts1 == length xts2 -> do
              xt1 <- asWeakBinder m1 cod1
              xt2 <- asWeakBinder m2 cod2
              cs' <- simplifyBinder orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
              simplify $ cs' ++ cs
        (m1 :< WT.PiIntro kind1 xts1 e1, m2 :< WT.PiIntro kind2 xts2 e2)
          | LK.Fix xt1@(_, x1, _) <- kind1,
            LK.Fix xt2@(_, x2, _) <- kind2,
            x1 == x2,
            length xts1 == length xts2 -> do
              yt1 <- asWeakBinder m1 e1
              yt2 <- asWeakBinder m2 e2
              cs' <- simplifyBinder orig (xt1 : xts1 ++ [yt1]) (xt2 : xts2 ++ [yt2])
              simplify $ cs' ++ cs
          | LK.Normal opacity1 <- kind1,
            LK.Normal opacity2 <- kind2,
            opacity1 == opacity2,
            length xts1 == length xts2 -> do
              xt1 <- asWeakBinder m1 e1
              xt2 <- asWeakBinder m2 e2
              cs' <- simplifyBinder orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
              simplify $ cs' ++ cs
        (_ :< WT.Data name1 es1, _ :< WT.Data name2 es2)
          | name1 == name2,
            length es1 == length es2 -> do
              let cs' = zip (zip es1 es2) (repeat orig)
              simplify $ cs' ++ cs
        (_ :< WT.DataIntro dataName1 consName1 _ dataArgs1 consArgs1, _ :< WT.DataIntro dataName2 consName2 _ dataArgs2 consArgs2)
          | dataName1 == dataName2,
            consName1 == consName2,
            length dataArgs1 == length dataArgs2,
            length consArgs1 == length consArgs2 -> do
              let es1 = dataArgs1 ++ consArgs1
              let es2 = dataArgs2 ++ consArgs2
              let cs' = zip (zip es1 es2) (repeat orig)
              simplify $ cs' ++ cs
        (_ :< WT.Noema t1, _ :< WT.Noema t2) ->
          simplify $ ((t1, t2), orig) : cs
        (_ :< WT.Prim a1, _ :< WT.Prim a2)
          | WP.Type t1 <- a1,
            WP.Type t2 <- a2,
            t1 == t2 ->
              simplify cs
          | WP.Value (WPV.Int t1 l1) <- a1,
            WP.Value (WPV.Int t2 l2) <- a2,
            l1 == l2 ->
              simplify $ ((t1, t2), orig) : cs
          | WP.Value (WPV.Float t1 l1) <- a1,
            WP.Value (WPV.Float t2 l2) <- a2,
            l1 == l2 ->
              simplify $ ((t1, t2), orig) : cs
          | WP.Value (WPV.Op op1) <- a1,
            WP.Value (WPV.Op op2) <- a2,
            op1 == op2 ->
              simplify cs
        (_ :< WT.ResourceType name1, _ :< WT.ResourceType name2)
          | name1 == name2 ->
              simplify cs
        (e1, e2) -> do
          sub <- getHoleSubst
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
              defMap <- Definition.read
              -- defMap <- getDefMap
              let fmvs = S.union fmvs1 fmvs2
              case (asStuckedTerm e1, asStuckedTerm e2) of
                (Just (StuckPiElimHole h1 ies1), _)
                  | Just xss1 <- mapM asIdent ies1,
                    Just argSet1 <- toLinearIdentSet xss1,
                    h1 `S.notMember` fmvs2,
                    fvs2 `S.isSubsetOf` argSet1 ->
                      resolveHole h1 xss1 e2 cs
                (_, Just (StuckPiElimHole h2 ies2))
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
                (Just (StuckPiElimVarGlobal g1 mess1), Just StuckPiElimHole {})
                  | Just lam <- Map.lookup g1 defMap -> do
                      let uc = C.SuspendedConstraint (fmvs, C.Delta (toPiElim lam mess1, e2), headConstraint)
                      insertConstraint uc
                      simplify cs
                (Just StuckPiElimHole {}, Just (StuckPiElimVarGlobal g2 mess2))
                  | Just lam <- Map.lookup g2 defMap -> do
                      let uc = C.SuspendedConstraint (fmvs, C.Delta (e1, toPiElim lam mess2), headConstraint)
                      insertConstraint uc
                      simplify cs
                (Just (StuckPiElimVarGlobal g1 mess1), _)
                  | Just lam <- Map.lookup g1 defMap ->
                      simplify $ ((toPiElim lam mess1, e2), orig) : cs
                (_, Just (StuckPiElimVarGlobal g2 mess2))
                  | Just lam <- Map.lookup g2 defMap ->
                      simplify $ ((e1, toPiElim lam mess2), orig) : cs
                _ -> do
                  let uc = C.SuspendedConstraint (fmvs, C.Other, headConstraint)
                  insertConstraint uc
                  simplify cs

{-# INLINE resolveHole #-}
resolveHole :: HID.HoleID -> [Ident] -> WT.WeakTerm -> [(C.Constraint, C.Constraint)] -> App ()
resolveHole h1 xs e2' cs = do
  insertSubst h1 xs e2'
  suspendedConstraintQueue <- getConstraintQueue
  let (sus1, sus2) = Q.partition (\(C.SuspendedConstraint (hs, _, _)) -> S.member h1 hs) suspendedConstraintQueue
  setConstraintQueue sus2
  let sus1' = map (\(C.SuspendedConstraint (_, _, c)) -> c) $ Q.toList sus1
  simplify $ sus1' ++ cs

simplifyBinder ::
  C.Constraint ->
  [BinderF WT.WeakTerm] ->
  [BinderF WT.WeakTerm] ->
  App [(C.Constraint, C.Constraint)]
simplifyBinder orig =
  simplifyBinder' orig IntMap.empty

simplifyBinder' ::
  C.Constraint ->
  WT.SubstWeakTerm ->
  [BinderF WT.WeakTerm] ->
  [BinderF WT.WeakTerm] ->
  App [(C.Constraint, C.Constraint)]
simplifyBinder' orig sub args1 args2 =
  case (args1, args2) of
    ((m1, x1, t1) : xts1, (_, x2, t2) : xts2) -> do
      t2' <- Subst.subst sub t2
      let sub' = IntMap.insert (Ident.toInt x2) (m1 :< WT.Var x1) sub
      rest <- simplifyBinder' orig sub' xts1 xts2
      return $ ((t1, t2'), orig) : rest
    _ ->
      return []

asWeakBinder :: Hint -> WT.WeakTerm -> App (BinderF WT.WeakTerm)
asWeakBinder m t = do
  h <- Gensym.newIdentFromText "hole"
  return (m, h, t)

asPairList ::
  [[WT.WeakTerm]] ->
  [[WT.WeakTerm]] ->
  Maybe [(WT.WeakTerm, WT.WeakTerm)]
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

asStuckedTerm :: WT.WeakTerm -> Maybe Stuck
asStuckedTerm term =
  case term of
    (_ :< WT.Var x) ->
      Just $ StuckPiElimVarLocal x []
    (_ :< WT.VarGlobal g _) ->
      Just $ StuckPiElimVarGlobal g []
    (_ :< WT.Hole h es) ->
      Just $ StuckPiElimHole h es
    (m :< WT.PiElim e es) ->
      case asStuckedTerm e of
        Just (StuckPiElimVarLocal x ess) ->
          Just $ StuckPiElimVarLocal x $ ess ++ [(m, es)]
        Just (StuckPiElimVarGlobal g ess) ->
          Just $ StuckPiElimVarGlobal g $ ess ++ [(m, es)]
        _ ->
          Nothing
    _ ->
      Nothing

toPiElim :: WT.WeakTerm -> [(Hint, [WT.WeakTerm])] -> WT.WeakTerm
toPiElim e args =
  case args of
    [] ->
      e
    (m, es) : ess ->
      toPiElim (m :< WT.PiElim e es) ess

asIdent :: WT.WeakTerm -> Maybe Ident
asIdent e =
  case e of
    _ :< WT.Var x ->
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

lookupAny :: [HID.HoleID] -> HS.HoleSubst -> Maybe (HID.HoleID, ([Ident], WT.WeakTerm))
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
