module Scene.Elaborate.Unify (unify) where

import Context.App
import Context.Elaborate
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Context.WeakDefinition qualified as WeakDefinition
import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.PQueue.Min qualified as Q
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Attr.Data qualified as AttrD
import Entity.Binder
import Entity.Constraint qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.Error qualified as E
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.HoleSubst qualified as HS
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Remark qualified as R
import Entity.Stuck qualified as Stuck
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as Subst
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.FreeVars
import Entity.WeakTerm.Holes
import Entity.WeakTerm.ToText
import Scene.WeakTerm.Fill
import Scene.WeakTerm.Reduce
import Scene.WeakTerm.Subst qualified as Subst

unify :: [C.Constraint] -> App HS.HoleSubst
unify constraintList = do
  -- the `reverse` here is to resolve constraints starting from the ends of functions
  analyze (reverse constraintList) >> synthesize
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
  errorList <- forM (Q.toList suspendedConstraintQueue) $ \(C.SuspendedConstraint (_, _, (_, c))) -> do
    case c of
      C.Actual t -> do
        t' <- fillAsMuchAsPossible sub t
        return $ R.newRemark (WT.metaOf t) R.Error $ constructErrorMessageActual t'
      C.Eq expected actual -> do
        expected' <- fillAsMuchAsPossible sub expected
        actual' <- fillAsMuchAsPossible sub actual
        return $ R.newRemark (WT.metaOf actual) R.Error $ constructErrorMessageEq actual' expected'
  Throw.throw $ E.MakeError errorList

fillAsMuchAsPossible :: HS.HoleSubst -> WT.WeakTerm -> App WT.WeakTerm
fillAsMuchAsPossible sub e = do
  e' <- reduce e
  if HS.fillable e' sub
    then fill sub e' >>= fillAsMuchAsPossible sub
    else return e'

constructErrorMessageEq :: WT.WeakTerm -> WT.WeakTerm -> T.Text
constructErrorMessageEq actual expected =
  "type mismatch:\n- "
    <> toText actual
    <> "\n- "
    <> toText expected

constructErrorMessageActual :: WT.WeakTerm -> T.Text
constructErrorMessageActual t =
  "a term of the following type might be noetic:\n"
    <> toText t

simplify :: [(C.Constraint, C.Constraint)] -> App ()
simplify constraintList =
  case constraintList of
    [] ->
      return ()
    (C.Actual t, orig) : cs -> do
      simplifyActual (WT.metaOf t) S.empty t orig
      simplify cs
    headConstraint@(C.Eq expected actual, orig) : cs -> do
      expected' <- reduce expected
      actual' <- reduce actual
      case (expected', actual') of
        (_ :< WT.Tau, _ :< WT.Tau) ->
          simplify cs
        (_ :< WT.Var x1, _ :< WT.Var x2)
          | x1 == x2 ->
              simplify cs
        (_ :< WT.VarGlobal _ g1, _ :< WT.VarGlobal _ g2)
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
          | LK.Normal <- kind1,
            LK.Normal <- kind2,
            length xts1 == length xts2 -> do
              xt1 <- asWeakBinder m1 e1
              xt2 <- asWeakBinder m2 e2
              cs' <- simplifyBinder orig (xts1 ++ [xt1]) (xts2 ++ [xt2])
              simplify $ cs' ++ cs
        (_ :< WT.Data _ name1 es1, _ :< WT.Data _ name2 es2)
          | name1 == name2,
            length es1 == length es2 -> do
              let cs' = map (,orig) (zipWith C.Eq es1 es2)
              simplify $ cs' ++ cs
        (_ :< WT.DataIntro _ consName1 dataArgs1 consArgs1, _ :< WT.DataIntro _ consName2 dataArgs2 consArgs2)
          | consName1 == consName2,
            length dataArgs1 == length dataArgs2,
            length consArgs1 == length consArgs2 -> do
              let es1 = dataArgs1 ++ consArgs1
              let es2 = dataArgs2 ++ consArgs2
              let cs' = map (,orig) (zipWith C.Eq es1 es2)
              simplify $ cs' ++ cs
        (_ :< WT.Noema t1, _ :< WT.Noema t2) ->
          simplify $ (C.Eq t1 t2, orig) : cs
        (_ :< WT.Embody t1 e1, _ :< WT.Embody t2 e2) ->
          simplify $ (C.Eq t1 t2, orig) : (C.Eq e1 e2, orig) : cs
        (_ :< WT.Prim a1, _ :< WT.Prim a2)
          | WP.Type t1 <- a1,
            WP.Type t2 <- a2,
            t1 == t2 ->
              simplify cs
          | WP.Value (WPV.Int t1 l1) <- a1,
            WP.Value (WPV.Int t2 l2) <- a2,
            l1 == l2 ->
              simplify $ (C.Eq t1 t2, orig) : cs
          | WP.Value (WPV.Float t1 l1) <- a1,
            WP.Value (WPV.Float t2 l2) <- a2,
            l1 == l2 ->
              simplify $ (C.Eq t1 t2, orig) : cs
          | WP.Value (WPV.Op op1) <- a1,
            WP.Value (WPV.Op op2) <- a2,
            op1 == op2 ->
              simplify cs
        (_ :< WT.ResourceType name1, _ :< WT.ResourceType name2)
          | name1 == name2 ->
              simplify cs
        (_ :< WT.Annotation _ _ e1, e2) ->
          simplify $ (C.Eq e1 e2, orig) : cs
        (e1, _ :< WT.Annotation _ _ e2) ->
          simplify $ (C.Eq e1 e2, orig) : cs
        (_ :< WT.Flow _ t1, _ :< WT.Flow _ t2) ->
          simplify $ (C.Eq t1 t2, orig) : cs
        (_ :< WT.FlowIntro _ _ (lam1, t1), _ :< WT.FlowIntro _ _ (lam2, t2)) ->
          simplify $ (C.Eq lam1 lam2, orig) : (C.Eq t1 t2, orig) : cs
        (_ :< WT.FlowElim _ _ (lam1, t1), _ :< WT.FlowElim _ _ (lam2, t2)) ->
          simplify $ (C.Eq lam1 lam2, orig) : (C.Eq t1 t2, orig) : cs
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
              simplify $ (C.Eq e1' e2', orig) : cs
            (Just (h1, (xs1, body1)), Nothing) -> do
              let s1 = HS.singleton h1 xs1 body1
              e1' <- fill s1 e1
              simplify $ (C.Eq e1' e2, orig) : cs
            (Nothing, Just (h2, (xs2, body2))) -> do
              let s2 = HS.singleton h2 xs2 body2
              e2' <- fill s2 e2
              simplify $ (C.Eq e1 e2', orig) : cs
            (Nothing, Nothing) -> do
              defMap <- WeakDefinition.read
              let fmvs = S.union fmvs1 fmvs2
              case (Stuck.asStuckedTerm e1, Stuck.asStuckedTerm e2) of
                (Just (Stuck.Hole h1 ies1, _ :< Stuck.Base), _)
                  | Just xss1 <- mapM asIdent ies1,
                    Just argSet1 <- toLinearIdentSet xss1,
                    h1 `S.notMember` fmvs2,
                    fvs2 `S.isSubsetOf` argSet1 ->
                      resolveHole h1 xss1 e2 cs
                (_, Just (Stuck.Hole h2 ies2, _ :< Stuck.Base))
                  | Just xss2 <- mapM asIdent ies2,
                    Just argSet2 <- toLinearIdentSet xss2,
                    h2 `S.notMember` fmvs1,
                    fvs1 `S.isSubsetOf` argSet2 ->
                      resolveHole h2 xss2 e1 cs
                (Just (Stuck.VarLocal x1, ctx1), Just (Stuck.VarLocal x2, ctx2))
                  | x1 == x2,
                    Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                      simplify $ map (,orig) pairList ++ cs
                (Just (Stuck.VarGlobal g1, ctx1), Just (Stuck.VarGlobal g2, ctx2))
                  | g1 == g2,
                    Nothing <- Map.lookup g1 defMap,
                    Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                      simplify $ map (,orig) pairList ++ cs
                  | g1 == g2,
                    Just lam <- Map.lookup g1 defMap ->
                      simplify $ (C.Eq (Stuck.resume lam ctx1) (Stuck.resume lam ctx2), orig) : cs
                  | Just lam1 <- Map.lookup g1 defMap,
                    Just lam2 <- Map.lookup g2 defMap ->
                      simplify $ (C.Eq (Stuck.resume lam1 ctx1) (Stuck.resume lam2 ctx2), orig) : cs
                (Just (Stuck.VarGlobal g1, ctx1), Just (Stuck.Hole {}, _))
                  | Just lam <- Map.lookup g1 defMap -> do
                      let uc = C.SuspendedConstraint (fmvs, C.Delta (C.Eq (Stuck.resume lam ctx1) e2), headConstraint)
                      insertConstraint uc
                      simplify cs
                (Just (Stuck.Hole {}, _), Just (Stuck.VarGlobal g2, ctx2))
                  | Just lam <- Map.lookup g2 defMap -> do
                      let uc = C.SuspendedConstraint (fmvs, C.Delta (C.Eq e1 (Stuck.resume lam ctx2)), headConstraint)
                      insertConstraint uc
                      simplify cs
                (Just (Stuck.VarGlobal g1, ctx1), _)
                  | Just lam <- Map.lookup g1 defMap ->
                      simplify $ (C.Eq (Stuck.resume lam ctx1) e2, orig) : cs
                (_, Just (Stuck.VarGlobal g2, ctx2))
                  | Just lam <- Map.lookup g2 defMap ->
                      simplify $ (C.Eq e1 (Stuck.resume lam ctx2), orig) : cs
                (Just (Stuck.Prim (WP.Value (WPV.Op op1)), ctx1), Just (Stuck.Prim (WP.Value (WPV.Op op2)), ctx2))
                  | op1 == op2,
                    Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                      simplify $ map (,orig) pairList ++ cs
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
      let sub' = IntMap.insert (Ident.toInt x2) (Right (m1 :< WT.Var x1)) sub
      rest <- simplifyBinder' orig sub' xts1 xts2
      return $ (C.Eq t1 t2', orig) : rest
    _ ->
      return []

asWeakBinder :: Hint -> WT.WeakTerm -> App (BinderF WT.WeakTerm)
asWeakBinder m t = do
  h <- Gensym.newIdentFromText "hole"
  return (m, h, t)

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

simplifyActual ::
  Hint ->
  S.Set DD.DefiniteDescription ->
  WT.WeakTerm ->
  C.Constraint ->
  App ()
simplifyActual m dataNameSet t orig = do
  t' <- reduce t
  case t' of
    _ :< WT.Tau ->
      return ()
    _ :< WT.Data (AttrD.Attr {..}) dataName dataArgs -> do
      let dataNameSet' = S.insert dataName dataNameSet
      forM_ dataArgs $ \dataArg ->
        simplifyActual m dataNameSet' dataArg orig
      dataConsArgsList <-
        if S.member dataName dataNameSet
          then return []
          else mapM (getConsArgTypes m) consNameList
      forM_ dataConsArgsList $ \dataConsArgs -> do
        dataConsArgs' <- substConsArgs IntMap.empty dataConsArgs
        forM_ dataConsArgs' $ \(_, _, consArg) -> do
          simplifyActual m dataNameSet' consArg orig
    _ :< WT.Prim {} ->
      return ()
    _ :< WT.ResourceType _ ->
      return ()
    _ -> do
      sub <- getHoleSubst
      let fmvs = holes t'
      case lookupAny (S.toList fmvs) sub of
        Just (h, (xs, body)) -> do
          let s = HS.singleton h xs body
          t'' <- fill s t'
          simplifyActual m dataNameSet t'' orig
        Nothing -> do
          defMap <- WeakDefinition.read
          case Stuck.asStuckedTerm t' of
            Just (Stuck.VarGlobal dd, ctx)
              | Just lam <- Map.lookup dd defMap -> do
                  simplifyActual m dataNameSet (Stuck.resume lam ctx) orig
              | otherwise ->
                  return ()
            _ -> do
              let uc = C.SuspendedConstraint (fmvs, C.Other, (C.Actual t', orig))
              insertConstraint uc

substConsArgs :: Subst.SubstWeakTerm -> [BinderF WT.WeakTerm] -> App [BinderF WT.WeakTerm]
substConsArgs sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, x, t) : rest -> do
      t' <- Subst.subst sub t
      let opaque = m :< WT.Tau -- allow `a` in `Cons(a: tau, x: a)`
      let sub' = IntMap.insert (Ident.toInt x) (Right opaque) sub
      rest' <- substConsArgs sub' rest
      return $ (m, x, t') : rest'

getConsArgTypes ::
  Hint ->
  DD.DefiniteDescription ->
  App [BinderF WT.WeakTerm]
getConsArgTypes m consName = do
  t <- Type.lookup m consName
  case t of
    _ :< WT.Pi xts _ -> do
      return xts
    _ ->
      Throw.raiseCritical m $ "the type of a constructor must be a Π-type, but it's not:\n" <> toText t
