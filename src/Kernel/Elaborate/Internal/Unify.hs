module Kernel.Elaborate.Internal.Unify
  ( unify,
    unifyCurrentConstraints,
  )
where

import App.App (App)
import App.Error qualified as E
import App.Run (raiseCritical)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.List (partition)
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Elaborate.Constraint (SuspendedConstraint)
import Kernel.Elaborate.Constraint qualified as C
import Kernel.Elaborate.HoleSubst qualified as HS
import Kernel.Elaborate.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Internal.Handle.Elaborate
import Kernel.Elaborate.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Stuck qualified as Stuck
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LamKind qualified as LK
import Language.Common.PiKind qualified as PK
import Language.Common.PrimType qualified as PT
import Language.WeakTerm.Eq qualified as WT
import Language.WeakTerm.FreeVars
import Language.WeakTerm.Holes
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText
import Language.WeakTerm.ToText qualified as WT
import Language.WeakTerm.WeakPrim qualified as WP
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as Subst
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint
import Logger.Log qualified as L
import Logger.LogLevel qualified as L

unify :: Handle -> [C.Constraint] -> App HS.HoleSubst
unify h constraintList = do
  susList <- unify' h (reverse constraintList)
  case susList of
    [] ->
      liftIO $ Hole.getSubst (holeHandle h)
    _ ->
      throwTypeErrors h susList

unifyCurrentConstraints :: Handle -> App HS.HoleSubst
unifyCurrentConstraints h = do
  susList <- liftIO $ Constraint.getSuspendedConstraints (constraintHandle h)
  cs <- liftIO $ Constraint.get (constraintHandle h)
  susList' <- simplify h susList $ zip cs cs
  liftIO $ Constraint.set (constraintHandle h) []
  liftIO $ Constraint.setSuspendedConstraints (constraintHandle h) susList'
  liftIO $ Hole.getSubst (holeHandle h)

unify' :: Handle -> [C.Constraint] -> App [SuspendedConstraint]
unify' h constraintList = do
  susList <- liftIO $ Constraint.getSuspendedConstraints (constraintHandle h)
  simplify h susList $ zip constraintList constraintList

throwTypeErrors :: Handle -> [SuspendedConstraint] -> App a
throwTypeErrors h susList = do
  sub <- liftIO $ Hole.getSubst (holeHandle h)
  errorList <- mapM (\(C.SuspendedConstraint (_, (_, c))) -> constraintToRemark h sub c) susList
  throwError $ E.MakeError errorList

constraintToRemark :: Handle -> HS.HoleSubst -> C.Constraint -> App L.Log
constraintToRemark h sub c = do
  case c of
    C.Actual t -> do
      t' <- fillAsMuchAsPossible h sub t
      return $ L.newLog (WT.metaOf t) L.Error $ constructErrorMessageActual t'
    C.Integer t -> do
      t' <- fillAsMuchAsPossible h sub t
      return $ L.newLog (WT.metaOf t) L.Error $ constructErrorMessageInteger t'
    C.Eq expected actual -> do
      expected' <- fillAsMuchAsPossible h sub expected
      actual' <- fillAsMuchAsPossible h sub actual
      return $ L.newLog (WT.metaOf actual) L.Error $ constructErrorMessageEq actual' expected'

fillAsMuchAsPossible :: Handle -> HS.HoleSubst -> WT.WeakTerm -> App WT.WeakTerm
fillAsMuchAsPossible h sub e = do
  e' <- reduce h e
  if HS.fillable e' sub
    then fill h sub e' >>= fillAsMuchAsPossible h sub
    else return e'

constructErrorMessageEq :: WT.WeakTerm -> WT.WeakTerm -> T.Text
constructErrorMessageEq found expected =
  "Expected:\n  "
    <> toText expected
    <> "\nFound:\n  "
    <> toText found

constructErrorMessageActual :: WT.WeakTerm -> T.Text
constructErrorMessageActual t =
  "A term of the following type might be noetic:\n  "
    <> toText t

constructErrorMessageInteger :: WT.WeakTerm -> T.Text
constructErrorMessageInteger t =
  "Expected:\n  "
    <> "an integer type"
    <> "\nFound:\n  "
    <> toText t

increment :: Handle -> Handle
increment h = do
  h {currentStep = currentStep h + 1}

detectPossibleInfiniteLoop :: Handle -> C.Constraint -> App ()
detectPossibleInfiniteLoop h c = do
  let Handle {inlineLimit, currentStep} = h
  when (inlineLimit < currentStep) $ do
    sub <- liftIO $ Hole.getSubst (holeHandle h)
    l <- constraintToRemark h sub c
    let suffix =
          "\n(Exceeded max recursion depth of "
            <> T.pack (show inlineLimit)
            <> " during unification)"
    throwError $ E.MakeError [l {L.content = L.content l <> suffix}]

simplify :: Handle -> [SuspendedConstraint] -> [(C.Constraint, C.Constraint)] -> App [SuspendedConstraint]
simplify h susList constraintList =
  case constraintList of
    [] ->
      return susList
    (C.Actual t, orig) : cs -> do
      susList' <- simplifyActual h (WT.metaOf t) S.empty t orig
      simplify h (susList' ++ susList) cs
    (C.Integer t, orig) : cs -> do
      susList' <- simplifyInteger h (WT.metaOf t) t orig
      simplify h (susList' ++ susList) cs
    headConstraint@(C.Eq expected actual, orig) : cs -> do
      detectPossibleInfiniteLoop h orig
      expected' <- reduce h expected
      actual' <- reduce h actual
      if WT.eq expected' actual'
        then simplify h susList cs
        else do
          case (expected', actual') of
            (m1 :< WT.Pi _ impArgs1 expArgs1 cod1, m2 :< WT.Pi _ impArgs2 expArgs2 cod2)
              | Just (impBinders, impConstraints) <- createDefaultConstraints impArgs1 impArgs2,
                length expArgs1 == length expArgs2 -> do
                  xt1 <- liftIO $ asWeakBinder h m1 cod1
                  xt2 <- liftIO $ asWeakBinder h m2 cod2
                  let (impBinders1, impBinders2) = unzip impBinders
                  let impEqs' = map (,orig) impConstraints
                  cs' <- liftIO $ simplifyBinder h orig (impBinders1 ++ expArgs1 ++ [xt1]) (impBinders2 ++ expArgs2 ++ [xt2])
                  simplify h susList $ cs' ++ impEqs' ++ cs
            (m1 :< WT.PiIntro kind1 impArgs1 expArgs1 e1, m2 :< WT.PiIntro kind2 impArgs2 expArgs2 e2)
              | AttrL.Attr {lamKind = LK.Fix xt1@(_, x1, _)} <- kind1,
                AttrL.Attr {lamKind = LK.Fix xt2@(_, x2, _)} <- kind2,
                x1 == x2,
                Just (impBinders, impConstraints) <- createDefaultConstraints impArgs1 impArgs2,
                length expArgs1 == length expArgs2 -> do
                  yt1 <- liftIO $ asWeakBinder h m1 e1
                  yt2 <- liftIO $ asWeakBinder h m2 e2
                  let (impBinders1, impBinders2) = unzip impBinders
                  let impEqs' = map (,orig) impConstraints
                  cs' <- liftIO $ simplifyBinder h orig (xt1 : impBinders1 ++ expArgs1 ++ [yt1]) (xt2 : impBinders2 ++ expArgs2 ++ [yt2])
                  simplify h susList $ cs' ++ impEqs' ++ cs
              | AttrL.Attr {lamKind = LK.Normal _ codType1} <- kind1,
                AttrL.Attr {lamKind = LK.Normal _ codType2} <- kind2,
                Just (impBinders, impConstraints) <- createDefaultConstraints impArgs1 impArgs2,
                length expArgs1 == length expArgs2 -> do
                  cod1 <- liftIO $ asWeakBinder h m1 codType1
                  xt1 <- liftIO $ asWeakBinder h m1 e1
                  cod2 <- liftIO $ asWeakBinder h m2 codType2
                  xt2 <- liftIO $ asWeakBinder h m2 e2
                  let (impBinders1, impBinders2) = unzip impBinders
                  let impEqs' = map (,orig) impConstraints
                  cs' <- liftIO $ simplifyBinder h orig (impBinders1 ++ expArgs1 ++ [cod1, xt1]) (impBinders2 ++ expArgs2 ++ [cod2, xt2])
                  simplify h susList $ cs' ++ impEqs' ++ cs
            (_ :< WT.Data _ name1 es1, _ :< WT.Data _ name2 es2)
              | name1 == name2,
                length es1 == length es2 -> do
                  let cs' = map (,orig) (zipWith C.Eq es1 es2)
                  simplify h susList $ cs' ++ cs
            (_ :< WT.DataIntro _ consName1 dataArgs1 consArgs1, _ :< WT.DataIntro _ consName2 dataArgs2 consArgs2)
              | consName1 == consName2,
                length dataArgs1 == length dataArgs2,
                length consArgs1 == length consArgs2 -> do
                  let es1 = dataArgs1 ++ consArgs1
                  let es2 = dataArgs2 ++ consArgs2
                  let cs' = map (,orig) (zipWith C.Eq es1 es2)
                  simplify h susList $ cs' ++ cs
            (_ :< WT.Box t1, _ :< WT.Box t2) ->
              simplify h susList $ (C.Eq t1 t2, orig) : cs
            (_ :< WT.BoxNoema t1, _ :< WT.BoxNoema t2) ->
              simplify h susList $ (C.Eq t1 t2, orig) : cs
            (_ :< WT.BoxIntro letSeq1 e1, _ :< WT.BoxIntro letSeq2 e2)
              | length letSeq1 == length letSeq2 -> do
                  let (xts1, es1) = unzip letSeq1
                  let (xts2, es2) = unzip letSeq2
                  cs' <- liftIO $ simplifyBinder h orig xts1 xts2
                  let cs'' = map (orig,) $ zipWith C.Eq es1 es2
                  simplify h susList $ (C.Eq e1 e2, orig) : cs' ++ cs'' ++ cs
            (_ :< WT.Annotation _ _ e1, e2) ->
              simplify h susList $ (C.Eq e1 e2, orig) : cs
            (e1, _ :< WT.Annotation _ _ e2) ->
              simplify h susList $ (C.Eq e1 e2, orig) : cs
            (e1, e2) -> do
              sub <- liftIO $ Hole.getSubst (holeHandle h)
              let fvs1 = freeVars e1
              let fvs2 = freeVars e2
              let fmvs1 = holes e1 -- fmvs: free meta-variables
              let fmvs2 = holes e2
              case (lookupAny (S.toList fmvs1) sub, lookupAny (S.toList fmvs2) sub) of
                (Just (hole1, (xs1, body1)), Just (hole2, (xs2, body2))) -> do
                  let s1 = HS.singleton hole1 xs1 body1
                  let s2 = HS.singleton hole2 xs2 body2
                  e1' <- fill h s1 e1
                  e2' <- fill h s2 e2
                  simplify h susList $ (C.Eq e1' e2', orig) : cs
                (Just (hole1, (xs1, body1)), Nothing) -> do
                  let s1 = HS.singleton hole1 xs1 body1
                  e1' <- fill h s1 e1
                  simplify h susList $ (C.Eq e1' e2, orig) : cs
                (Nothing, Just (hole2, (xs2, body2))) -> do
                  let s2 = HS.singleton hole2 xs2 body2
                  e2' <- fill h s2 e2
                  simplify h susList $ (C.Eq e1 e2', orig) : cs
                (Nothing, Nothing) -> do
                  let fmvs = S.union fmvs1 fmvs2
                  defMap <- liftIO $ WeakDef.read' (weakDefHandle h)
                  case (Stuck.asStuckedTerm e1, Stuck.asStuckedTerm e2) of
                    (Just (Stuck.Hole hole1 ies1, _ :< Stuck.Base), _)
                      | Just xss1 <- mapM asIdent ies1,
                        Just argSet1 <- toLinearIdentSet xss1,
                        hole1 `S.notMember` fmvs2,
                        fvs2 `S.isSubsetOf` argSet1 ->
                          resolveHole h susList hole1 xss1 e2 cs
                    (_, Just (Stuck.Hole hole2 ies2, _ :< Stuck.Base))
                      | Just xss2 <- mapM asIdent ies2,
                        Just argSet2 <- toLinearIdentSet xss2,
                        hole2 `S.notMember` fmvs1,
                        fvs1 `S.isSubsetOf` argSet2 ->
                          resolveHole h susList hole2 xss2 e1 cs
                    (Just (Stuck.VarLocal x1, ctx1), Just (Stuck.VarLocal x2, ctx2))
                      | x1 == x2,
                        Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                          simplify h susList $ map (,orig) pairList ++ cs
                    (Just (Stuck.VarGlobal g1, ctx1), Just (Stuck.VarGlobal g2, ctx2))
                      | g1 == g2,
                        Nothing <- Map.lookup g1 defMap,
                        Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                          simplify h susList $ map (,orig) pairList ++ cs
                      | g1 == g2,
                        Just lam <- Map.lookup g1 defMap -> do
                          let h' = increment h
                          simplify h' susList $ (C.Eq (Stuck.resume lam ctx1) (Stuck.resume lam ctx2), orig) : cs
                      | Just lam1 <- Map.lookup g1 defMap,
                        Just lam2 <- Map.lookup g2 defMap -> do
                          let h' = increment h
                          simplify h' susList $ (C.Eq (Stuck.resume lam1 ctx1) (Stuck.resume lam2 ctx2), orig) : cs
                    (Just (Stuck.VarGlobal g1, ctx1), _)
                      | Just lam <- Map.lookup g1 defMap -> do
                          let h' = increment h
                          simplify h' susList $ (C.Eq (Stuck.resume lam ctx1) e2, orig) : cs
                    (_, Just (Stuck.VarGlobal g2, ctx2))
                      | Just lam <- Map.lookup g2 defMap -> do
                          let h' = increment h
                          simplify h' susList $ (C.Eq e1 (Stuck.resume lam ctx2), orig) : cs
                    (Just (Stuck.Prim (WP.Value (WPV.Op op1)), ctx1), Just (Stuck.Prim (WP.Value (WPV.Op op2)), ctx2))
                      | op1 == op2,
                        Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                          simplify h susList $ map (,orig) pairList ++ cs
                    _ -> do
                      let uc = C.SuspendedConstraint (fmvs, headConstraint)
                      simplify h (uc : susList) cs

{-# INLINE resolveHole #-}
resolveHole ::
  Handle ->
  [SuspendedConstraint] ->
  HID.HoleID ->
  [Ident] ->
  WT.WeakTerm ->
  [(C.Constraint, C.Constraint)] ->
  App [SuspendedConstraint]
resolveHole h susList hole1 xs e2' cs = do
  liftIO $ Hole.insertSubst (holeHandle h) hole1 xs e2'
  let (susList1, susList2) = partition (\(C.SuspendedConstraint (hs, _)) -> S.member hole1 hs) susList
  let susList1' = map (\(C.SuspendedConstraint (_, c)) -> c) susList1
  simplify h susList2 $ reverse susList1' ++ cs

simplifyBinder ::
  Handle ->
  C.Constraint ->
  [BinderF WT.WeakTerm] ->
  [BinderF WT.WeakTerm] ->
  IO [(C.Constraint, C.Constraint)]
simplifyBinder h orig =
  simplifyBinder' h orig IntMap.empty

simplifyBinder' ::
  Handle ->
  C.Constraint ->
  WT.SubstWeakTerm ->
  [BinderF WT.WeakTerm] ->
  [BinderF WT.WeakTerm] ->
  IO [(C.Constraint, C.Constraint)]
simplifyBinder' h orig sub args1 args2 =
  case (args1, args2) of
    ((m1, x1, t1) : xts1, (_, x2, t2) : xts2) -> do
      t2' <- liftIO $ Subst.subst (substHandle h) sub t2
      let sub' = IntMap.insert (Ident.toInt x2) (Right (m1 :< WT.Var x1)) sub
      rest <- simplifyBinder' h orig sub' xts1 xts2
      return $ (C.Eq t1 t2', orig) : rest
    _ ->
      return []

asWeakBinder :: Handle -> Hint -> WT.WeakTerm -> IO (BinderF WT.WeakTerm)
asWeakBinder h m t = do
  x <- Gensym.newIdentFromText (gensymHandle h) "hole"
  return (m, x, t)

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
  Handle ->
  Hint ->
  S.Set DD.DefiniteDescription ->
  WT.WeakTerm ->
  C.Constraint ->
  App [SuspendedConstraint]
simplifyActual h m dataNameSet t orig = do
  detectPossibleInfiniteLoop h orig
  t' <- reduce h t
  case t' of
    _ :< WT.Tau -> do
      return []
    _ :< WT.Data (AttrD.Attr {consNameList}) dataName dataArgs -> do
      let dataNameSet' = S.insert dataName dataNameSet
      constraintsFromDataArgs <- fmap concat $ forM dataArgs $ \dataArg ->
        simplifyActual h m dataNameSet' dataArg orig
      dataConsArgsList <-
        if S.member dataName dataNameSet
          then return []
          else mapM (getConsArgTypes h m . fst) consNameList
      constraintsFromDataConsArgs <- fmap concat $ forM dataConsArgsList $ \dataConsArgs -> do
        dataConsArgs' <- liftIO $ substConsArgs h IntMap.empty dataConsArgs
        fmap concat $ forM dataConsArgs' $ \(_, _, consArg) -> do
          simplifyActual h m dataNameSet' consArg orig
      return $ constraintsFromDataArgs ++ constraintsFromDataConsArgs
    _ :< WT.Box t'' -> do
      simplifyActual h m dataNameSet t'' orig
    _ :< WT.Prim {} -> do
      return []
    _ :< WT.Void -> do
      return []
    _ :< WT.Resource {} -> do
      return []
    _ -> do
      sub <- liftIO $ Hole.getSubst (holeHandle h)
      let fmvs = holes t'
      case lookupAny (S.toList fmvs) sub of
        Just (hole, (xs, body)) -> do
          let s = HS.singleton hole xs body
          t'' <- fill h s t'
          simplifyActual h m dataNameSet t'' orig
        Nothing -> do
          defMap <- liftIO $ WeakDef.read' (weakDefHandle h)
          case Stuck.asStuckedTerm t' of
            Just (Stuck.VarGlobal dd, evalCtx)
              | Just lam <- Map.lookup dd defMap -> do
                  simplifyActual (increment h) m dataNameSet (Stuck.resume lam evalCtx) orig
            _ -> do
              return [C.SuspendedConstraint (fmvs, (C.Actual t', orig))]

substConsArgs :: Handle -> Subst.SubstWeakTerm -> [BinderF WT.WeakTerm] -> IO [BinderF WT.WeakTerm]
substConsArgs h sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, x, t) : rest -> do
      t' <- Subst.subst (substHandle h) sub t
      let opaque = m :< WT.Tau -- allow `a` in `Cons(a: type, x: a)`
      let sub' = IntMap.insert (Ident.toInt x) (Right opaque) sub
      rest' <- substConsArgs h sub' rest
      return $ (m, x, t') : rest'

getConsArgTypes ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  App [BinderF WT.WeakTerm]
getConsArgTypes h m consName = do
  t <- Type.lookup' (typeHandle h) m consName
  case t of
    _ :< WT.Pi (PK.DataIntro False) impArgs expArgs (_ :< WT.Pi (PK.Normal _) impArgs' expArgs' _dataType) -> do
      return $ map fst impArgs ++ expArgs ++ map fst impArgs' ++ expArgs'
    _ :< WT.Pi (PK.DataIntro True) impArgs expArgs _dataType -> do
      return $ map fst impArgs ++ expArgs
    _ ->
      raiseCritical m $ "Got a malformed constructor type:\n" <> WT.toText t

simplifyInteger ::
  Handle ->
  Hint ->
  WT.WeakTerm ->
  C.Constraint ->
  App [SuspendedConstraint]
simplifyInteger h m t orig = do
  detectPossibleInfiniteLoop h orig
  t' <- reduce h t
  case t' of
    _ :< WT.Prim (WP.Type (PT.Int _)) -> do
      return []
    _ -> do
      sub <- liftIO $ Hole.getSubst (holeHandle h)
      let fmvs = holes t'
      case lookupAny (S.toList fmvs) sub of
        Just (hole, (xs, body)) -> do
          t'' <- fill h (HS.singleton hole xs body) t'
          simplifyInteger h m t'' orig
        Nothing -> do
          defMap <- liftIO $ WeakDef.read' (weakDefHandle h)
          case Stuck.asStuckedTerm t' of
            Just (Stuck.VarGlobal dd, evalCtx)
              | Just lam <- Map.lookup dd defMap -> do
                  simplifyInteger (increment h) m (Stuck.resume lam evalCtx) orig
            _ -> do
              return [C.SuspendedConstraint (fmvs, (C.Integer t', orig))]

createDefaultConstraints ::
  [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)] ->
  [(BinderF WT.WeakTerm, Maybe WT.WeakTerm)] ->
  Maybe ([(BinderF WT.WeakTerm, BinderF WT.WeakTerm)], [C.Constraint])
createDefaultConstraints impArgs1 impArgs2 = do
  case (impArgs1, impArgs2) of
    ([], []) ->
      Just ([], [])
    (_ : _, []) ->
      trace "found mismatch" Nothing
    ([], _ : _) ->
      trace "found mismatch" Nothing
    ((binder1, me1) : rest1, (binder2, me2) : rest2) -> do
      case (me1, me2) of
        (Nothing, Nothing) -> do
          (binders, cs) <- createDefaultConstraints rest1 rest2
          return ((binder1, binder2) : binders, cs)
        (Just _, Nothing) ->
          Nothing
        (Nothing, Just _) ->
          Nothing
        (Just e1, Just e2) -> do
          (binders, cs) <- createDefaultConstraints rest1 rest2
          return ((binder1, binder2) : binders, C.Eq e1 e2 : cs)
