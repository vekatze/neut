module Move.Scene.Elaborate.Unify
  ( Handle,
    new,
    unify,
    unifyCurrentConstraints,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.List (partition)
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (EIO, raiseCritical, toApp)
import Move.Context.Elaborate qualified as Elaborate
import Move.Context.Env qualified as Env
import Move.Context.Type qualified as Type
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Elaborate.Handle.Constraint qualified as Constraint
import Move.Scene.Elaborate.Handle.Hole qualified as Hole
import Move.Scene.WeakTerm.Fill qualified as Fill
import Move.Scene.WeakTerm.Reduce qualified as Reduce
import Move.Scene.WeakTerm.Subst qualified as Subst
import Rule.Attr.Data qualified as AttrD
import Rule.Attr.Lam qualified as AttrL
import Rule.Binder
import Rule.Const
import Rule.Constraint (SuspendedConstraint)
import Rule.Constraint qualified as C
import Rule.DefiniteDescription qualified as DD
import Rule.Error qualified as E
import Rule.Hint
import Rule.HoleID qualified as HID
import Rule.HoleSubst qualified as HS
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.LamKind qualified as LK
import Rule.Module
import Rule.PrimType qualified as PT
import Rule.Remark qualified as R
import Rule.Source
import Rule.Stuck qualified as Stuck
import Rule.WeakPrim qualified as WP
import Rule.WeakPrimValue qualified as WPV
import Rule.WeakTerm qualified as Subst
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.Eq qualified as WT
import Rule.WeakTerm.FreeVars
import Rule.WeakTerm.Holes
import Rule.WeakTerm.ToText

data Handle = Handle
  { reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle,
    fillHandle :: Fill.Handle,
    typeHandle :: Type.Handle,
    gensymHandle :: Gensym.Handle,
    constraintHandle :: Constraint.Handle,
    holeHandle :: Hole.Handle,
    inlineLimit :: Int,
    currentStep :: Int,
    weakDefHandle :: WeakDefinition.Handle
  }

new :: Elaborate.HandleEnv -> Gensym.Handle -> App Handle
new Elaborate.HandleEnv {..} gensymHandle = do
  reduceHandle <- Reduce.new
  substHandle <- Subst.new
  fillHandle <- Fill.new
  typeHandle <- Type.new
  envHandle <- Env.new
  source <- toApp $ Env.getCurrentSource envHandle
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule source)
  weakDefHandle <- WeakDefinition.new gensymHandle
  let currentStep = 0
  return $ Handle {..}

unify :: Handle -> [C.Constraint] -> EIO HS.HoleSubst
unify h constraintList = do
  susList <- unify' h (reverse constraintList)
  case susList of
    [] ->
      liftIO $ Hole.getSubst (holeHandle h)
    _ ->
      throwTypeErrors h susList

unifyCurrentConstraints :: Handle -> EIO HS.HoleSubst
unifyCurrentConstraints h = do
  susList <- liftIO $ Constraint.getSuspendedConstraints (constraintHandle h)
  cs <- liftIO $ Constraint.get (constraintHandle h)
  susList' <- simplify h susList $ zip cs cs
  liftIO $ Constraint.set (constraintHandle h) []
  liftIO $ Constraint.setSuspendedConstraints (constraintHandle h) susList'
  liftIO $ Hole.getSubst (holeHandle h)

unify' :: Handle -> [C.Constraint] -> EIO [SuspendedConstraint]
unify' h constraintList = do
  susList <- liftIO $ Constraint.getSuspendedConstraints (constraintHandle h)
  simplify h susList $ zip constraintList constraintList

throwTypeErrors :: Handle -> [SuspendedConstraint] -> EIO a
throwTypeErrors h susList = do
  sub <- liftIO $ Hole.getSubst (holeHandle h)
  errorList <- mapM (\(C.SuspendedConstraint (_, (_, c))) -> constraintToRemark h sub c) susList
  throwError $ E.MakeError errorList

constraintToRemark :: Handle -> HS.HoleSubst -> C.Constraint -> EIO R.Remark
constraintToRemark h sub c = do
  case c of
    C.Actual t -> do
      t' <- fillAsMuchAsPossible h sub t
      return $ R.newRemark (WT.metaOf t) R.Error $ constructErrorMessageActual t'
    C.Integer t -> do
      t' <- fillAsMuchAsPossible h sub t
      return $ R.newRemark (WT.metaOf t) R.Error $ constructErrorMessageInteger t'
    C.Eq expected actual -> do
      expected' <- fillAsMuchAsPossible h sub expected
      actual' <- fillAsMuchAsPossible h sub actual
      return $ R.newRemark (WT.metaOf actual) R.Error $ constructErrorMessageEq actual' expected'

fillAsMuchAsPossible :: Handle -> HS.HoleSubst -> WT.WeakTerm -> EIO WT.WeakTerm
fillAsMuchAsPossible h sub e = do
  e' <- Reduce.reduce (reduceHandle h) e
  if HS.fillable e' sub
    then Fill.fill (fillHandle h) sub e' >>= fillAsMuchAsPossible h sub
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

detectPossibleInfiniteLoop :: Handle -> C.Constraint -> EIO ()
detectPossibleInfiniteLoop h c = do
  let Handle {inlineLimit, currentStep} = h
  when (inlineLimit < currentStep) $ do
    sub <- liftIO $ Hole.getSubst (holeHandle h)
    r <- constraintToRemark h sub c
    throwError $
      E.MakeError
        [ R.attachSuffix r $
            "\n(Exceeded max recursion depth of "
              <> T.pack (show inlineLimit)
              <> " during unification)"
        ]

simplify :: Handle -> [SuspendedConstraint] -> [(C.Constraint, C.Constraint)] -> EIO [SuspendedConstraint]
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
      expected' <- Reduce.reduce (reduceHandle h) expected
      actual' <- Reduce.reduce (reduceHandle h) actual
      if WT.eq expected' actual'
        then simplify h susList cs
        else do
          case (expected', actual') of
            (m1 :< WT.Pi impArgs1 expArgs1 cod1, m2 :< WT.Pi impArgs2 expArgs2 cod2)
              | length impArgs1 == length impArgs2,
                length expArgs1 == length expArgs2 -> do
                  xt1 <- liftIO $ asWeakBinder h m1 cod1
                  xt2 <- liftIO $ asWeakBinder h m2 cod2
                  cs' <- liftIO $ simplifyBinder h orig (impArgs1 ++ expArgs1 ++ [xt1]) (impArgs2 ++ expArgs2 ++ [xt2])
                  simplify h susList $ cs' ++ cs
            (m1 :< WT.PiIntro kind1 impArgs1 expArgs1 e1, m2 :< WT.PiIntro kind2 impArgs2 expArgs2 e2)
              | AttrL.Attr {lamKind = LK.Fix xt1@(_, x1, _)} <- kind1,
                AttrL.Attr {lamKind = LK.Fix xt2@(_, x2, _)} <- kind2,
                x1 == x2,
                length impArgs1 == length impArgs2,
                length expArgs1 == length expArgs2 -> do
                  yt1 <- liftIO $ asWeakBinder h m1 e1
                  yt2 <- liftIO $ asWeakBinder h m2 e2
                  cs' <- liftIO $ simplifyBinder h orig (xt1 : impArgs1 ++ expArgs1 ++ [yt1]) (xt2 : impArgs2 ++ expArgs2 ++ [yt2])
                  simplify h susList $ cs' ++ cs
              | AttrL.Attr {lamKind = LK.Normal codType1} <- kind1,
                AttrL.Attr {lamKind = LK.Normal codType2} <- kind2,
                length impArgs1 == length impArgs2,
                length expArgs1 == length expArgs2 -> do
                  cod1 <- liftIO $ asWeakBinder h m1 codType1
                  xt1 <- liftIO $ asWeakBinder h m1 e1
                  cod2 <- liftIO $ asWeakBinder h m2 codType2
                  xt2 <- liftIO $ asWeakBinder h m2 e2
                  cs' <- liftIO $ simplifyBinder h orig (impArgs1 ++ expArgs1 ++ [cod1, xt1]) (impArgs2 ++ expArgs2 ++ [cod2, xt2])
                  simplify h susList $ cs' ++ cs
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
                  e1' <- Fill.fill (fillHandle h) s1 e1
                  e2' <- Fill.fill (fillHandle h) s2 e2
                  simplify h susList $ (C.Eq e1' e2', orig) : cs
                (Just (hole1, (xs1, body1)), Nothing) -> do
                  let s1 = HS.singleton hole1 xs1 body1
                  e1' <- Fill.fill (fillHandle h) s1 e1
                  simplify h susList $ (C.Eq e1' e2, orig) : cs
                (Nothing, Just (hole2, (xs2, body2))) -> do
                  let s2 = HS.singleton hole2 xs2 body2
                  e2' <- Fill.fill (fillHandle h) s2 e2
                  simplify h susList $ (C.Eq e1 e2', orig) : cs
                (Nothing, Nothing) -> do
                  let fmvs = S.union fmvs1 fmvs2
                  defMap <- liftIO $ WeakDefinition.read' (weakDefHandle h)
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
  EIO [SuspendedConstraint]
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
  EIO [SuspendedConstraint]
simplifyActual h m dataNameSet t orig = do
  detectPossibleInfiniteLoop h orig
  t' <- Reduce.reduce (reduceHandle h) t
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
          t'' <- Fill.fill (fillHandle h) s t'
          simplifyActual h m dataNameSet t'' orig
        Nothing -> do
          defMap <- liftIO $ WeakDefinition.read' (weakDefHandle h)
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
  EIO [BinderF WT.WeakTerm]
getConsArgTypes h m consName = do
  t <- Type.lookup' (typeHandle h) m consName
  case t of
    _ :< WT.Pi impArgs expArgs _ -> do
      return $ impArgs ++ expArgs
    _ ->
      raiseCritical m $ "The type of a constructor must be a Π-type, but it's not:\n" <> toText t

simplifyInteger ::
  Handle ->
  Hint ->
  WT.WeakTerm ->
  C.Constraint ->
  EIO [SuspendedConstraint]
simplifyInteger h m t orig = do
  detectPossibleInfiniteLoop h orig
  t' <- Reduce.reduce (reduceHandle h) t
  case t' of
    _ :< WT.Prim (WP.Type (PT.Int _)) -> do
      return []
    _ -> do
      sub <- liftIO $ Hole.getSubst (holeHandle h)
      let fmvs = holes t'
      case lookupAny (S.toList fmvs) sub of
        Just (hole, (xs, body)) -> do
          t'' <- Fill.fill (fillHandle h) (HS.singleton hole xs body) t'
          simplifyInteger h m t'' orig
        Nothing -> do
          defMap <- liftIO $ WeakDefinition.read' (weakDefHandle h)
          case Stuck.asStuckedTerm t' of
            Just (Stuck.VarGlobal dd, evalCtx)
              | Just lam <- Map.lookup dd defMap -> do
                  simplifyInteger (increment h) m (Stuck.resume lam evalCtx) orig
            _ -> do
              return [C.SuspendedConstraint (fmvs, (C.Integer t', orig))]
