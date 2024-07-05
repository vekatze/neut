module Scene.Elaborate.Unify
  ( unify,
    unifyCurrentConstraints,
  )
where

import Context.App
import Context.Elaborate
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.OptimizableData qualified as OptimizableData
import Context.Throw qualified as Throw
import Context.Type qualified as Type
import Context.WeakDefinition qualified as WeakDefinition
import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.List (partition)
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.Const
import Entity.Constraint (SuspendedConstraint)
import Entity.Constraint qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.Error qualified as E
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.HoleSubst qualified as HS
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Module
import Entity.OptimizableData qualified as OD
import Entity.Remark qualified as R
import Entity.Source
import Entity.Stuck qualified as Stuck
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as Subst
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.Eq qualified as WT
import Entity.WeakTerm.FreeVars
import Entity.WeakTerm.Holes
import Entity.WeakTerm.ToText
import Scene.WeakTerm.Fill
import Scene.WeakTerm.Reduce
import Scene.WeakTerm.Subst qualified as Subst

unify :: [C.Constraint] -> App HS.HoleSubst
unify constraintList = do
  susList <- unify' (reverse constraintList)
  case susList of
    [] ->
      getHoleSubst
    _ ->
      throwTypeErrors susList

unifyCurrentConstraints :: App HS.HoleSubst
unifyCurrentConstraints = do
  susList <- getSuspendedEnv
  cs <- getConstraintEnv
  ax <- newAxis
  susList' <- simplify ax susList $ zip cs cs
  setConstraintEnv []
  setSuspendedEnv susList'
  getHoleSubst

unify' :: [C.Constraint] -> App [SuspendedConstraint]
unify' constraintList = do
  susList <- getSuspendedEnv
  ax <- newAxis
  simplify ax susList $ zip constraintList constraintList

throwTypeErrors :: [SuspendedConstraint] -> App a
throwTypeErrors susList = do
  sub <- getHoleSubst
  errorList <- forM susList $ \(C.SuspendedConstraint (_, (_, c))) -> do
    case c of
      C.Actual t -> do
        t' <- fillAsMuchAsPossible sub t
        return $ R.newRemark (WT.metaOf t) R.Error $ constructErrorMessageActual t'
      C.Affine t -> do
        t' <- fillAsMuchAsPossible sub t
        return $ R.newRemark (WT.metaOf t) R.Error $ constructErrorMessageAffine t'
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
constructErrorMessageEq found expected =
  "Expected:\n  "
    <> toText expected
    <> "\nFound:\n  "
    <> toText found

constructErrorMessageActual :: WT.WeakTerm -> T.Text
constructErrorMessageActual t =
  "A term of the following type might be noetic:\n"
    <> toText t

constructErrorMessageAffine :: WT.WeakTerm -> T.Text
constructErrorMessageAffine t =
  "The type of this affine variable is not affine, but:\n"
    <> toText t

data Axis = Axis
  { inlineLimit :: Int,
    currentStep :: Int,
    defMap :: WeakDefinition.DefMap
  }

newAxis :: App Axis
newAxis = do
  source <- Env.getCurrentSource
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule source)
  defMap <- WeakDefinition.read
  return
    Axis
      { inlineLimit = inlineLimit,
        currentStep = 0,
        defMap = defMap
      }

increment :: Axis -> Axis
increment ax = do
  ax {currentStep = currentStep ax + 1}

detectPossibleInfiniteLoop :: Hint -> Axis -> App ()
detectPossibleInfiniteLoop location axis = do
  let Axis {inlineLimit, currentStep} = axis
  when (inlineLimit < currentStep) $ do
    Throw.raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit) <> " during unification"

simplify :: Axis -> [SuspendedConstraint] -> [(C.Constraint, C.Constraint)] -> App [SuspendedConstraint]
simplify ax susList constraintList =
  case constraintList of
    [] ->
      return susList
    (C.Actual t, orig) : cs -> do
      detectPossibleInfiniteLoop (C.getLoc orig) ax
      susList' <- simplifyActual (WT.metaOf t) S.empty t orig
      simplify ax (susList' ++ susList) cs
    (C.Affine t, orig) : cs -> do
      detectPossibleInfiniteLoop (C.getLoc orig) ax
      susList' <- simplifyAffine (WT.metaOf t) S.empty t orig
      simplify ax (susList' ++ susList) cs
    headConstraint@(C.Eq expected actual, orig) : cs -> do
      detectPossibleInfiniteLoop (C.getLoc orig) ax
      expected' <- reduce expected
      actual' <- reduce actual
      if WT.eq expected' actual'
        then simplify ax susList cs
        else do
          case (expected', actual') of
            (m1 :< WT.Pi impArgs1 expArgs1 cod1, m2 :< WT.Pi impArgs2 expArgs2 cod2)
              | length impArgs1 == length impArgs2,
                length expArgs1 == length expArgs2 -> do
                  xt1 <- asWeakBinder m1 cod1
                  xt2 <- asWeakBinder m2 cod2
                  cs' <- simplifyBinder orig (impArgs1 ++ expArgs1 ++ [xt1]) (impArgs2 ++ expArgs2 ++ [xt2])
                  simplify ax susList $ cs' ++ cs
            (m1 :< WT.PiIntro kind1 impArgs1 expArgs1 e1, m2 :< WT.PiIntro kind2 impArgs2 expArgs2 e2)
              | AttrL.Attr {lamKind = LK.Fix xt1@(_, x1, _)} <- kind1,
                AttrL.Attr {lamKind = LK.Fix xt2@(_, x2, _)} <- kind2,
                x1 == x2,
                length impArgs1 == length impArgs2,
                length expArgs1 == length expArgs2 -> do
                  yt1 <- asWeakBinder m1 e1
                  yt2 <- asWeakBinder m2 e2
                  cs' <- simplifyBinder orig (xt1 : impArgs1 ++ expArgs1 ++ [yt1]) (xt2 : impArgs2 ++ expArgs2 ++ [yt2])
                  simplify ax susList $ cs' ++ cs
              | AttrL.Attr {lamKind = LK.Normal codType1} <- kind1,
                AttrL.Attr {lamKind = LK.Normal codType2} <- kind2,
                length impArgs1 == length impArgs2,
                length expArgs1 == length expArgs2 -> do
                  cod1 <- asWeakBinder m1 codType1
                  xt1 <- asWeakBinder m1 e1
                  cod2 <- asWeakBinder m2 codType2
                  xt2 <- asWeakBinder m2 e2
                  cs' <- simplifyBinder orig (impArgs1 ++ expArgs1 ++ [cod1, xt1]) (impArgs2 ++ expArgs2 ++ [cod2, xt2])
                  simplify ax susList $ cs' ++ cs
            (_ :< WT.Data _ name1 es1, _ :< WT.Data _ name2 es2)
              | name1 == name2,
                length es1 == length es2 -> do
                  let cs' = map (,orig) (zipWith C.Eq es1 es2)
                  simplify ax susList $ cs' ++ cs
            (_ :< WT.DataIntro _ consName1 dataArgs1 consArgs1, _ :< WT.DataIntro _ consName2 dataArgs2 consArgs2)
              | consName1 == consName2,
                length dataArgs1 == length dataArgs2,
                length consArgs1 == length consArgs2 -> do
                  let es1 = dataArgs1 ++ consArgs1
                  let es2 = dataArgs2 ++ consArgs2
                  let cs' = map (,orig) (zipWith C.Eq es1 es2)
                  simplify ax susList $ cs' ++ cs
            (_ :< WT.Box t1, _ :< WT.Box t2) ->
              simplify ax susList $ (C.Eq t1 t2, orig) : cs
            (m1 :< WT.BoxIntro xets1 e1, m2 :< WT.BoxIntro xets2 e2)
              | length xets1 == length xets2 -> do
                  let (xs1, es1, ts1) = unzip3 xets1
                  let (xs2, es2, ts2) = unzip3 xets2
                  let cs' = map (orig,) $ zipWith C.Eq es1 es2
                  let binder1 = zipWith (\x t -> (m1, x, t)) xs1 ts1
                  let binder2 = zipWith (\x t -> (m2, x, t)) xs2 ts2
                  cs'' <- simplifyBinder orig binder1 binder2
                  simplify ax susList $ (C.Eq e1 e2, orig) : cs' ++ cs'' ++ cs
            (_ :< WT.Noema t1, _ :< WT.Noema t2) ->
              simplify ax susList $ (C.Eq t1 t2, orig) : cs
            (_ :< WT.Embody t1 e1, _ :< WT.Embody t2 e2) ->
              simplify ax susList $ (C.Eq t1 t2, orig) : (C.Eq e1 e2, orig) : cs
            (_ :< WT.Annotation _ _ e1, e2) ->
              simplify ax susList $ (C.Eq e1 e2, orig) : cs
            (e1, _ :< WT.Annotation _ _ e2) ->
              simplify ax susList $ (C.Eq e1 e2, orig) : cs
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
                  simplify ax susList $ (C.Eq e1' e2', orig) : cs
                (Just (h1, (xs1, body1)), Nothing) -> do
                  let s1 = HS.singleton h1 xs1 body1
                  e1' <- fill s1 e1
                  simplify ax susList $ (C.Eq e1' e2, orig) : cs
                (Nothing, Just (h2, (xs2, body2))) -> do
                  let s2 = HS.singleton h2 xs2 body2
                  e2' <- fill s2 e2
                  simplify ax susList $ (C.Eq e1 e2', orig) : cs
                (Nothing, Nothing) -> do
                  let fmvs = S.union fmvs1 fmvs2
                  case (Stuck.asStuckedTerm e1, Stuck.asStuckedTerm e2) of
                    (Just (Stuck.Hole h1 ies1, _ :< Stuck.Base), _)
                      | Just xss1 <- mapM asIdent ies1,
                        Just argSet1 <- toLinearIdentSet xss1,
                        h1 `S.notMember` fmvs2,
                        fvs2 `S.isSubsetOf` argSet1 ->
                          resolveHole ax susList h1 xss1 e2 cs
                    (_, Just (Stuck.Hole h2 ies2, _ :< Stuck.Base))
                      | Just xss2 <- mapM asIdent ies2,
                        Just argSet2 <- toLinearIdentSet xss2,
                        h2 `S.notMember` fmvs1,
                        fvs1 `S.isSubsetOf` argSet2 ->
                          resolveHole ax susList h2 xss2 e1 cs
                    (Just (Stuck.VarLocal x1, ctx1), Just (Stuck.VarLocal x2, ctx2))
                      | x1 == x2,
                        Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                          simplify ax susList $ map (,orig) pairList ++ cs
                    (Just (Stuck.VarGlobal g1, ctx1), Just (Stuck.VarGlobal g2, ctx2))
                      | g1 == g2,
                        Nothing <- Map.lookup g1 (defMap ax),
                        Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                          simplify ax susList $ map (,orig) pairList ++ cs
                      | g1 == g2,
                        Just lam <- Map.lookup g1 (defMap ax) -> do
                          let ax' = increment ax
                          simplify ax' susList $ (C.Eq (Stuck.resume lam ctx1) (Stuck.resume lam ctx2), orig) : cs
                      | Just lam1 <- Map.lookup g1 (defMap ax),
                        Just lam2 <- Map.lookup g2 (defMap ax) -> do
                          let ax' = increment ax
                          simplify ax' susList $ (C.Eq (Stuck.resume lam1 ctx1) (Stuck.resume lam2 ctx2), orig) : cs
                    (Just (Stuck.VarGlobal g1, ctx1), _)
                      | Just lam <- Map.lookup g1 (defMap ax) -> do
                          let ax' = increment ax
                          simplify ax' susList $ (C.Eq (Stuck.resume lam ctx1) e2, orig) : cs
                    (_, Just (Stuck.VarGlobal g2, ctx2))
                      | Just lam <- Map.lookup g2 (defMap ax) -> do
                          let ax' = increment ax
                          simplify ax' susList $ (C.Eq e1 (Stuck.resume lam ctx2), orig) : cs
                    (Just (Stuck.Prim (WP.Value (WPV.Op op1)), ctx1), Just (Stuck.Prim (WP.Value (WPV.Op op2)), ctx2))
                      | op1 == op2,
                        Just pairList <- Stuck.asPairList ctx1 ctx2 ->
                          simplify ax susList $ map (,orig) pairList ++ cs
                    _ -> do
                      let uc = C.SuspendedConstraint (fmvs, headConstraint)
                      simplify ax (uc : susList) cs

{-# INLINE resolveHole #-}
resolveHole ::
  Axis ->
  [SuspendedConstraint] ->
  HID.HoleID ->
  [Ident] ->
  WT.WeakTerm ->
  [(C.Constraint, C.Constraint)] ->
  App [SuspendedConstraint]
resolveHole ax susList h1 xs e2' cs = do
  insertSubst h1 xs e2'
  let (susList1, susList2) = partition (\(C.SuspendedConstraint (hs, _)) -> S.member h1 hs) susList
  let susList1' = map (\(C.SuspendedConstraint (_, c)) -> c) susList1
  simplify ax susList2 $ reverse susList1' ++ cs

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
  App [SuspendedConstraint]
simplifyActual m dataNameSet t orig = do
  t' <- reduce t
  case t' of
    _ :< WT.Tau -> do
      return []
    _ :< WT.Data (AttrD.Attr {consNameList}) dataName dataArgs -> do
      let dataNameSet' = S.insert dataName dataNameSet
      constraintsFromDataArgs <- fmap concat $ forM dataArgs $ \dataArg ->
        simplifyActual m dataNameSet' dataArg orig
      dataConsArgsList <-
        if S.member dataName dataNameSet
          then return []
          else mapM (getConsArgTypes m . fst) consNameList
      constraintsFromDataConsArgs <- fmap concat $ forM dataConsArgsList $ \dataConsArgs -> do
        dataConsArgs' <- substConsArgs IntMap.empty dataConsArgs
        fmap concat $ forM dataConsArgs' $ \(_, _, consArg) -> do
          simplifyActual m dataNameSet' consArg orig
      return $ constraintsFromDataArgs ++ constraintsFromDataConsArgs
    _ :< WT.Prim {} -> do
      return []
    _ :< WT.Resource {} -> do
      return []
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
            Just (Stuck.VarGlobal dd, evalCtx)
              | Just lam <- Map.lookup dd defMap -> do
                  simplifyActual m dataNameSet (Stuck.resume lam evalCtx) orig
            _ -> do
              return [C.SuspendedConstraint (fmvs, (C.Actual t', orig))]

substConsArgs :: Subst.SubstWeakTerm -> [BinderF WT.WeakTerm] -> App [BinderF WT.WeakTerm]
substConsArgs sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, x, t) : rest -> do
      t' <- Subst.subst sub t
      let opaque = m :< WT.Tau -- allow `a` in `Cons(a: type, x: a)`
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
    _ :< WT.Pi impArgs expArgs _ -> do
      return $ impArgs ++ expArgs
    _ ->
      Throw.raiseCritical m $ "The type of a constructor must be a Π-type, but it's not:\n" <> toText t

simplifyAffine ::
  Hint ->
  S.Set DD.DefiniteDescription ->
  WT.WeakTerm ->
  C.Constraint ->
  App [SuspendedConstraint]
simplifyAffine m dataNameSet t orig = do
  t' <- reduce t
  case t' of
    _ :< WT.Tau -> do
      return []
    _ :< WT.Data (AttrD.Attr {consNameList}) dataName dataArgs -> do
      od <- OptimizableData.lookup dataName
      case od of
        Just OD.Enum -> do
          return []
        Just OD.Unary -> do
          let dataNameSet' = S.insert dataName dataNameSet
          constraintsFromDataArgs <- fmap concat $ forM dataArgs $ \dataArg ->
            simplifyAffine m dataNameSet' dataArg orig
          dataConsArgsList <-
            if S.member dataName dataNameSet
              then return []
              else mapM (getConsArgTypes m . fst) consNameList
          constraintsFromDataConsArgs <- fmap concat $ forM dataConsArgsList $ \dataConsArgs -> do
            dataConsArgs' <- substConsArgs IntMap.empty dataConsArgs
            fmap concat $ forM dataConsArgs' $ \(_, _, consArg) -> do
              simplifyAffine m dataNameSet' consArg orig
          return $ constraintsFromDataArgs ++ constraintsFromDataConsArgs
        _ -> do
          return [C.SuspendedConstraint (holes t', (C.Affine t', orig))]
    _ :< WT.Noema {} ->
      return []
    _ :< WT.Prim {} -> do
      return []
    _ -> do
      sub <- getHoleSubst
      let fmvs = holes t'
      case lookupAny (S.toList fmvs) sub of
        Just (h, (xs, body)) -> do
          let s = HS.singleton h xs body
          t'' <- fill s t'
          simplifyAffine m dataNameSet t'' orig
        Nothing -> do
          defMap <- WeakDefinition.read
          case Stuck.asStuckedTerm t' of
            Just (Stuck.VarGlobal dd, evalCtx)
              | Just lam <- Map.lookup dd defMap -> do
                  simplifyAffine m dataNameSet (Stuck.resume lam evalCtx) orig
            _ -> do
              return [C.SuspendedConstraint (fmvs, (C.Affine t', orig))]
