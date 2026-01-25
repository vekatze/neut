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
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Elaborate.Constraint (SuspendedConstraint)
import Kernel.Elaborate.Constraint qualified as C
import Kernel.Elaborate.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Internal.Handle.Elaborate
import Kernel.Elaborate.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Kernel.Elaborate.Stuck qualified as Stuck
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.PiKind qualified as PK
import Language.Common.PrimType qualified as PT
import Language.Common.VarKind qualified as VK
import Language.WeakTerm.Eq qualified as WT
import Language.WeakTerm.FreeVars
import Language.WeakTerm.Holes
import Language.WeakTerm.Subst (Subst, SubstEntry (..))
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.ToText (toTextType)
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint
import Logger.Log qualified as L
import Logger.LogLevel qualified as L

unify :: Handle -> [C.Constraint] -> App THS.TypeHoleSubst
unify h constraintList = do
  susList <- unify' h (reverse constraintList)
  case susList of
    [] ->
      liftIO $ Hole.getTypeSubst (holeHandle h)
    _ ->
      throwTypeErrors h susList

unifyCurrentConstraints :: Handle -> App THS.TypeHoleSubst
unifyCurrentConstraints h = do
  susList <- liftIO $ Constraint.getSuspendedConstraints (constraintHandle h)
  cs <- liftIO $ Constraint.get (constraintHandle h)
  susList' <- simplify h susList $ zip cs cs
  -- liftIO $ putStrLn "remaining:"
  -- liftIO $ putStrLn $ T.unpack $ C.showSuspendedConstraints susList'
  liftIO $ Constraint.set (constraintHandle h) []
  liftIO $ Constraint.setSuspendedConstraints (constraintHandle h) susList'
  liftIO $ Hole.getTypeSubst (holeHandle h)

unify' :: Handle -> [C.Constraint] -> App [SuspendedConstraint]
unify' h constraintList = do
  susList <- liftIO $ Constraint.getSuspendedConstraints (constraintHandle h)
  simplify h susList $ zip constraintList constraintList

throwTypeErrors :: Handle -> [SuspendedConstraint] -> App a
throwTypeErrors h susList = do
  sub <- liftIO $ Hole.getTypeSubst (holeHandle h)
  errorList <- mapM (\(C.SuspendedConstraint (_, (_, c))) -> constraintToRemark h sub c) susList
  throwError $ E.MakeError errorList

constraintToRemark :: Handle -> THS.TypeHoleSubst -> C.Constraint -> App L.Log
constraintToRemark h sub c = do
  case c of
    C.Actual t -> do
      t' <- fillAsMuchAsPossible h sub t
      return $ L.newLog (WT.metaOfType t) L.Error $ constructErrorMessageActual t'
    C.Integer t -> do
      t' <- fillAsMuchAsPossible h sub t
      return $ L.newLog (WT.metaOfType t) L.Error $ constructErrorMessageInteger t'
    C.Eq expected actual -> do
      expected' <- fillAsMuchAsPossible h sub expected
      actual' <- fillAsMuchAsPossible h sub actual
      return $ L.newLog (WT.metaOfType actual) L.Error $ constructErrorMessageEq actual' expected'

fillAsMuchAsPossible :: Handle -> THS.TypeHoleSubst -> WT.WeakType -> App WT.WeakType
fillAsMuchAsPossible h sub e = do
  e' <- reduceType h e
  if THS.fillableType e' sub
    then fillType h sub e' >>= fillAsMuchAsPossible h sub
    else return e'

constructErrorMessageEq :: WT.WeakType -> WT.WeakType -> T.Text
constructErrorMessageEq found expected =
  "Expected:\n  "
    <> toTextType expected
    <> "\nFound:\n  "
    <> toTextType found

constructErrorMessageActual :: WT.WeakType -> T.Text
constructErrorMessageActual t =
  "A term of the following type might be noetic:\n  "
    <> toTextType t

constructErrorMessageInteger :: WT.WeakType -> T.Text
constructErrorMessageInteger t =
  "Expected:\n  "
    <> "an integer type"
    <> "\nFound:\n  "
    <> toTextType t

detectPossibleInfiniteLoop :: Handle -> C.Constraint -> App ()
detectPossibleInfiniteLoop h c = do
  let Handle {inlineLimit, currentStep} = h
  when (inlineLimit < currentStep) $ do
    sub <- liftIO $ Hole.getTypeSubst (holeHandle h)
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
      susList' <- simplifyActual h (WT.metaOfType t) S.empty t orig
      simplify h (susList' ++ susList) cs
    (C.Integer t, orig) : cs -> do
      susList' <- simplifyInteger h (WT.metaOfType t) t orig
      simplify h (susList' ++ susList) cs
    headConstraint@(C.Eq expected actual, orig) : cs -> do
      detectPossibleInfiniteLoop h orig
      expected' <- reduceType h expected
      actual' <- reduceType h actual
      if WT.eqType expected' actual'
        then simplify h susList cs
        else do
          case (expected', actual') of
            (m1 :< WT.Pi _ impArgs1 expArgs1 defaultArgs1 cod1, m2 :< WT.Pi _ impArgs2 expArgs2 defaultArgs2 cod2)
              | Just (impBinders, impConstraints) <- createDefaultConstraints impArgs1 defaultArgs1 impArgs2 defaultArgs2,
                length expArgs1 == length expArgs2 -> do
                  codBinder1 <- liftIO $ asWeakBinder h m1 cod1
                  codBinder2 <- liftIO $ asWeakBinder h m2 cod2
                  let (impBinders1, impBinders2) = unzip impBinders
                  let impEqs' = map (,orig) impConstraints
                  cs' <- liftIO $ simplifyBinder h orig (impBinders1 ++ expArgs1 ++ [codBinder1]) (impBinders2 ++ expArgs2 ++ [codBinder2])
                  simplify h susList $ cs' ++ impEqs' ++ cs
            (_ :< WT.Data _ name1 es1, _ :< WT.Data _ name2 es2)
              | name1 == name2,
                length es1 == length es2 -> do
                  let cs' = map (,orig) (zipWith C.Eq es1 es2)
                  simplify h susList $ cs' ++ cs
            (_ :< WT.Box t1, _ :< WT.Box t2) ->
              simplify h susList $ (C.Eq t1 t2, orig) : cs
            (_ :< WT.BoxNoema t1, _ :< WT.BoxNoema t2) ->
              simplify h susList $ (C.Eq t1 t2, orig) : cs
            (_ :< WT.Code t1, _ :< WT.Code t2) ->
              simplify h susList $ (C.Eq t1 t2, orig) : cs
            (t1, t2) -> do
              sub <- liftIO $ Hole.getTypeSubst (holeHandle h)
              let fvs1 = freeVarsType t1
              let fvs2 = freeVarsType t2
              let fmvs1 = holesType t1
              let fmvs2 = holesType t2
              case (lookupAnyType (S.toList fmvs1) sub, lookupAnyType (S.toList fmvs2) sub) of
                (Just (hole1, (xs1, body1)), Just (hole2, (xs2, body2))) -> do
                  let s1 = THS.singleton hole1 xs1 body1
                  let s2 = THS.singleton hole2 xs2 body2
                  t1' <- fillType h s1 t1
                  t2' <- fillType h s2 t2
                  simplify h susList $ (C.Eq t1' t2', orig) : cs
                (Just (hole1, (xs1, body1)), Nothing) -> do
                  let s1 = THS.singleton hole1 xs1 body1
                  t1' <- fillType h s1 t1
                  simplify h susList $ (C.Eq t1' t2, orig) : cs
                (Nothing, Just (hole2, (xs2, body2))) -> do
                  let s2 = THS.singleton hole2 xs2 body2
                  t2' <- fillType h s2 t2
                  simplify h susList $ (C.Eq t1 t2', orig) : cs
                (Nothing, Nothing) -> do
                  let fmvs = S.union fmvs1 fmvs2
                  defMap <- liftIO $ WeakTypeDef.read' (weakTypeDefHandle h)
                  case (Stuck.asStuckedType t1, Stuck.asStuckedType t2) of
                    (Just (Stuck.Hole hole1 ies1, _ :< Stuck.Base), _)
                      | Just xss1 <- mapM asIdentType ies1,
                        Just argSet1 <- toLinearIdentSet xss1,
                        hole1 `S.notMember` fmvs2,
                        fvs2 `S.isSubsetOf` argSet1 ->
                          resolveHole h susList hole1 xss1 t2 cs
                    (_, Just (Stuck.Hole hole2 ies2, _ :< Stuck.Base))
                      | Just xss2 <- mapM asIdentType ies2,
                        Just argSet2 <- toLinearIdentSet xss2,
                        hole2 `S.notMember` fmvs1,
                        fvs1 `S.isSubsetOf` argSet2 ->
                          resolveHole h susList hole2 xss2 t1 cs
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
                          mt1' <- liftIO $ Stuck.resume (substHandle h) lam ctx1
                          mt2' <- liftIO $ Stuck.resume (substHandle h) lam ctx2
                          case (mt1', mt2') of
                            (Just t1', Just t2') -> do
                              simplify h' susList $ (C.Eq t1' t2', orig) : cs
                            _ ->
                              simplify h (C.SuspendedConstraint (fmvs, headConstraint) : susList) cs
                      | Just lam1 <- Map.lookup g1 defMap,
                        Just lam2 <- Map.lookup g2 defMap -> do
                          let h' = increment h
                          mt1' <- liftIO $ Stuck.resume (substHandle h) lam1 ctx1
                          mt2' <- liftIO $ Stuck.resume (substHandle h) lam2 ctx2
                          case (mt1', mt2') of
                            (Just t1', Just t2') -> do
                              simplify h' susList $ (C.Eq t1' t2', orig) : cs
                            _ ->
                              simplify h (C.SuspendedConstraint (fmvs, headConstraint) : susList) cs
                    (Just (Stuck.VarGlobal g1, ctx1), _)
                      | Just lam <- Map.lookup g1 defMap -> do
                          let h' = increment h
                          mt1' <- liftIO $ Stuck.resume (substHandle h) lam ctx1
                          case mt1' of
                            Just t1' -> do
                              simplify h' susList $ (C.Eq t1' t2, orig) : cs
                            Nothing -> do
                              simplify h (C.SuspendedConstraint (fmvs, headConstraint) : susList) cs
                    (_, Just (Stuck.VarGlobal g2, ctx2))
                      | Just lam <- Map.lookup g2 defMap -> do
                          let h' = increment h
                          mt2' <- liftIO $ Stuck.resume (substHandle h) lam ctx2
                          case mt2' of
                            Just t2' -> do
                              simplify h' susList $ (C.Eq t1 t2', orig) : cs
                            Nothing -> do
                              simplify h (C.SuspendedConstraint (fmvs, headConstraint) : susList) cs
                    _ -> do
                      simplify h (C.SuspendedConstraint (fmvs, headConstraint) : susList) cs

{-# INLINE resolveHole #-}
resolveHole ::
  Handle ->
  [SuspendedConstraint] ->
  HID.HoleID ->
  [Ident] ->
  WT.WeakType ->
  [(C.Constraint, C.Constraint)] ->
  App [SuspendedConstraint]
resolveHole h susList hole1 xs e2' cs = do
  liftIO $ Hole.insertTypeSubst (holeHandle h) hole1 xs e2'
  let (susList1, susList2) = partition (\(C.SuspendedConstraint (hs, _)) -> S.member hole1 hs) susList
  let susList1' = map (\(C.SuspendedConstraint (_, c)) -> c) susList1
  simplify h susList2 $ reverse susList1' ++ cs

simplifyBinder ::
  Handle ->
  C.Constraint ->
  [BinderF WT.WeakType] ->
  [BinderF WT.WeakType] ->
  IO [(C.Constraint, C.Constraint)]
simplifyBinder h orig =
  simplifyBinder' h orig IntMap.empty

simplifyBinder' ::
  Handle ->
  C.Constraint ->
  Subst ->
  [BinderF WT.WeakType] ->
  [BinderF WT.WeakType] ->
  IO [(C.Constraint, C.Constraint)]
simplifyBinder' h orig sub args1 args2 =
  case (args1, args2) of
    ((m1, _, x1, t1) : xts1, (_, _, x2, t2) : xts2) -> do
      t2' <- Subst.substType (substHandle h) sub t2
      let sub' = IntMap.insert (Ident.toInt x2) (Type (m1 :< WT.TVar x1)) sub
      rest <- simplifyBinder' h orig sub' xts1 xts2
      return $ (C.Eq t1 t2', orig) : rest
    _ ->
      return []

asWeakBinder :: Handle -> Hint -> WT.WeakType -> IO (BinderF WT.WeakType)
asWeakBinder h m t = do
  x <- Gensym.newIdentFromText (gensymHandle h) "hole"
  return (m, VK.Normal, x, t)

asIdentType :: WT.WeakType -> Maybe Ident
asIdentType e =
  case e of
    _ :< WT.TVar x ->
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

simplifyActual ::
  Handle ->
  Hint ->
  S.Set DD.DefiniteDescription ->
  WT.WeakType ->
  C.Constraint ->
  App [SuspendedConstraint]
simplifyActual h m dataNameSet t orig = do
  detectPossibleInfiniteLoop h orig
  t' <- reduceType h t
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
          else mapM (getConsArgTypes h m . (\(name, _, _) -> name)) consNameList
      constraintsFromDataConsArgs <- fmap concat $ forM dataConsArgsList $ \dataConsArgs -> do
        dataConsArgs' <- liftIO $ substConsArgs h IntMap.empty dataConsArgs
        fmap concat $ forM dataConsArgs' $ \(_, _, _, consArg) -> do
          simplifyActual h m dataNameSet' consArg orig
      return $ constraintsFromDataArgs ++ constraintsFromDataConsArgs
    _ :< WT.Box t'' -> do
      simplifyActual h m dataNameSet t'' orig
    _ :< WT.PrimType {} -> do
      return []
    _ :< WT.Void -> do
      return []
    _ :< WT.Resource {} -> do
      return []
    _ -> do
      sub <- liftIO $ Hole.getTypeSubst (holeHandle h)
      let fmvs = holesType t'
      case lookupAnyType (S.toList fmvs) sub of
        Just (hole, (xs, body)) -> do
          let s = THS.singleton hole xs body
          t'' <- fillType h s t'
          simplifyActual h m dataNameSet t'' orig
        Nothing -> do
          defMap <- liftIO $ WeakTypeDef.read' (weakTypeDefHandle h)
          case Stuck.asStuckedType t' of
            Just (Stuck.VarGlobal dd, evalCtx)
              | Just lam <- Map.lookup dd defMap -> do
                  let h' = increment h
                  mt'' <- liftIO $ Stuck.resume (substHandle h) lam evalCtx
                  case mt'' of
                    Just t'' -> do
                      simplifyActual h' m dataNameSet t'' orig
                    Nothing -> do
                      return [C.SuspendedConstraint (fmvs, (C.Actual t', orig))]
            _ -> do
              return [C.SuspendedConstraint (fmvs, (C.Actual t', orig))]

getConsArgTypes ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  App [BinderF WT.WeakType]
getConsArgTypes h m consName = do
  t <- Type.lookup' (typeHandle h) m consName
  case t of
    _ :< WT.Pi (PK.DataIntro False) impArgs expArgs defaultArgs (_ :< WT.Pi (PK.Normal _) impArgs' expArgs' defaultArgs' _dataType) -> do
      return $ impArgs ++ expArgs ++ defaultArgs ++ impArgs' ++ expArgs' ++ defaultArgs'
    _ :< WT.Pi (PK.DataIntro True) impArgs expArgs defaultArgs _dataType -> do
      return $ impArgs ++ expArgs ++ defaultArgs
    _ ->
      raiseCritical m $ "Got a malformed constructor type:\n" <> toTextType t

simplifyInteger ::
  Handle ->
  Hint ->
  WT.WeakType ->
  C.Constraint ->
  App [SuspendedConstraint]
simplifyInteger h m t orig = do
  detectPossibleInfiniteLoop h orig
  t' <- reduceType h t
  case t' of
    _ :< WT.PrimType (PT.Int _) -> do
      return []
    _ -> do
      sub <- liftIO $ Hole.getTypeSubst (holeHandle h)
      let fmvs = holesType t'
      case lookupAnyType (S.toList fmvs) sub of
        Just (hole, (xs, body)) -> do
          t'' <- fillType h (THS.singleton hole xs body) t'
          simplifyInteger h m t'' orig
        Nothing -> do
          defMap <- liftIO $ WeakTypeDef.read' (weakTypeDefHandle h)
          case Stuck.asStuckedType t' of
            Just (Stuck.VarGlobal dd, evalCtx)
              | Just lam <- Map.lookup dd defMap -> do
                  let h' = increment h
                  mt'' <- liftIO $ Stuck.resume (substHandle h) lam evalCtx
                  case mt'' of
                    Just t'' -> do
                      simplifyInteger h' m t'' orig
                    Nothing -> do
                      return [C.SuspendedConstraint (fmvs, (C.Integer t', orig))]
            _ -> do
              return [C.SuspendedConstraint (fmvs, (C.Integer t', orig))]

lookupAnyType :: [HID.HoleID] -> THS.TypeHoleSubst -> Maybe (HID.HoleID, ([Ident], WT.WeakType))
lookupAnyType is sub =
  case is of
    [] ->
      Nothing
    j : js ->
      case THS.lookup j sub of
        Just v ->
          Just (j, v)
        _ ->
          lookupAnyType js sub

substConsArgs :: Handle -> Subst -> [BinderF WT.WeakType] -> IO [BinderF WT.WeakType]
substConsArgs h sub consArgs =
  case consArgs of
    [] ->
      return []
    (m, k, x, t) : rest -> do
      t' <- Subst.substType (substHandle h) sub t
      let opaque = m :< WT.Tau -- allow `a` in `Cons(a: type, x: a)`
      let sub' = IntMap.insert (Ident.toInt x) (Type opaque) sub
      rest' <- substConsArgs h sub' rest
      return $ (m, k, x, t') : rest'

createDefaultConstraints ::
  [BinderF WT.WeakType] ->
  [BinderF WT.WeakType] ->
  [BinderF WT.WeakType] ->
  [BinderF WT.WeakType] ->
  Maybe ([(BinderF WT.WeakType, BinderF WT.WeakType)], [C.Constraint])
createDefaultConstraints impArgs1 defaultArgs1 impArgs2 defaultArgs2 = do
  let params1 = impArgs1 ++ defaultArgs1
  let params2 = impArgs2 ++ defaultArgs2
  if length params1 == length params2
    then Just (zip params1 params2, [])
    else Nothing

increment :: Handle -> Handle
increment h = do
  h {currentStep = currentStep h + 1}
