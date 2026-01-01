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
import Data.IntMap qualified as IntMap
import Data.List (partition)
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Elaborate.Constraint (SuspendedConstraint)
import Kernel.Elaborate.Constraint qualified as C
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Kernel.Elaborate.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Internal.Handle.Elaborate
import Kernel.Elaborate.Internal.Handle.Hole qualified as Hole
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.HoleID qualified as HID
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.PiKind qualified as PK
import Language.Common.PrimType qualified as PT
import Language.WeakTerm.Eq qualified as WT
import Language.WeakTerm.FreeVars
import Language.WeakTerm.Holes
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
      sub <- liftIO $ Hole.getTypeSubst (holeHandle h)
      expected'' <- if THS.fillableType expected' sub then fillType h sub expected' else return expected'
      actual'' <- if THS.fillableType actual' sub then fillType h sub actual' else return actual'
      if WT.eqType expected'' actual''
        then simplify h susList cs
        else do
          case (expected'', actual'') of
            (holeTerm@(_ :< WT.TypeHole hole args), t) ->
              resolveTypeHole h susList holeTerm hole args t cs
            (t, holeTerm@(_ :< WT.TypeHole hole args)) ->
              resolveTypeHole h susList holeTerm hole args t cs
            (m1 :< WT.Pi pk1 impArgs1 defaultArgs1 expArgs1 cod1, m2 :< WT.Pi pk2 impArgs2 defaultArgs2 expArgs2 cod2)
              | pk1 == pk2,
                length impArgs1 == length impArgs2,
                length defaultArgs1 == length defaultArgs2,
                length expArgs1 == length expArgs2 -> do
                  codBinder1 <- liftIO $ asWeakBinder h m1 cod1
                  codBinder2 <- liftIO $ asWeakBinder h m2 cod2
                  cs' <- liftIO $ simplifyBinder h orig (impArgs1 ++ map fst defaultArgs1 ++ expArgs1 ++ [codBinder1]) (impArgs2 ++ map fst defaultArgs2 ++ expArgs2 ++ [codBinder2])
                  simplify h susList $ cs' ++ cs
            (_ :< WT.TyApp t1 args1, _ :< WT.TyApp t2 args2)
              | length args1 == length args2 -> do
                  let cs' = map (,orig) (C.Eq t1 t2 : zipWith C.Eq args1 args2)
                  simplify h susList $ cs' ++ cs
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
            (_ :< WT.PrimType pt1, _ :< WT.PrimType pt2)
              | pt1 == pt2 ->
                  simplify h susList cs
            (_ :< WT.Tau, _ :< WT.Tau) ->
              simplify h susList cs
            (_ :< WT.TVar x1, _ :< WT.TVar x2)
              | x1 == x2 ->
                  simplify h susList cs
            (_ :< WT.TVarGlobal _ x1, _ :< WT.TVarGlobal _ x2)
              | x1 == x2 ->
                  simplify h susList cs
            (_ :< WT.Void, _ :< WT.Void) ->
              simplify h susList cs
            (_ :< WT.Resource _ id1 _ _ _ _, _ :< WT.Resource _ id2 _ _ _ _)
              | id1 == id2 ->
                  simplify h susList cs
            _ -> do
              let fmvs = S.union (holesType expected'') (holesType actual'')
              let uc = C.SuspendedConstraint (fmvs, headConstraint)
              simplify h (uc : susList) cs

resolveTypeHole ::
  Handle ->
  [SuspendedConstraint] ->
  WT.WeakType ->
  HID.HoleID ->
  [WT.WeakType] ->
  WT.WeakType ->
  [(C.Constraint, C.Constraint)] ->
  App [SuspendedConstraint]
resolveTypeHole h susList holeTerm hole args t cs = do
  case (mapM asIdentType args, hole `S.notMember` holesType t) of
    (Just xs, True)
      | Just argSet <- toLinearIdentSet xs,
        freeVarsType t `S.isSubsetOf` argSet -> do
          liftIO $ Hole.insertTypeSubst (holeHandle h) hole xs t
          let (susList1, susList2) = partition (\(C.SuspendedConstraint (hs, _)) -> S.member hole hs) susList
          let susList1' = map (\(C.SuspendedConstraint (_, c)) -> c) susList1
          simplify h susList2 $ reverse susList1' ++ cs
    _ -> do
      let fmvs = S.union (holesType holeTerm) (holesType t)
      let uc = C.SuspendedConstraint (fmvs, (C.Eq holeTerm t, C.Eq holeTerm t))
      simplify h (uc : susList) cs

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
  WT.SubstWeakType ->
  [BinderF WT.WeakType] ->
  [BinderF WT.WeakType] ->
  IO [(C.Constraint, C.Constraint)]
simplifyBinder' h orig sub args1 args2 =
  case (args1, args2) of
    ((m1, x1, t1) : xts1, (_, x2, t2) : xts2) -> do
      let t2' = substType sub t2
      let sub' = IntMap.insert (Ident.toInt x2) (Right (m1 :< WT.TVar x1)) sub
      rest <- simplifyBinder' h orig sub' xts1 xts2
      return $ (C.Eq t1 t2', orig) : rest
    _ ->
      return []

asWeakBinder :: Handle -> Hint -> WT.WeakType -> IO (BinderF WT.WeakType)
asWeakBinder h m t = do
  x <- Gensym.newIdentFromText (gensymHandle h) "hole"
  return (m, x, t)

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
        return $ map (\(_, _, consArg) -> C.SuspendedConstraint (holesType consArg, (C.Actual consArg, orig))) dataConsArgs
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
        Nothing ->
          return [C.SuspendedConstraint (fmvs, (C.Actual t', orig))]

getConsArgTypes ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  App [BinderF WT.WeakType]
getConsArgTypes h m consName = do
  t <- Type.lookup' (typeHandle h) m consName
  case t of
    _ :< WT.Pi (PK.DataIntro False) impArgs defaultArgs expArgs (_ :< WT.Pi (PK.Normal _) impArgs' defaultArgs' expArgs' _dataType) -> do
      return $ impArgs ++ map fst defaultArgs ++ expArgs ++ impArgs' ++ map fst defaultArgs' ++ expArgs'
    _ :< WT.Pi (PK.DataIntro True) impArgs defaultArgs expArgs _dataType -> do
      return $ impArgs ++ map fst defaultArgs ++ expArgs
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
        Nothing ->
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

substType :: WT.SubstWeakType -> WT.WeakType -> WT.WeakType
substType sub ty =
  case ty of
    m :< WT.TVar x
      | Just varOrType <- IntMap.lookup (Ident.toInt x) sub ->
          case varOrType of
            Left x' ->
              m :< WT.TVar x'
            Right t ->
              t
      | otherwise ->
          ty
    _ :< WT.Tau ->
      ty
    _ :< WT.TVarGlobal {} ->
      ty
    m :< WT.TyApp t args ->
      m :< WT.TyApp (substType sub t) (map (substType sub) args)
    m :< WT.Pi piKind impArgs defaultArgs expArgs t ->
      let impArgs' = map (substTypeBinder sub) impArgs
          expArgs' = map (substTypeBinder sub) expArgs
          bound = map (\(_, x, _) -> Ident.toInt x) (impArgs ++ map fst defaultArgs ++ expArgs)
          sub' = foldr IntMap.delete sub bound
       in m :< WT.Pi piKind impArgs' defaultArgs expArgs' (substType sub' t)
    m :< WT.Data attr name es ->
      m :< WT.Data attr name (map (substType sub) es)
    m :< WT.Box t ->
      m :< WT.Box (substType sub t)
    m :< WT.BoxNoema t ->
      m :< WT.BoxNoema (substType sub t)
    m :< WT.Code t ->
      m :< WT.Code (substType sub t)
    _ :< WT.PrimType {} ->
      ty
    _ :< WT.Void ->
      ty
    m :< WT.Resource dd resourceID unitType discarder copier typeTag ->
      m :< WT.Resource dd resourceID (substType sub unitType) discarder copier typeTag
    m :< WT.TypeHole hole es ->
      m :< WT.TypeHole hole (map (substType sub) es)

substTypeBinder :: WT.SubstWeakType -> BinderF WT.WeakType -> BinderF WT.WeakType
substTypeBinder sub (m, x, t) =
  (m, x, substType sub t)
