module Polarize where

import           Control.Monad

import           Control.Comonad.Cofree

import           Data

polarize :: WeakTerm -> Either String Term
polarize (i :< WeakTermVar s) = return $ TermValue $ Value $ i :< ValueVar s
polarize (i :< WeakTermConst s) = return $ TermValue $ Value $ i :< ValueConst s
polarize (i :< WeakTermNodeApp s vs) = do
  let sanitizer v =
        case v of
          TermValue (Value v) -> return v
          _ -> Left $ "the polarity of " ++ show v ++ " is wrong"
  vs' <- mapM polarize vs
  vs'' <- mapM sanitizer vs'
  return $ TermValue $ Value $ i :< ValueNodeApp s vs''
polarize (i :< WeakTermLam (s, _) e) = do
  mc <- polarize e
  case mc of
    TermComp (Comp c) -> return $ TermComp $ Comp $ i :< CompLam s c
    _                 -> Left $ "the polarity of " ++ show e ++ " is wrong"
polarize (i :< WeakTermApp e1 e2) = do
  mc <- polarize e1
  mv <- polarize e2
  case (mc, mv) of
    (TermComp (Comp c), TermValue v) ->
      return $ TermComp $ Comp $ i :< CompApp c v
    _ ->
      Left $ "the polarity of " ++ show e1 ++ " or " ++ show e2 ++ " is wrong"
polarize (i :< WeakTermRet e) = do
  mv <- polarize e
  case mv of
    TermValue v -> return $ TermComp $ Comp $ i :< CompRet v
    _           -> Left $ "the polarity of " ++ show e ++ " is wrong"
polarize (i :< WeakTermBind (s, _) e1 e2) = do
  mc1 <- polarize e1
  mc2 <- polarize e2
  case (mc1, mc2) of
    (TermComp (Comp c1), TermComp (Comp c2)) ->
      return $ TermComp $ Comp $ i :< CompBind s c1 c2
    _ ->
      Left $
      "foo the polarity of " ++ show e1 ++ " or " ++ show e2 ++ " is wrong"
polarize (i :< WeakTermThunk e) = do
  mc <- polarize e
  case mc of
    TermComp c -> return $ TermValue $ Value $ i :< ValueThunk c
    _          -> Left $ "the polarity of " ++ show e ++ " is wrong"
polarize (i :< WeakTermUnthunk e) = do
  mv <- polarize e
  case mv of
    TermValue v -> return $ TermComp $ Comp $ i :< CompUnthunk v
    _           -> Left $ "the polarity of " ++ show e ++ " is wrong"
polarize (i :< WeakTermMu (s, _) e) = do
  mc <- polarize e
  case mc of
    TermComp (Comp c) -> return $ TermComp $ Comp $ i :< CompMu s c
    _                 -> Left $ "the polarity of " ++ show e ++ " is wrong"
polarize (i :< WeakTermCase e ves) = do
  ves' <-
    forM ves $ \(v, e) -> do
      e' <- polarize e
      case e' of
        TermComp (Comp c) -> return (v, c)
        _ ->
          Left $ "the polarity of " ++ show v ++ " or " ++ show e ++ " is wrong"
  e' <- polarize e
  case e' of
    TermValue v -> return $ TermComp $ Comp $ i :< CompCase v ves'
polarize (i :< WeakTermAsc e t) = polarize e

polarizeType :: WeakType -> Either String Type
polarizeType (WeakTypeVar i) = return $ TypeValueType (ValueTypeVar i)
polarizeType (WeakTypeConst c) = return $ TypeValueType (ValueTypeConst c)
polarizeType (WeakTypeNode xts t2) = do
  let (xs, ts) = unzip xts
  let sanitizer v =
        case v of
          TypeValueType v -> return v
          _ -> Left $ "the polarity of " ++ show v ++ " is wrong"
  ts' <- mapM polarizeType ts
  ts'' <- mapM sanitizer ts'
  mt2' <- polarizeType t2
  case mt2' of
    TypeValueType t2' ->
      return $ TypeValueType (ValueTypeNode (zip xs ts'') t2')
    _ -> Left $ "the polarity of " ++ show t2 ++ " is wrong"
polarizeType (WeakTypeUp t) = do
  mt' <- polarizeType t
  case mt' of
    TypeValueType t' -> return $ TypeCompType (CompTypeUp t')
    _                -> Left $ "the polarity of " ++ show t ++ " is wrong"
polarizeType (WeakTypeDown t) = do
  mt' <- polarizeType t
  case mt' of
    TypeCompType t' -> return $ TypeValueType (ValueTypeDown t')
    _               -> Left $ "the polarity of " ++ show t ++ " is wrong"
polarizeType (WeakTypeUniv (WeakLevelFixed i)) =
  return $ TypeValueType (ValueTypeUniv i)
polarizeType (WeakTypeUniv (WeakLevelHole _)) =
  return $ TypeValueType (ValueTypeUniv 0) -- for now
polarizeType (WeakTypeForall (Ident s, t1) t2) = do
  mt1' <- polarizeType t1
  mt2' <- polarizeType t2
  case (mt1', mt2') of
    (TypeValueType t1', TypeCompType t2') ->
      return $ TypeCompType (CompTypeForall (s, t1') t2')
    _ ->
      Left $ "the polarity of " ++ show t1 ++ " or " ++ show t2 ++ " is wrong"
polarizeType t@(WeakTypeForall (Hole i, t1) t2) = do
  mt1' <- polarizeType t1
  mt2' <- polarizeType t2
  case (mt1', mt2') of
    (TypeValueType t1', TypeCompType t2') ->
      return $ TypeCompType (CompTypeForall (i, t1') t2')
    _ ->
      Left $ "the polarity of " ++ show t1 ++ " or " ++ show t2 ++ " is wrong"
polarizeType t = Left $ "the polarity of " ++ show t ++ " is wrong"
