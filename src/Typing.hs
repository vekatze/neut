module Typing
  ( check
  ) where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except

import qualified Text.Show.Pretty           as Pr

import           Data

check :: Expr -> WithEnv ()
check e = do
  t <- infer e
  env <- get
  sub <- unify $ constraintEnv env
  liftIO $ putStrLn $ Pr.ppShow sub
  let tenv' = map (\(s, t) -> (s, sType sub t)) $ typeEnv env
  modify (\e -> e {typeEnv = tenv'})

infer :: Expr -> WithEnv Type
infer (Var s) = do
  mt <- lookupTEnv s
  case mt of
    Just t -> return t
    Nothing -> do
      new <- THole <$> newName
      insTEnv s new
      return new
infer (Const (S s t)) = do
  insTEnv s t
  return t
infer (Lam (S s t) e) = do
  insTEnv s t
  te <- infer e
  return $ TForall (S s t) te
infer (App e v) = do
  te <- infer e
  tv <- infer v
  i <- newName
  insTEnv i (THole i)
  j <- newName
  insTEnv j tv
  insCEnv te (TForall (S j tv) (THole i))
  return $ THole i
infer (VApp v1 v2) = do
  t1 <- infer v1
  t2 <- infer v2
  i <- newName
  insTEnv i (THole i)
  j <- newName
  insTEnv j t2
  insCEnv t1 (TNode (S j t2) (THole i))
  return $ THole i
infer (Ret v) = do
  tv <- infer v
  return $ TUp tv
infer (Bind (S s t) e1 e2) = do
  insTEnv s t
  t1 <- infer e1
  t2 <- infer e2
  insCEnv (TUp t) t1
  return t2
infer (Thunk e) = do
  t <- infer e
  return $ TDown t
infer (Unthunk v) = do
  t <- infer v
  i <- newName
  insCEnv t (TDown (THole i))
  return $ THole i
infer (Send (S s t) e) = do
  insTEnv s t
  t <- infer e
  return t
infer (Recv (S s t) e) = do
  insTEnv s t
  t <- infer e
  return t
infer (Dispatch e1 e2) = do
  t1 <- infer e1
  t2 <- infer e2
  return $ TCotensor t1 t2
infer (Coleft e) = do
  t <- infer e
  t1 <- THole <$> newName
  t2 <- THole <$> newName
  insCEnv t (TCotensor t1 t2)
  return t1
infer (Coright e) = do
  t <- infer e
  t1 <- THole <$> newName
  t2 <- THole <$> newName
  insCEnv t (TCotensor t1 t2)
  return t2
infer (Mu (S s t) e) = do
  insTEnv s t
  te <- infer e
  insCEnv (TDown te) t
  return te
infer (Case e ves) = do
  t <- infer e
  let (vs, es) = unzip ves
  tvs <- mapM infer vs
  forM_ tvs $ \tv -> insCEnv t tv
  ans <- THole <$> newName
  tes <- mapM infer es
  forM_ tes $ \te -> insCEnv ans te
  return ans
infer (Asc e t) = do
  te <- infer e
  insCEnv t te
  return te

type Subst = [(String, Type)]

type Constraint = [(Type, Type)]

unify :: Constraint -> WithEnv Subst
unify [] = return []
unify ((THole s, t2):cs)
  | not (occur s t2) = do
    sub <- unify (sConstraint [(s, t2)] cs)
    return $ compose sub [(s, t2)]
unify ((t1, THole s):cs)
  | not (occur s t1) = do
    sub <- unify (sConstraint [(s, t1)] cs)
    return $ compose sub [(s, t1)]
unify ((TVar s1, TVar s2):cs)
  | s1 == s2 = unify cs
unify ((TConst s1, TConst s2):cs)
  | s1 == s2 = unify cs
unify ((TNode (S _ tdom1) tcod1, TNode (S _ tdom2) tcod2):cs) =
  unify $ (tdom1, tdom2) : (tcod1, tcod2) : cs
unify ((TForall (S _ tdom1) tcod1, TForall (S _ tdom2) tcod2):cs) =
  unify $ (tdom1, tdom2) : (tcod1, tcod2) : cs
unify ((TCotensor t11 t12, TCotensor t21 t22):cs) =
  unify $ (t11, t21) : (t12, t22) : cs
unify ((TUp t1, TUp t2):cs) = unify $ (t1, t2) : cs
unify ((TDown t1, TDown t2):cs) = unify $ (t1, t2) : cs
unify ((TUniv i, TUniv j):cs) = do
  insLEnv i j
  unify cs
unify ((t1, t2):cs) =
  lift $
  throwE $
  "unification failed for:\n" ++ Pr.ppShow t1 ++ "\nand:\n" ++ Pr.ppShow t2

occur :: String -> Type -> Bool
occur _ (TVar s)                  = False
occur x (THole s)                 = x == s
occur _ (TConst (S _ _))          = False
occur x (TNode (S _ tdom) tcod)   = occur x tdom || occur x tcod
occur x (TUp t)                   = occur x t
occur x (TDown t)                 = occur x t
occur _ (TUniv i)                 = False
occur x (TForall (S _ tdom) tcod) = occur x tdom || occur x tcod
occur x (TCotensor t1 t2)         = occur x t1 || occur x t2

compose :: Subst -> Subst -> Subst
compose s1 s2 = do
  let domS2 = map fst s2
  let codS2 = map snd s2
  let codS2' = map (sType s1) codS2
  let fromS1 = filter (\(ident, _) -> ident `notElem` domS2) s1
  fromS1 ++ zip domS2 codS2'

sType :: Subst -> Type -> Type
sType _ (TVar s) = TVar s
sType sub (THole s) =
  case lookup s sub of
    Nothing -> THole s
    Just t  -> t
sType sub (TConst (S s t)) = do
  let t' = sType sub t
  TConst (S s t')
sType sub (TNode (S s tdom) tcod) = do
  let tdom' = sType sub tdom
  let tcod' = sType sub tcod
  TNode (S s tdom') tcod'
sType sub (TUp t) = do
  let t' = sType sub t
  TUp t'
sType sub (TDown t) = do
  let t' = sType sub t
  TDown t'
sType _ (TUniv i) = TUniv i
sType sub (TForall (S s tdom) tcod) = do
  let tdom' = sType sub tdom
  let tcod' = sType sub tcod
  TForall (S s tdom') tcod'
sType sub (TCotensor t1 t2) = do
  let t1' = sType sub t1
  let t2' = sType sub t2
  TCotensor t1' t2'

sConstraint :: Subst -> Constraint -> Constraint
sConstraint s = map (\(t1, t2) -> (sType s t1, sType s t2))
