module Exhaust where

import           Control.Monad.State
import           Control.Monad.Trans.Except

import           Control.Comonad.Cofree

import           Data
import           Data.List

import qualified Text.Show.Pretty           as Pr

exhaust :: Neut -> WithEnv Neut
exhaust e = do
  liftIO $ putStrLn $ "checking " ++ Pr.ppShow e
  b <- exhaust' e
  if b
    then return e
    else lift $ throwE "non-exhaustive pattern"

exhaust' :: Neut -> WithEnv Bool
exhaust' (_ :< NeutVar _) = return True
exhaust' (_ :< NeutPi (_, tdom) tcod) = do
  b1 <- exhaust' tdom
  b2 <- exhaust' tcod
  return $ b1 && b2
exhaust' (_ :< NeutPiIntro (_, tdom) e) = do
  b1 <- exhaust' tdom
  b2 <- exhaust' e
  return $ b1 && b2
exhaust' (_ :< NeutPiElim e1 e2) = do
  b1 <- exhaust' e1
  b2 <- exhaust' e2
  return $ b1 && b2
exhaust' (_ :< NeutSigma (_, t1) t2) = do
  b1 <- exhaust' t1
  b2 <- exhaust' t2
  return $ b1 && b2
exhaust' (_ :< NeutSigmaIntro e1 e2) = do
  b1 <- exhaust' e1
  b2 <- exhaust' e2
  return $ b1 && b2
exhaust' (_ :< NeutSigmaElim e1 (_, _) e2) = do
  b1 <- exhaust' e1
  b2 <- exhaust' e2
  return $ b1 && b2
exhaust' (_ :< NeutMu _ e) = exhaust' e
exhaust' (_ :< NeutIndex _) = return True
exhaust' (_ :< NeutIndexIntro _) = return True
exhaust' (_ :< NeutIndexElim _ []) = return False -- empty clause?
exhaust' (meta :< NeutIndexElim e1 branchList@((l, _):_)) = do
  b1 <- exhaust' e1
  t <- lookupTypeEnv' meta
  let labelList = map fst branchList
  case t of
    _ :< NeutIndex "int" -> return $ b1 && (IndexDefault `elem` labelList)
    _ ->
      if IndexDefault `elem` labelList
        then return b1
        else case l of
               IndexLabel x -> do
                 set <- lookupIndexSet x
                 if length set <= length (nub labelList)
                   then return True
                   else return False
               _ -> return False
exhaust' (_ :< NeutUniv _) = return True
exhaust' (_ :< NeutHole _) = return False
exhaust' (_ :< NeutSubst (_ :< NeutHole x) sub) =
  case lookup x sub of
    Nothing -> return False
    Just e  -> exhaust' e
exhaust' e@(_ :< NeutSubst _ _) = do
  e' <- reduce e
  exhaust' e'
