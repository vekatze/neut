module Language.Term.Eq (eqType, eqTypes) where

import Control.Comonad.Cofree
import Data.Map.Strict qualified as Map
import Language.Common.Binder (BinderF)
import Language.Common.Ident (Ident)
import Language.Term.Term qualified as TM

type VarMap = Map.Map Ident Ident

eqType :: TM.Type -> TM.Type -> Bool
eqType = eqTypeWithEnv Map.empty

eqTypeWithEnv :: VarMap -> TM.Type -> TM.Type -> Bool
eqTypeWithEnv env (_ :< type1) (_ :< type2) =
  case (type1, type2) of
    (TM.Tau, TM.Tau) ->
      True
    (TM.TVar x1, TM.TVar x2) ->
      case Map.lookup x1 env of
        Just x2' ->
          x2 == x2'
        Nothing ->
          x1 == x2
    (TM.TVarGlobal _ dd1, TM.TVarGlobal _ dd2) ->
      dd1 == dd2
    (TM.TyApp t1 args1, TM.TyApp t2 args2) ->
      eqTypeWithEnv env t1 t2
        && length args1 == length args2
        && and (zipWith (eqTypeWithEnv env) args1 args2)
    (TM.Pi pk1 impArgs1 expArgs1 defaultArgs1 cod1, TM.Pi pk2 impArgs2 expArgs2 defaultArgs2 cod2) -> do
      let (envAfterImp, impArgsEq) = eqAndExtendImpArgs env impArgs1 impArgs2
      let (envAfterExp, expArgsEq) = eqAndExtendImpArgs envAfterImp expArgs1 expArgs2
      let (envAfterDefault, defaultArgsEq) = eqAndExtendImpArgs envAfterExp defaultArgs1 defaultArgs2
      pk1 == pk2
        && impArgsEq
        && expArgsEq
        && defaultArgsEq
        && eqTypeWithEnv envAfterDefault cod1 cod2
    (TM.Data _ name1 es1, TM.Data _ name2 es2) ->
      name1 == name2 && and (zipWith (eqTypeWithEnv env) es1 es2)
    (TM.Box t1, TM.Box t2) ->
      eqTypeWithEnv env t1 t2
    (TM.BoxNoema t1, TM.BoxNoema t2) ->
      eqTypeWithEnv env t1 t2
    (TM.Code t1, TM.Code t2) ->
      eqTypeWithEnv env t1 t2
    (TM.PrimType pt1, TM.PrimType pt2) ->
      pt1 == pt2
    (TM.Void, TM.Void) ->
      True
    (TM.Resource dd1 resourceID1, TM.Resource dd2 resourceID2) ->
      dd1 == dd2 && resourceID1 == resourceID2
    _ ->
      False

eqAndExtendImpArgs :: VarMap -> [BinderF TM.Type] -> [BinderF TM.Type] -> (VarMap, Bool)
eqAndExtendImpArgs env bs1 bs2 =
  case (bs1, bs2) of
    ([], []) ->
      (env, True)
    ((_, k1, x1, t1) : rest1, (_, k2, x2, t2) : rest2) -> do
      let typeEq = eqTypeWithEnv env t1 t2
      let env' = Map.insert x1 x2 env
      let (envFinal, restEq) = eqAndExtendImpArgs env' rest1 rest2
      (envFinal, k1 == k2 && typeEq && restEq)
    _ ->
      (env, False)

eqTypes :: [TM.Type] -> [TM.Type] -> Bool
eqTypes ts1 ts2 =
  length ts1 == length ts2 && and (zipWith (eqTypeWithEnv Map.empty) ts1 ts2)
