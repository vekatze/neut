module Scene.Parse.Discern.NominalEnv
  ( Axis (..),
    emptyAxis,
    extendAxis,
    extendAxisByNominalEnv,
    extendNominalEnvWithoutInsert,
  )
where

import Context.App
import Context.UnusedVariable qualified as UnusedVariable
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.Module
import Entity.NominalEnv
import Entity.VarDefKind

data Axis = Axis
  { _nenv :: NominalEnv,
    currentModule :: Module
  }

emptyAxis :: Module -> Axis
emptyAxis m =
  Axis {_nenv = empty, currentModule = m}

extendAxis :: Hint -> Ident -> VarDefKind -> Axis -> App Axis
extendAxis m newVar k axis = do
  nenv' <- extendNominalEnv m newVar k (_nenv axis)
  return $ axis {_nenv = nenv'}

extendAxisByNominalEnv :: VarDefKind -> NominalEnv -> Axis -> App Axis
extendAxisByNominalEnv k newNominalEnv oldAxis = do
  nenv' <- joinNominalEnv k newNominalEnv (_nenv oldAxis)
  return $ oldAxis {_nenv = nenv'}

extendNominalEnv :: Hint -> Ident -> VarDefKind -> NominalEnv -> App NominalEnv
extendNominalEnv m newVar k nenv = do
  UnusedVariable.insert m newVar k
  return $ (Ident.toText newVar, (m, newVar)) : nenv

extendNominalEnvWithoutInsert :: Hint -> Ident -> NominalEnv -> NominalEnv
extendNominalEnvWithoutInsert m newVar nenv = do
  (Ident.toText newVar, (m, newVar)) : nenv

joinNominalEnv :: VarDefKind -> NominalEnv -> NominalEnv -> App NominalEnv
joinNominalEnv k newNominalEnv oldNominalEnv = do
  case newNominalEnv of
    [] ->
      return oldNominalEnv
    (_, (m, x)) : rest -> do
      oldNominalEnv' <- extendNominalEnv m x k oldNominalEnv
      joinNominalEnv k rest oldNominalEnv'
