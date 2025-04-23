module Move.Scene.Parse.Discern.Handle
  ( Handle (..),
    new,
    extendHandle,
    extendHandleByNominalEnv,
    extendNominalEnvWithoutInsert,
  )
where

import Move.Context.App
import Move.Context.UnusedVariable qualified as UnusedVariable
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Layer
import Rule.Module
import Rule.NominalEnv
import Rule.VarDefKind

data Handle = Handle
  { _nenv :: NominalEnv,
    currentModule :: Module,
    currentLayer :: Layer
  }

new :: Module -> App Handle
new currentModule = do
  return $ Handle {_nenv = empty, currentModule, currentLayer = 0}

extendHandle :: Hint -> Ident -> VarDefKind -> Handle -> App Handle
extendHandle m newVar k axis = do
  nenv' <- extendNominalEnv m newVar (currentLayer axis) k (_nenv axis)
  return $ axis {_nenv = nenv'}

extendHandleByNominalEnv :: VarDefKind -> NominalEnv -> Handle -> App Handle
extendHandleByNominalEnv k newNominalEnv oldHandle = do
  nenv' <- joinNominalEnv k newNominalEnv (_nenv oldHandle)
  return $ oldHandle {_nenv = nenv'}

extendNominalEnv :: Hint -> Ident -> Layer -> VarDefKind -> NominalEnv -> App NominalEnv
extendNominalEnv m newVar l k nenv = do
  UnusedVariable.insert m newVar k
  return $ (Ident.toText newVar, (m, newVar, l)) : nenv

extendNominalEnvWithoutInsert :: Hint -> Ident -> Layer -> NominalEnv -> NominalEnv
extendNominalEnvWithoutInsert m newVar l nenv = do
  (Ident.toText newVar, (m, newVar, l)) : nenv

joinNominalEnv :: VarDefKind -> NominalEnv -> NominalEnv -> App NominalEnv
joinNominalEnv k newNominalEnv oldNominalEnv = do
  case newNominalEnv of
    [] ->
      return oldNominalEnv
    (_, (m, x, l)) : rest -> do
      oldNominalEnv' <- extendNominalEnv m x l k oldNominalEnv
      joinNominalEnv k rest oldNominalEnv'
