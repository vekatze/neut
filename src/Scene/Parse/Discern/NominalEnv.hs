module Scene.Parse.Discern.NominalEnv
  ( extendNominalEnv,
    extendNominalEnvWithoutInsert,
    joinNominalEnv,
  )
where

import Context.App
import Context.UnusedVariable qualified as UnusedVariable
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.NominalEnv
import Entity.VarDefKind

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
