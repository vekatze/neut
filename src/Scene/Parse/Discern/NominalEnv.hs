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

extendNominalEnv :: Hint -> Ident -> NominalEnv -> App NominalEnv
extendNominalEnv m newVar nenv = do
  UnusedVariable.insert m newVar
  return $ (Ident.toText newVar, (m, newVar)) : nenv

extendNominalEnvWithoutInsert :: Hint -> Ident -> NominalEnv -> NominalEnv
extendNominalEnvWithoutInsert m newVar nenv = do
  (Ident.toText newVar, (m, newVar)) : nenv

joinNominalEnv :: NominalEnv -> NominalEnv -> App NominalEnv
joinNominalEnv newNominalEnv oldNominalEnv = do
  case newNominalEnv of
    [] ->
      return oldNominalEnv
    (_, (m, x)) : rest -> do
      oldNominalEnv' <- extendNominalEnv m x oldNominalEnv
      joinNominalEnv rest oldNominalEnv'
