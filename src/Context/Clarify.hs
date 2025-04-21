module Context.Clarify
  ( clearAuxEnv,
    getAuxEnv,
    insertToAuxEnv,
    checkIfAlreadyRegistered,
    toCompStmtList,
  )
where

import Context.App
import Context.App.Internal
import Context.CompDefinition qualified as CompDefinition
import Data.HashMap.Strict qualified as Map
import Rule.Comp (CompStmt, fromDefTuple)
import Prelude hiding (lookup)

clearAuxEnv :: App ()
clearAuxEnv = do
  writeRef' compAuxEnv mempty

getAuxEnv :: App CompDefinition.DefMap
getAuxEnv =
  readRef' compAuxEnv

insertToAuxEnv :: CompDefinition.DefKey -> CompDefinition.DefValue -> App ()
insertToAuxEnv k v =
  modifyRef' compAuxEnv $ Map.insert k v

checkIfAlreadyRegistered :: CompDefinition.DefKey -> App Bool
checkIfAlreadyRegistered k = do
  Map.member k <$> getAuxEnv

toCompStmtList :: CompDefinition.DefMap -> [CompStmt]
toCompStmtList defMap = do
  map fromDefTuple $ Map.toList defMap
