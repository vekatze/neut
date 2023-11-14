module Context.Clarify
  ( initialize,
    getAuxEnv,
    insertToAuxEnv,
    checkIfAlreadyRegistered,
  )
where

import Context.App
import Context.App.Internal
import Context.CompDefinition qualified as CompDefinition
import Data.HashMap.Strict qualified as Map
import Prelude hiding (lookup)

initialize :: App ()
initialize = do
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
