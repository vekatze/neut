module Context.Clarify
  ( initialize,
    getAuxEnv,
    insertToAuxEnv,
    isAlreadyRegistered,
  )
where

import Context.App
import Context.App.Internal
import Context.CompDefinition qualified as CompDefinition
import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD

initialize :: App ()
initialize = do
  writeRef' compDefMap mempty

getAuxEnv :: App CompDefinition.DefMap
getAuxEnv =
  readRef' compDefMap

insertToAuxEnv :: CompDefinition.DefKey -> CompDefinition.DefValue -> App ()
insertToAuxEnv k v =
  modifyRef' compDefMap $ Map.insert k v

isAlreadyRegistered :: DD.DefiniteDescription -> App Bool
isAlreadyRegistered dd =
  Map.member dd <$> getAuxEnv
