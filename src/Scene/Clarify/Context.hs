module Scene.Clarify.Context
  ( Context (..),
    readAuxEnv,
    insertToAuxEnv,
    isAlreadyRegistered,
    specialize,
  )
where

import qualified Context.App as App
import qualified Context.CompDefinition as CompDefinition
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Entity.DefiniteDescription as DD

data Context = Context
  { base :: App.Context,
    auxEnvRef :: IORef CompDefinition.DefMap
  }

readAuxEnv :: Context -> IO CompDefinition.DefMap
readAuxEnv ctx =
  readIORef $ auxEnvRef ctx

insertToAuxEnv :: Context -> CompDefinition.DefKey -> CompDefinition.DefValue -> IO ()
insertToAuxEnv ctx k v =
  modifyIORef (auxEnvRef ctx) $ Map.insert k v

isAlreadyRegistered :: Context -> DD.DefiniteDescription -> IO Bool
isAlreadyRegistered ctx dd = do
  auxEnv <- readAuxEnv ctx
  return $ Map.member dd auxEnv

specialize :: App.Context -> IO Context
specialize ctx = do
  _auxEnvRef <- newIORef Map.empty
  return $
    Context
      { base = ctx,
        auxEnvRef = _auxEnvRef
      }
