module Scene.Clarify.Context
  ( Context (..),
  )
where

import qualified Context.CompDefinition as CompDefinition
import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import qualified Entity.DefiniteDescription as DD

class
  ( CompDefinition.Context m,
    Gensym.Context m,
    Locator.Context m
  ) =>
  Context m
  where
  initialize :: m ()
  getAuxEnv :: m CompDefinition.DefMap
  insertToAuxEnv :: CompDefinition.DefKey -> CompDefinition.DefValue -> m ()
  isAlreadyRegistered :: DD.DefiniteDescription -> m Bool

-- data Context = Context
--   { base :: App.Context,
--     auxEnvRef :: IORef CompDefinition.DefMap
--   }

-- readAuxEnv :: Context -> IO CompDefinition.DefMap
-- readAuxEnv ctx =
--   readIORef $ auxEnvRef ctx

-- insertToAuxEnv :: Context -> CompDefinition.DefKey -> CompDefinition.DefValue -> IO ()
-- insertToAuxEnv ctx k v =
--   modifyIORef (auxEnvRef ctx) $ Map.insert k v

-- isAlreadyRegistered :: Context -> DD.DefiniteDescription -> IO Bool
-- isAlreadyRegistered ctx dd = do
--   auxEnv <- readAuxEnv ctx
--   return $ Map.member dd auxEnv

-- specialize :: App.Context -> IO Context
-- specialize ctx = do
--   _auxEnvRef <- newIORef Map.empty
--   return $
--     Context
--       { base = ctx,
--         auxEnvRef = _auxEnvRef
--       }
