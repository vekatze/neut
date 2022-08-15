module Context.Implicit.Main () where

-- import qualified Context.Implicit as Implicit
-- import qualified Data.HashMap.Strict as Map
-- import Data.IORef
-- import qualified Entity.DefiniteDescription as DD
-- import qualified Entity.ImpArgNum as I
-- import Prelude hiding (lookup)

-- new :: Implicit.Config -> IO Implicit.Context
-- new _ = do
--   impArgEnvRef <- newIORef Map.empty
--   return
--     Implicit.Context
--       { Implicit.insert = insert impArgEnvRef,
--         Implicit.lookup = lookup impArgEnvRef
--       }

-- insert ::
--   IORef (Map.HashMap DD.DefiniteDescription I.ImpArgNum) ->
--   DD.DefiniteDescription ->
--   I.ImpArgNum ->
--   IO ()
-- insert implicitEnvRef name impArgNum =
--   modifyIORef' implicitEnvRef $ Map.insert name impArgNum

-- lookup ::
--   IORef (Map.HashMap DD.DefiniteDescription I.ImpArgNum) ->
--   DD.DefiniteDescription ->
--   IO (Maybe I.ImpArgNum)
-- lookup implicitEnvRef name = do
--   termImplicitEnv <- readIORef implicitEnvRef
--   return $ Map.lookup name termImplicitEnv
