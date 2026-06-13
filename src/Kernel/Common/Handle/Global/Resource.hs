module Kernel.Common.Handle.Global.Resource
  ( Handle,
    ResourceLayout (..),
    new,
    insert,
    lookup,
    layoutOf,
  )
where

import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Prelude hiding (lookup)

data ResourceLayout
  = Direct
  | Flattened Int
  deriving (Show, Eq)

layoutOf :: TM.Term -> Maybe ResourceLayout
layoutOf term =
  case term of
    _ :< TM.Prim (PV.Int _ _ value) ->
      if value < 0
        then Just Direct
        else Just $ Flattened $ fromInteger value
    _ ->
      Nothing

newtype Handle = Handle
  { resourceMapRef :: IORef (Map.HashMap DD.DefiniteDescription ResourceLayout)
  }

new :: IO Handle
new = do
  resourceMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> DD.DefiniteDescription -> ResourceLayout -> IO ()
insert h dd resourceSize =
  atomicModifyIORef' (resourceMapRef h) $ \mp ->
    (Map.insert dd resourceSize mp, ())

lookup :: Handle -> DD.DefiniteDescription -> IO (Maybe ResourceLayout)
lookup h dd = do
  resourceMap <- readIORef (resourceMapRef h)
  return $ Map.lookup dd resourceMap
