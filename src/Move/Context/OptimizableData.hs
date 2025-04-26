module Move.Context.OptimizableData
  ( Handle,
    new,
    insert,
    lookupH,
  )
where

import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.DefiniteDescription qualified as DD
import Rule.OptimizableData
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { optDataMapRef :: IORef (Map.HashMap DD.DefiniteDescription OptimizableData)
  }

new :: App Handle
new = do
  optDataMapRef <- asks App.optDataMap
  return $ Handle {..}

insert :: Handle -> DD.DefiniteDescription -> OptimizableData -> IO ()
insert h dd grp = do
  modifyIORef' (optDataMapRef h) $ Map.insert dd grp

lookupH :: Handle -> DD.DefiniteDescription -> IO (Maybe OptimizableData)
lookupH h dd = do
  optDataMap <- readIORef (optDataMapRef h)
  return $ Map.lookup dd optDataMap
