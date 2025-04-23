module Move.Scene.Elaborate.Handle.Hole
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.WeakTerm qualified as WT
import Prelude hiding (lookup)

newtype Handle = Handle
  { holeEnvRef :: IORef (IntMap.IntMap (WT.WeakTerm, WT.WeakTerm))
  }

new :: App Handle
new = do
  holeEnvRef <- asks App.holeEnv
  return $ Handle {..}

insert :: Handle -> Int -> WT.WeakTerm -> WT.WeakTerm -> IO ()
insert h i e1 e2 =
  modifyIORef' (holeEnvRef h) $ IntMap.insert i (e1, e2)

lookup :: Handle -> Int -> IO (Maybe (WT.WeakTerm, WT.WeakTerm))
lookup h i =
  IntMap.lookup i <$> readIORef (holeEnvRef h)
