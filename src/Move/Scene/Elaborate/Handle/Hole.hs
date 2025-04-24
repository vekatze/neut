module Move.Scene.Elaborate.Handle.Hole
  ( Handle,
    new,
    insert,
    lookup,
    insertSubst,
    getSubst,
    setSubst,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.HoleID qualified as HID
import Rule.HoleSubst qualified as HS
import Rule.Ident
import Rule.WeakTerm qualified as WT
import Prelude hiding (lookup)

data Handle = Handle
  { holeEnvRef :: IORef (IntMap.IntMap (WT.WeakTerm, WT.WeakTerm)),
    holeSubstRef :: IORef HS.HoleSubst
  }

new :: App Handle
new = do
  holeEnvRef <- asks App.holeEnv
  holeSubstRef <- asks App.holeSubst
  return $ Handle {..}

insert :: Handle -> Int -> WT.WeakTerm -> WT.WeakTerm -> IO ()
insert h i e1 e2 =
  modifyIORef' (holeEnvRef h) $ IntMap.insert i (e1, e2)

lookup :: Handle -> Int -> IO (Maybe (WT.WeakTerm, WT.WeakTerm))
lookup h i =
  IntMap.lookup i <$> readIORef (holeEnvRef h)

insertSubst :: Handle -> HID.HoleID -> [Ident] -> WT.WeakTerm -> IO ()
insertSubst h holeID xs e =
  modifyIORef' (holeSubstRef h) $ HS.insert holeID xs e

getSubst :: Handle -> IO HS.HoleSubst
getSubst h =
  readIORef (holeSubstRef h)

setSubst :: Handle -> HS.HoleSubst -> IO ()
setSubst h =
  writeIORef (holeSubstRef h)
