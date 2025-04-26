module Move.Context.SymLoc
  ( Handle,
    new,
    initialize,
    insert,
    get,
  )
where

import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Hint
import Rule.Ident
import Rule.LocalVarTree qualified as LVT

newtype Handle
  = Handle
  { localVarMapRef :: IORef LVT.LocalVarTree
  }

new :: App Handle
new = do
  localVarMapRef <- asks App.localVarMap
  return $ Handle {..}

initialize :: Handle -> IO ()
initialize h =
  writeIORef (localVarMapRef h) LVT.empty

insert :: Handle -> Ident -> Loc -> Loc -> IO ()
insert h x startLoc endLoc = do
  unless (isHole x) $ do
    modifyIORef' (localVarMapRef h) $ LVT.insert startLoc endLoc x

get :: Handle -> IO LVT.LocalVarTree
get h =
  readIORef (localVarMapRef h)
