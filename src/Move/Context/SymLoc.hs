module Move.Context.SymLoc
  ( Handle,
    new,
    initialize,
    insert,
    get,
  )
where

import Control.Monad (unless)
import Data.IORef
import Rule.Hint
import Rule.Ident
import Rule.LocalVarTree qualified as LVT

newtype Handle
  = Handle
  { localVarMapRef :: IORef LVT.LocalVarTree
  }

new :: IO Handle
new = do
  localVarMapRef <- newIORef LVT.empty
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
