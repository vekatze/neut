module Kernel.Move.Context.SymLoc
  ( Handle,
    new,
    insert,
    get,
  )
where

import Control.Monad (unless)
import Data.IORef
import Kernel.Rule.LocalVarTree qualified as LVT
import Language.Common.Rule.Ident
import Logger.Rule.Hint

newtype Handle = Handle
  { localVarMapRef :: IORef LVT.LocalVarTree
  }

new :: IO Handle
new = do
  localVarMapRef <- newIORef LVT.empty
  return $ Handle {..}

insert :: Handle -> Ident -> Loc -> Loc -> IO ()
insert h x startLoc endLoc = do
  unless (isHole x) $ do
    modifyIORef' (localVarMapRef h) $ LVT.insert startLoc endLoc x

get :: Handle -> IO LVT.LocalVarTree
get h =
  readIORef (localVarMapRef h)
