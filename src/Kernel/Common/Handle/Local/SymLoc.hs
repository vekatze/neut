module Kernel.Common.Handle.Local.SymLoc
  ( Handle (..),
    new,
    insert,
    get,
  )
where

import Control.Monad (unless)
import Data.IORef
import Kernel.Common.LocalVarTree qualified as LVT
import Language.Common.Ident
import Logger.Hint

newtype Handle = Handle
  { _localVarMapRef :: IORef LVT.LocalVarTree
  }

new :: IO Handle
new = do
  _localVarMapRef <- newIORef LVT.empty
  return $ Handle {..}

insert :: Handle -> Ident -> Loc -> Loc -> IO ()
insert h x startLoc endLoc = do
  unless (isHole x) $ do
    modifyIORef' (_localVarMapRef h) $ LVT.insert startLoc endLoc x

get :: Handle -> IO LVT.LocalVarTree
get h =
  readIORef (_localVarMapRef h)
