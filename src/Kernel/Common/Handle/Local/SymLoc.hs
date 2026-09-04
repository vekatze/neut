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
  { _localVarListRef :: IORef [LVT.Entry]
  }

new :: IO Handle
new = do
  _localVarListRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> Ident -> Loc -> Loc -> IO ()
insert h x startLoc endLoc = do
  unless (isHole x) $ do
    modifyIORef' (_localVarListRef h) ((startLoc, endLoc, x) :)

get :: Handle -> IO LVT.LocalVarTree
get h = do
  entries <- readIORef (_localVarListRef h)
  return $ LVT.fromList $ reverse entries
