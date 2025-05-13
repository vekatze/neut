module Kernel.Common.Move.Handle.Local.SymLoc
  ( new,
    insert,
    get,
  )
where

import Logger.Rule.Hint
import Control.Monad (unless)
import Data.IORef
import Kernel.Common.Rule.Handle.Local.SymLoc
import Kernel.Common.Rule.LocalVarTree qualified as LVT
import Language.Common.Rule.Ident

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
