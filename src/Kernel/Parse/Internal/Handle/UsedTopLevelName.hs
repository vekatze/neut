module Kernel.Parse.Internal.Handle.UsedTopLevelName
  ( Handle,
    new,
    insert,
    get,
  )
where

import Data.IORef
import Data.Set qualified as S
import Language.Common.DefiniteDescription qualified as DD

newtype Handle = Handle
  { usedTopLevelNameSetRef :: IORef (S.Set DD.DefiniteDescription)
  }

new :: IO Handle
new = do
  usedTopLevelNameSetRef <- newIORef S.empty
  return $ Handle {..}

insert :: Handle -> DD.DefiniteDescription -> IO ()
insert h dd =
  atomicModifyIORef' (usedTopLevelNameSetRef h) $ \s ->
    (S.insert dd s, ())

get :: Handle -> IO [DD.DefiniteDescription]
get h =
  S.toList <$> readIORef (usedTopLevelNameSetRef h)
