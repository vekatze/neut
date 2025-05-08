module Kernel.Common.Move.Handle.Local.TopCandidate
  ( new,
    insert,
    get,
  )
where

import Data.IORef
import Kernel.Common.Rule.Handle.Local.TopCandidate
import Kernel.Common.Rule.TopCandidate

new :: IO Handle
new = do
  _topCandidateEnvRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> TopCandidate -> IO ()
insert h cand = do
  modifyIORef' (_topCandidateEnvRef h) $ (:) cand

get :: Handle -> IO [TopCandidate]
get h =
  readIORef (_topCandidateEnvRef h)
