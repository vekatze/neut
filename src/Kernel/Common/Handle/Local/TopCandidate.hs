module Kernel.Common.Handle.Local.TopCandidate
  ( Handle,
    new,
    insert,
    get,
  )
where

import Data.IORef
import Kernel.Common.RuleHandle.Local.TopCandidate
import Kernel.Common.TopCandidate

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
