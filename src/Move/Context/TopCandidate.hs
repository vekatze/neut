module Move.Context.TopCandidate
  ( Handle,
    new,
    initialize,
    insert,
    get,
  )
where

import Data.IORef
import Rule.TopCandidate

newtype Handle = Handle
  { topCandidateEnvRef :: IORef [TopCandidate]
  }

new :: IO Handle
new = do
  topCandidateEnvRef <- newIORef []
  return $ Handle {..}

initialize :: Handle -> IO ()
initialize h =
  writeIORef (topCandidateEnvRef h) []

insert :: Handle -> TopCandidate -> IO ()
insert h cand = do
  modifyIORef' (topCandidateEnvRef h) $ (:) cand

get :: Handle -> IO [TopCandidate]
get h =
  readIORef (topCandidateEnvRef h)
