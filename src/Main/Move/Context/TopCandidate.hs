module Main.Move.Context.TopCandidate
  ( Handle,
    new,
    insert,
    get,
  )
where

import Data.IORef
import Main.Rule.TopCandidate

newtype Handle = Handle
  { topCandidateEnvRef :: IORef [TopCandidate]
  }

new :: IO Handle
new = do
  topCandidateEnvRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> TopCandidate -> IO ()
insert h cand = do
  modifyIORef' (topCandidateEnvRef h) $ (:) cand

get :: Handle -> IO [TopCandidate]
get h =
  readIORef (topCandidateEnvRef h)
