module Move.Context.TopCandidate
  ( Handle,
    new,
    initialize,
    insert,
    get,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.TopCandidate

newtype Handle = Handle
  { topCandidateEnvRef :: IORef [TopCandidate]
  }

new :: App Handle
new = do
  topCandidateEnvRef <- asks App.topCandidateEnv
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
