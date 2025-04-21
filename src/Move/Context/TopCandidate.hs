module Move.Context.TopCandidate
  ( initialize,
    insert,
    get,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Rule.TopCandidate

initialize :: App ()
initialize =
  writeRef' topCandidateEnv []

insert :: TopCandidate -> App ()
insert cand = do
  modifyRef' topCandidateEnv $ (:) cand

get :: App [TopCandidate]
get =
  readRef' topCandidateEnv
