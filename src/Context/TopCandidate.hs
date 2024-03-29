module Context.TopCandidate
  ( initialize,
    insert,
    get,
  )
where

import Context.App
import Context.App.Internal
import Entity.TopCandidate

initialize :: App ()
initialize =
  writeRef' topCandidateEnv []

insert :: TopCandidate -> App ()
insert cand = do
  modifyRef' topCandidateEnv $ (:) cand

get :: App [TopCandidate]
get =
  readRef' topCandidateEnv
