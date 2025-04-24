module Move.Context.Elaborate
  ( initialize,
    initializeInferenceEnv,
  )
where

import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal
import Rule.HoleSubst qualified as HS

initialize :: App ()
initialize = do
  initializeInferenceEnv
  writeRef' weakTypeEnv IntMap.empty
  writeRef' holeSubst HS.empty

initializeInferenceEnv :: App ()
initializeInferenceEnv = do
  writeRef' constraintEnv []
  writeRef' suspendedEnv []
  writeRef' holeEnv IntMap.empty
