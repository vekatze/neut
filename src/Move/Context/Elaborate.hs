module Move.Context.Elaborate
  ( initialize,
  )
where

import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal
import Rule.HoleSubst qualified as HS

initialize :: App ()
initialize = do
  writeRef' suspendedEnv []
  writeRef' holeEnv IntMap.empty
  writeRef' weakTypeEnv IntMap.empty
  writeRef' holeSubst HS.empty
