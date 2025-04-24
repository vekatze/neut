module Move.Context.Elaborate
  ( HandleEnv (..),
    createNewEnv,
    initialize,
  )
where

import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal
import Move.Scene.Elaborate.Handle.Constraint qualified as Constraint
import Move.Scene.Elaborate.Handle.Hole qualified as Hole

initialize :: App ()
initialize = do
  writeRef' suspendedEnv []
  writeRef' weakTypeEnv IntMap.empty

data HandleEnv
  = HandleEnv
  { constraintHandle :: Constraint.Handle,
    holeHandle :: Hole.Handle
  }

createNewEnv :: IO HandleEnv
createNewEnv = do
  constraintHandle <- Constraint.new
  holeHandle <- Hole.new
  return $ HandleEnv {..}
