module Move.Context.Elaborate
  ( HandleEnv (..),
    createNewEnv,
  )
where

import Move.Scene.Elaborate.Handle.Constraint qualified as Constraint
import Move.Scene.Elaborate.Handle.Hole qualified as Hole
import Move.Scene.Elaborate.Handle.WeakType qualified as WeakType

data HandleEnv
  = HandleEnv
  { constraintHandle :: Constraint.Handle,
    holeHandle :: Hole.Handle,
    weakTypeHandle :: WeakType.Handle
  }

createNewEnv :: IO HandleEnv
createNewEnv = do
  constraintHandle <- Constraint.new
  holeHandle <- Hole.new
  weakTypeHandle <- WeakType.new
  return $ HandleEnv {..}
