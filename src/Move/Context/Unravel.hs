module Move.Context.Unravel
  ( initialize,
  )
where

import Data.HashMap.Strict qualified as Map
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.App.Internal

initialize :: App ()
initialize = do
  writeRef' visitEnv Map.empty
  writeRef' sourceChildrenMap Map.empty
  Antecedent.initialize
