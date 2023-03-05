module Context.Implicit
  ( insert,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Entity.ArgNum qualified as AN
import Entity.DefiniteDescription qualified as DD
import Prelude hiding (lookup)

insert :: DD.DefiniteDescription -> AN.ArgNum -> App ()
insert k impArgNum =
  modifyRef' impArgEnv $ Map.insert k impArgNum

lookup :: DD.DefiniteDescription -> App (Maybe AN.ArgNum)
lookup k =
  Map.lookup k <$> readRef' impArgEnv
