module Context.Implicit
  ( insert,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD
import Entity.ImpArgNum qualified as I
import Prelude hiding (lookup)

insert :: DD.DefiniteDescription -> I.ImpArgNum -> App ()
insert k v =
  modifyRef' impEnv $ Map.insert k v

lookup :: DD.DefiniteDescription -> App (Maybe I.ImpArgNum)
lookup k =
  Map.lookup k <$> readRef' impEnv
