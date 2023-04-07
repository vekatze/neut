module Context.Definition
  ( insert,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD
import Entity.Opacity qualified as O
import Entity.Term qualified as TM
import Prelude hiding (lookup, read)

insert :: O.Opacity -> DD.DefiniteDescription -> TM.Term -> App ()
insert opacity name e =
  when (opacity == O.Transparent) $
    modifyRef' defMap $
      Map.insert name e

lookup :: DD.DefiniteDescription -> App (Maybe TM.Term)
lookup name = do
  denv <- readRef' defMap
  return $ Map.lookup name denv
