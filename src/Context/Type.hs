module Context.Type
  ( lookup,
    insert,
  )
where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.WeakTerm
import Prelude hiding (lookup)

lookup :: Hint -> DD.DefiniteDescription -> App WeakTerm
lookup m k = do
  tenv <- readRef' typeEnv
  case Map.lookup k tenv of
    Just value ->
      return value
    Nothing ->
      Throw.raiseCritical m $ "`" <> DD.reify k <> "` is not found in the term type environment."

insert :: DD.DefiniteDescription -> WeakTerm -> App ()
insert k v =
  modifyRef' typeEnv $ Map.insert k v
