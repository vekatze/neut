module Move.Context.Type
  ( initialize,
    lookup,
    lookupMaybe,
    insert,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.WeakTerm
import Prelude hiding (lookup)

initialize :: App ()
initialize = do
  writeRef' typeEnv Map.empty

lookup :: Hint -> DD.DefiniteDescription -> App WeakTerm
lookup m k = do
  valueOrNone <- lookupMaybe k
  case valueOrNone of
    Just value ->
      return value
    Nothing ->
      Throw.raiseCritical m $ "`" <> DD.reify k <> "` is not found in the term type environment."

lookupMaybe :: DD.DefiniteDescription -> App (Maybe WeakTerm)
lookupMaybe k = do
  tenv <- readRef' typeEnv
  return $ Map.lookup k tenv

insert :: DD.DefiniteDescription -> WeakTerm -> App ()
insert k v =
  modifyRef' typeEnv $ Map.insert k v
