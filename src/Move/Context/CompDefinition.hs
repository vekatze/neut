module Move.Context.CompDefinition
  ( DefKey,
    DefValue,
    DefMap,
    insert,
    union,
    lookup,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Rule.Comp
import Rule.DefiniteDescription qualified as DD
import Rule.Ident
import Rule.Opacity
import Prelude hiding (lookup, read)

type DefKey = DD.DefiniteDescription

type DefValue = (Opacity, [Ident], Comp)

type DefMap = Map.HashMap DD.DefiniteDescription (Opacity, [Ident], Comp)

insert :: DefKey -> DefValue -> App ()
insert k v =
  modifyRef' compEnv $ Map.insert k v

union :: DefMap -> App ()
union otherEnv =
  modifyRef' compEnv $ Map.union otherEnv

lookup :: DefKey -> App (Maybe DefValue)
lookup k = do
  cenv <- readRef' compEnv
  return $ Map.lookup k cenv
