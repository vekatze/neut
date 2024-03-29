module Context.CompDefinition
  ( DefKey,
    DefValue,
    DefMap,
    insert,
    union,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Entity.Comp
import Entity.DefiniteDescription qualified as DD
import Entity.Ident
import Entity.Opacity
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
