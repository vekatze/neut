module Context.NameDependence
  ( initialize,
    add,
    get,
  )
where

import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Entity.TopNameMap
import Path

initialize :: App ()
initialize =
  writeRef' nameDependenceMap Map.empty

add :: Path Abs File -> TopNameMap -> App ()
add path autoImportedNameSet = do
  modifyRef' nameDependenceMap $ Map.insertWith Map.union path autoImportedNameSet

get :: Path Abs File -> App TopNameMap
get path = do
  nmap <- readRef' nameDependenceMap
  case Map.lookup path nmap of
    Just nameSet ->
      return nameSet
    Nothing ->
      return Map.empty
