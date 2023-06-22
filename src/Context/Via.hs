module Context.Via
  ( initialize,
    initializeActiveViaMap,
    addToActiveViaMap,
    union,
    lookup,
    get,
    getActive,
  )
where

import Context.App
import Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Entity.DefiniteDescription qualified as DD
import Entity.RawIdent
import Entity.ViaMap
import Path
import Prelude hiding (lookup)

initialize :: App ()
initialize =
  writeRef' viaMap Map.empty

initializeActiveViaMap :: App ()
initializeActiveViaMap =
  writeRef' activeViaMap Map.empty

union :: Path Abs File -> ViaMap -> App ()
union path newViaInfo = do
  modifyRef' viaMap $ Map.insertWith Map.union path newViaInfo

lookup :: DD.DefiniteDescription -> App (Map.HashMap RawIdent DD.DefiniteDescription)
lookup consName = do
  vmap <- readRef' activeViaMap
  case Map.lookup consName vmap of
    Just keyInfo ->
      return keyInfo
    Nothing ->
      return Map.empty

get :: Path Abs File -> App ViaMap
get path = do
  nmap <- readRef' viaMap
  case Map.lookup path nmap of
    Just nameSet ->
      return nameSet
    Nothing ->
      return Map.empty

getActive :: App ViaMap
getActive = do
  readRef' activeViaMap

addToActiveViaMap :: ViaMap -> App ()
addToActiveViaMap vm =
  modifyRef' activeViaMap $ Map.union vm
