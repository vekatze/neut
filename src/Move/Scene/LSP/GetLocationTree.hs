module Move.Scene.LSP.GetLocationTree
  ( Handle,
    new,
    getLocationTree,
  )
where

import Move.Context.Cache qualified as Cache
import Move.Context.EIO (EIO, liftMaybe)
import Move.Context.Path qualified as Path
import Move.Scene.Init.Base qualified as Base
import Rule.Cache qualified as Cache
import Rule.LocationTree qualified as LT
import Rule.Source
import Rule.Target (Target (Peripheral))

newtype Handle
  = Handle
  { pathHandle :: Path.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  Handle {..}

getLocationTree ::
  Handle ->
  Source ->
  EIO LT.LocationTree
getLocationTree h src = do
  cache <- Cache.loadLocationCache (pathHandle h) Peripheral src >>= liftMaybe
  return $ Cache.locationTree cache
