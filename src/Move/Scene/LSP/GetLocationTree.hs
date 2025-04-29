module Move.Scene.LSP.GetLocationTree
  ( Handle,
    new,
    getLocationTree,
  )
where

import Move.Context.App (App)
import Move.Context.Cache qualified as Cache
import Move.Context.EIO (EIO, liftMaybe)
import Move.Context.Path qualified as Path
import Rule.Cache qualified as Cache
import Rule.LocationTree qualified as LT
import Rule.Source
import Rule.Target (Target (Peripheral))

newtype Handle
  = Handle
  { pathHandle :: Path.Handle
  }

new :: Path.Handle -> App Handle
new pathHandle = do
  return $ Handle {..}

getLocationTree ::
  Handle ->
  Source ->
  EIO LT.LocationTree
getLocationTree h src = do
  cache <- Cache.loadLocationCache (pathHandle h) Peripheral src >>= liftMaybe
  return $ Cache.locationTree cache
