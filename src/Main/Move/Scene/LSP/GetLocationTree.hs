module Main.Move.Scene.LSP.GetLocationTree
  ( Handle,
    new,
    getLocationTree,
  )
where

import Main.Move.Context.Cache qualified as Cache
import Main.Move.Context.EIO (EIO, liftMaybe)
import Main.Move.Context.Path qualified as Path
import Main.Move.Scene.Init.Base qualified as Base
import Main.Rule.Cache qualified as Cache
import Main.Rule.LocationTree qualified as LT
import Main.Rule.Source
import Main.Rule.Target (Target (Peripheral))

newtype Handle = Handle
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
