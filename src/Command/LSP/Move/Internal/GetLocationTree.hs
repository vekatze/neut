module Command.LSP.Move.Internal.GetLocationTree
  ( Handle,
    new,
    getLocationTree,
  )
where

import Error.Move.Run (liftMaybe)
import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Cache qualified as Cache
import Kernel.Move.Context.Path qualified as Path
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Rule.Cache qualified as Cache
import Kernel.Rule.LocationTree qualified as LT
import Kernel.Rule.Source
import Kernel.Rule.Target (Target (Peripheral))

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
