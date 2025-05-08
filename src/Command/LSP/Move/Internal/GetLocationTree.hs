module Command.LSP.Move.Internal.GetLocationTree
  ( Handle,
    new,
    getLocationTree,
  )
where

import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.ManageCache qualified as Cache
import Kernel.Common.Rule.Cache qualified as Cache
import Kernel.Common.Rule.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.LocationTree qualified as LT
import Kernel.Common.Rule.Source
import Kernel.Common.Rule.Target (Target (Peripheral))
import Library.Error.Move.Run (liftMaybe)
import Library.Error.Rule.EIO (EIO)

newtype Handle = Handle
  { pathHandle :: Path.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  Handle {..}

getLocationTree ::
  Handle ->
  Source ->
  EIO LT.LocationTree
getLocationTree h src = do
  cache <- Cache.loadLocationCache (pathHandle h) Peripheral src >>= liftMaybe
  return $ Cache.locationTree cache
