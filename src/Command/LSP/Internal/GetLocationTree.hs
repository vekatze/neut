module Command.LSP.Internal.GetLocationTree
  ( Handle,
    new,
    getLocationTree,
  )
where

import App.App (App)
import App.Run (liftMaybe)
import Kernel.Common.Cache qualified as Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.LocationTree qualified as LT
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.Source
import Kernel.Common.Target (Target (Peripheral))

newtype Handle = Handle
  { pathHandle :: Path.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
  Handle {..}

getLocationTree ::
  Handle ->
  Source ->
  App LT.LocationTree
getLocationTree h src = do
  cache <- Cache.loadLocationCache (pathHandle h) Peripheral src >>= liftMaybe
  return $ Cache.locationTree cache
