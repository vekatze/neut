module Scene.LSP.GetLocationTree
  ( getLocationTree,
  )
where

import Context.AppM
import Context.Cache qualified as Cache
import Control.Monad.Trans
import Entity.Cache qualified as Cache
import Entity.LocationTree qualified as LT
import Entity.Source
import Entity.Target (Target (Peripheral))

getLocationTree ::
  Source ->
  AppM LT.LocationTree
getLocationTree src = do
  cache <- lift (Cache.loadLocationCache Peripheral src) >>= liftMaybe
  return $ Cache.locationTree cache
