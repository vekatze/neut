module Move.Scene.LSP.GetLocationTree
  ( getLocationTree,
  )
where

import Move.Context.AppM
import Move.Context.Cache qualified as Cache
import Control.Monad.Trans
import Rule.Cache qualified as Cache
import Rule.LocationTree qualified as LT
import Rule.Source
import Rule.Target (Target (Peripheral))

getLocationTree ::
  Source ->
  AppM LT.LocationTree
getLocationTree src = do
  cache <- lift (Cache.loadLocationCache Peripheral src) >>= liftMaybe
  return $ Cache.locationTree cache
