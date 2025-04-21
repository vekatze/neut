module Move.Scene.LSP.GetLocationTree
  ( getLocationTree,
  )
where

import Control.Monad.Trans
import Move.Context.AppM
import Move.Context.Cache qualified as Cache
import Move.Context.EIO (toApp)
import Move.Context.Path qualified as Path
import Rule.Cache qualified as Cache
import Rule.LocationTree qualified as LT
import Rule.Source
import Rule.Target (Target (Peripheral))

getLocationTree ::
  Source ->
  AppM LT.LocationTree
getLocationTree src = do
  h <- lift Path.new
  cache <- lift (toApp $ Cache.loadLocationCache h Peripheral src) >>= liftMaybe
  return $ Cache.locationTree cache
