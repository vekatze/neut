module Scene.LSP.GetLocationTree (getLocationTree) where

import Context.AppM
import Context.Cache qualified as Cache
import Context.Throw qualified as Throw
import Context.Unravel qualified as Unravel
import Control.Monad.Trans
import Entity.Cache qualified as Cache
import Entity.LocationTree qualified as LT
import Entity.Source
import Entity.Target (Target (Peripheral))
import Scene.Unravel

getLocationTree ::
  Source ->
  AppM LT.LocationTree
getLocationTree src = do
  lift Unravel.initialize
  resultOrError <- lift $ Throw.execute $ unravel' Peripheral src
  case resultOrError of
    Left _ ->
      liftMaybe Nothing
    Right _ -> do
      cache <- lift (Cache.loadLocationCache Peripheral src) >>= liftMaybe
      return $ Cache.locationTree cache
