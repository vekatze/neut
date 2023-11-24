module Scene.LSP.GetLocationTree (getLocationTree) where

import Context.App
import Context.Cache qualified as Cache
import Context.Throw qualified as Throw
import Context.Unravel qualified as Unravel
import Entity.Cache qualified as Cache
import Entity.LocationTree qualified as LT
import Entity.Source
import Scene.Unravel

getLocationTree ::
  Source ->
  App (Maybe LT.LocationTree)
getLocationTree src = do
  Unravel.initialize
  resultOrError <- Throw.execute $ unravel' src
  case resultOrError of
    Left _ ->
      return Nothing
    Right _ -> do
      mCache <- Cache.loadCacheOptimistically src
      case mCache of
        Nothing -> do
          return Nothing
        Just cache -> do
          return $ Just $ Cache.locationTree cache
