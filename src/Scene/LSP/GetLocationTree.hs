module Scene.LSP.GetLocationTree (getLocationTree) where

import Context.App
import Context.Cache qualified as Cache
import Context.Unravel qualified as Unravel
import Control.Lens hiding (Iso, List)
import Entity.Cache qualified as Cache
import Entity.LocationTree qualified as LT
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Types
import Scene.Source.Reflect qualified as Source
import Scene.Unravel

getLocationTree ::
  (J.HasTextDocument p a1, J.HasUri a1 Uri) =>
  p ->
  App (Maybe LT.LocationTree)
getLocationTree params = do
  case uriToFilePath $ params ^. J.textDocument . J.uri of
    Nothing ->
      return Nothing
    Just fp -> do
      mSrc <- Source.reflect fp
      case mSrc of
        Nothing -> do
          return Nothing
        Just src -> do
          Unravel.initialize
          _ <- unravel' src
          mCache <- Cache.loadCacheOptimistically src
          case mCache of
            Nothing -> do
              return Nothing
            Just cache -> do
              return $ Just $ Cache.locationTree cache
