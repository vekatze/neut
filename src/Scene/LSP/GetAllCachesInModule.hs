module Scene.LSP.GetAllCachesInModule (getAllCachesInModule) where

import Context.App
import Context.AppM
import Context.Cache qualified as Cache
import Context.Path (getSourceCachePath)
import Control.Monad
import Control.Monad.Trans
import Data.Maybe (catMaybes)
import Entity.Cache qualified as Cache
import Entity.LocationTree
import Entity.Module
import Entity.Source
import Path
import Path.IO

getAllCachesInModule :: Module -> AppM [(Path Abs File, LocationTree)]
getAllCachesInModule baseModule = do
  (_, filePathList) <- listDirRecur $ getSourceDir baseModule
  fmap catMaybes $ forM filePathList $ \path -> lift (getLocationTree baseModule path)

getLocationTree :: Module -> Path Abs File -> App (Maybe (Path Abs File, LocationTree))
getLocationTree baseModule filePath = do
  let source = Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cachePath <- getSourceCachePath source
  cacheOrNone <- Cache.loadCacheOptimistically cachePath
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (filePath, Cache.locationTree cache)
