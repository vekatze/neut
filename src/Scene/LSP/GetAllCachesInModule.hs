module Scene.LSP.GetAllCachesInModule (getAllCachesInModule) where

import Context.App
import Context.Cache qualified as Cache
import Context.Path (getSourceCachePath)
import Data.Maybe (catMaybes)
import Entity.Cache
import Entity.Module
import Entity.Source
import Path
import Path.IO
import UnliftIO.Async

getAllCachesInModule :: Module -> App [(Source, Cache)]
getAllCachesInModule baseModule = do
  (_, filePathList) <- listDirRecur $ getSourceDir baseModule
  fmap catMaybes $ forConcurrently filePathList $ \path -> getLocationTree baseModule path

getLocationTree :: Module -> Path Abs File -> App (Maybe (Source, Cache))
getLocationTree baseModule filePath = do
  let source = Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cachePath <- getSourceCachePath source
  cacheOrNone <- Cache.loadCacheOptimistically cachePath
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)
