module Scene.LSP.GetAllCachesInModule
  ( getAllLocationCachesInModule,
    getAllCompletionCachesInModule,
  )
where

import Context.App
import Context.Cache qualified as Cache
import Context.Module (getAllSourcePathInModule)
import Context.Path (getSourceCompletionCachePath)
import Data.Maybe (catMaybes)
import Entity.Cache
import Entity.Module
import Entity.Source
import Entity.Target (Target (Peripheral))
import Path
import Scene.Source.ShiftToLatest (shiftToLatest)
import UnliftIO.Async

getAllLocationCachesInModule :: Module -> App [(Source, LocationCache)]
getAllLocationCachesInModule baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  fmap catMaybes $ pooledForConcurrently sourcePathList $ \path -> getLocationCache baseModule path

getLocationCache :: Module -> Path Abs File -> App (Maybe (Source, LocationCache))
getLocationCache baseModule filePath = do
  source <- shiftToLatest $ Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cacheOrNone <- Cache.loadLocationCache Peripheral source
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)

getAllCompletionCachesInModule :: Module -> App [(Source, CompletionCache)]
getAllCompletionCachesInModule baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  fmap catMaybes $ pooledForConcurrently sourcePathList $ \path -> getCompletionCache baseModule path

getCompletionCache :: Module -> Path Abs File -> App (Maybe (Source, CompletionCache))
getCompletionCache baseModule filePath = do
  source <- shiftToLatest $ Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cacheOrNone <- getSourceCompletionCachePath Peripheral source >>= Cache.loadCompletionCacheOptimistically
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)
