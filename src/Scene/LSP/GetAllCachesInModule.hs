module Scene.LSP.GetAllCachesInModule
  ( getAllCachesInModule,
    getAllCompletionCachesInModule,
  )
where

import Context.App
import Context.Cache qualified as Cache
import Context.Module (getAllSourcePathInModule)
import Context.Path (getSourceCachePath, getSourceCompletionCachePath)
import Data.Maybe (catMaybes)
import Entity.Cache
import Entity.Module
import Entity.Source
import Path
import Scene.Source.ShiftToLatest (shiftToLatest)
import UnliftIO.Async

getAllCachesInModule :: Module -> App [(Source, Cache)]
getAllCachesInModule baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  fmap catMaybes $ forConcurrently sourcePathList $ \path -> getCache baseModule path

getCache :: Module -> Path Abs File -> App (Maybe (Source, Cache))
getCache baseModule filePath = do
  source <- shiftToLatest $ Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cacheOrNone <- getSourceCachePath source >>= Cache.loadCacheOptimistically
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)

getAllCompletionCachesInModule :: Module -> App [(Source, CompletionCache)]
getAllCompletionCachesInModule baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  fmap catMaybes $ forConcurrently sourcePathList $ \path -> getCompletionCache baseModule path

getCompletionCache :: Module -> Path Abs File -> App (Maybe (Source, CompletionCache))
getCompletionCache baseModule filePath = do
  source <- shiftToLatest $ Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cacheOrNone <- getSourceCompletionCachePath source >>= Cache.loadCompletionCacheOptimistically
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)
