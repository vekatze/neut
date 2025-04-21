module Move.Scene.LSP.GetAllCachesInModule
  ( getAllLocationCachesInModule,
    getAllCompletionCachesInModule,
  )
where

import Data.Maybe (catMaybes)
import Move.Context.App
import Move.Context.Cache qualified as Cache
import Move.Context.EIO (toApp)
import Move.Context.Module (getAllSourcePathInModule)
import Move.Context.Path qualified as Path
import Move.Scene.Source.ShiftToLatest (shiftToLatest)
import Path
import Rule.Cache
import Rule.Module
import Rule.Source
import Rule.Target (Target (Peripheral))
import UnliftIO.Async

getAllLocationCachesInModule :: Module -> App [(Source, LocationCache)]
getAllLocationCachesInModule baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  fmap catMaybes $ pooledForConcurrently sourcePathList $ \path -> getLocationCache baseModule path

getLocationCache :: Module -> Path Abs File -> App (Maybe (Source, LocationCache))
getLocationCache baseModule filePath = do
  source <- shiftToLatest $ Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  h <- Path.new
  cacheOrNone <- toApp $ Cache.loadLocationCache h Peripheral source
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
  h <- Path.new
  cachePath <- toApp (Path.getSourceCompletionCachePath h Peripheral source)
  cacheOrNone <- Cache.loadCompletionCacheOptimistically cachePath
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)
