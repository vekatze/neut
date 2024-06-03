module Scene.LSP.GetAllCachesInModule
  ( getAllCachesInModule,
    getAllCompletionCachesInModule,
  )
where

import Context.App
import Context.Cache qualified as Cache
import Context.Path (getSourceCachePath, getSourceCompletionCachePath)
import Data.Maybe (catMaybes)
import Entity.Cache
import Entity.Module
import Entity.Source
import Entity.Target
import Path
import Path.IO
import UnliftIO.Async

getAllCachesInModule :: Module -> App [(Source, Cache)]
getAllCachesInModule baseModule = do
  (_, filePathList) <- listDirRecur $ getSourceDir baseModule
  fmap catMaybes $ forConcurrently filePathList $ \path -> getCache baseModule path

getCache :: Module -> Path Abs File -> App (Maybe (Source, Cache))
getCache baseModule filePath = do
  let source = Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cacheOrNone <- getSourceCachePath Peripheral source >>= Cache.loadCacheOptimistically
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)

getAllCompletionCachesInModule :: Module -> App [(Source, CompletionCache)]
getAllCompletionCachesInModule baseModule = do
  (_, filePathList) <- listDirRecur $ getSourceDir baseModule
  fmap catMaybes $ forConcurrently filePathList $ \path -> getCompletionCache baseModule path

getCompletionCache :: Module -> Path Abs File -> App (Maybe (Source, CompletionCache))
getCompletionCache baseModule filePath = do
  let source = Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cacheOrNone <-
    getSourceCompletionCachePath Peripheral source
      >>= Cache.loadCompletionCacheOptimistically
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)
