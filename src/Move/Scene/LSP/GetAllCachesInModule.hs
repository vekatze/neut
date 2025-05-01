module Move.Scene.LSP.GetAllCachesInModule
  ( Handle,
    new,
    getAllLocationCachesInModule,
    getAllCompletionCachesInModule,
  )
where

import Data.Maybe (catMaybes)
import Move.Context.Cache qualified as Cache
import Move.Context.EIO (EIO, forP)
import Move.Context.Module (getAllSourcePathInModule)
import Move.Context.Path qualified as Path
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Source.ShiftToLatest qualified as STL
import Path
import Rule.Cache
import Rule.Module
import Rule.Source
import Rule.Target (Target (Peripheral))

data Handle
  = Handle
  { shiftToLatestHandle :: STL.Handle,
    pathHandle :: Path.Handle
  }

new :: Base.Handle -> Handle
new (Base.Handle {..}) = do
  let shiftToLatestHandle = STL.new antecedentHandle
  Handle {..}

getAllLocationCachesInModule :: Handle -> Module -> EIO [(Source, LocationCache)]
getAllLocationCachesInModule h baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  fmap catMaybes $ forP sourcePathList $ \path -> getLocationCache h baseModule path

getLocationCache :: Handle -> Module -> Path Abs File -> EIO (Maybe (Source, LocationCache))
getLocationCache h baseModule filePath = do
  source <-
    STL.shiftToLatest (shiftToLatestHandle h) $
      Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cacheOrNone <- Cache.loadLocationCache (pathHandle h) Peripheral source
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)

getAllCompletionCachesInModule :: Handle -> Module -> EIO [(Source, CompletionCache)]
getAllCompletionCachesInModule h baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  fmap catMaybes $ forP sourcePathList $ \path -> getCompletionCache h baseModule path

getCompletionCache :: Handle -> Module -> Path Abs File -> EIO (Maybe (Source, CompletionCache))
getCompletionCache h baseModule filePath = do
  source <-
    STL.shiftToLatest (shiftToLatestHandle h) $
      Source {sourceFilePath = filePath, sourceModule = baseModule, sourceHint = Nothing}
  cachePath <- Path.getSourceCompletionCachePath (pathHandle h) Peripheral source
  cacheOrNone <- Cache.loadCompletionCacheOptimistically cachePath
  case cacheOrNone of
    Nothing ->
      return Nothing
    Just cache ->
      return $ Just (source, cache)
