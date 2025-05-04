module Main.Move.Scene.LSP.GetAllCachesInModule
  ( Handle,
    new,
    getAllLocationCachesInModule,
    getAllCompletionCachesInModule,
  )
where

import Data.Maybe (catMaybes)
import Main.Move.Context.Cache qualified as Cache
import Main.Move.Context.EIO (EIO, forP)
import Main.Move.Context.Module (getAllSourcePathInModule)
import Main.Move.Context.Path qualified as Path
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Source.ShiftToLatest qualified as STL
import Main.Rule.Cache
import Main.Rule.Module
import Main.Rule.Source
import Main.Rule.Target (Target (Peripheral))
import Path

data Handle = Handle
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
