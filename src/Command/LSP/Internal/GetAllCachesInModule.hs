module Command.LSP.Internal.GetAllCachesInModule
  ( Handle,
    new,
    getAllLocationCachesInModule,
    getAllCompletionCachesInModule,
  )
where

import Data.Maybe (catMaybes)
import Error.EIO (EIO)
import Error.Run (forP)
import Kernel.Common.Cache
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Module (getAllSourcePathInModule)
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.ManageCache qualified as Cache
import Kernel.Common.Module
import Kernel.Common.Source
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Kernel.Common.Target (Target (Peripheral))
import Path

data Handle = Handle
  { shiftToLatestHandle :: STL.Handle,
    pathHandle :: Path.Handle
  }

new :: Global.Handle -> Handle
new (Global.Handle {..}) = do
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
