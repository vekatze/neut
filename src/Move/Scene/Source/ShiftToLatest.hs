module Move.Scene.Source.ShiftToLatest
  ( Handle,
    new,
    shiftToLatest,
    shiftToLatestModule,
    ShiftMap,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseError, raiseError')
import Path
import Path.IO
import Rule.Module
import Rule.ModuleID qualified as MID
import Rule.Source (Source (sourceModule))
import Rule.Source qualified as Source

type ShiftMap = Map.HashMap MID.ModuleID Module

newtype Handle
  = Handle
  { shiftMapRef :: IORef (Map.HashMap MID.ModuleID Module)
  }

new :: App Handle
new = do
  shiftMapRef <- asks App.antecedentMap
  return $ Handle {..}

shiftToLatest :: Handle -> Source.Source -> EIO Source.Source
shiftToLatest h source = do
  shiftMap <- liftIO $ readIORef (shiftMapRef h)
  case Map.lookup (moduleID $ sourceModule source) shiftMap of
    Nothing ->
      return source
    Just newModule -> do
      getNewerSource source newModule

shiftToLatestModule :: Handle -> Module -> EIO Module
shiftToLatestModule h m = do
  shiftMap <- liftIO $ readIORef (shiftMapRef h)
  case Map.lookup (moduleID m) shiftMap of
    Nothing ->
      return m
    Just newModule -> do
      return newModule

getNewerSource :: Source.Source -> Module -> EIO Source.Source
getNewerSource source newModule = do
  relSourceFilePath <- Source.getRelPathFromSourceDir source
  let newSourceFilePath = getSourceDir newModule </> relSourceFilePath
  let newSource =
        Source.Source
          { sourceFilePath = newSourceFilePath,
            sourceModule = newModule,
            sourceHint = Source.sourceHint source
          }
  b <- doesFileExist newSourceFilePath
  if b
    then return newSource
    else do
      relPath <- Source.getRelPathFromSourceDir source
      case Source.sourceHint source of
        Nothing -> do
          raiseError' $
            "The file `"
              <> T.pack (toFilePath relPath)
              <> "` is missing in the module `"
              <> MID.reify (moduleID newModule)
              <> "`"
        Just m -> do
          raiseError m $
            "The file `"
              <> T.pack (toFilePath relPath)
              <> "` is missing in the module `"
              <> MID.reify (moduleID newModule)
              <> "`"
