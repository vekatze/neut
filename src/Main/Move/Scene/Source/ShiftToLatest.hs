module Main.Move.Scene.Source.ShiftToLatest
  ( Handle,
    new,
    shiftToLatest,
    shiftToLatestModule,
    ShiftMap,
  )
where

import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Language.Common.Move.Raise (raiseError, raiseError')
import Language.Common.Rule.ModuleID qualified as MID
import Main.Move.Context.Antecedent qualified as Antecedent
import Main.Rule.Module
import Main.Rule.Source (Source (sourceModule))
import Main.Rule.Source qualified as Source
import Path
import Path.IO

type ShiftMap = Map.HashMap MID.ModuleID Module

newtype Handle = Handle
  { antecedentHandle :: Antecedent.Handle
  }

new :: Antecedent.Handle -> Handle
new antecedentHandle = do
  Handle {..}

shiftToLatest :: Handle -> Source.Source -> EIO Source.Source
shiftToLatest h source = do
  shiftMap <- liftIO $ Antecedent.get (antecedentHandle h)
  case Map.lookup (moduleID $ sourceModule source) shiftMap of
    Nothing ->
      return source
    Just newModule -> do
      getNewerSource source newModule

shiftToLatestModule :: Handle -> Module -> EIO Module
shiftToLatestModule h m = do
  shiftMap <- liftIO $ Antecedent.get (antecedentHandle h)
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
