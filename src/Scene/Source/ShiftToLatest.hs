module Scene.Source.ShiftToLatest
  ( shiftToLatest,
    ShiftMap,
  )
where

import Context.App
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.Module
import Entity.ModuleID qualified as MID
import Entity.Source (Source (sourceModule))
import Entity.Source qualified as Source
import Path

type ShiftMap = Map.HashMap MID.ModuleID Module

shiftToLatest :: ShiftMap -> Source.Source -> App Source.Source
shiftToLatest shiftMap source = do
  case Map.lookup (moduleID $ sourceModule source) shiftMap of
    Nothing ->
      return source
    Just newModule -> do
      getNewerSource source newModule

getNewerSource :: Source.Source -> Module -> App Source.Source
getNewerSource source newModule = do
  relSourceFilePath <- Source.getRelPathFromSourceDir source
  let newSourceFilePath = getSourceDir newModule </> relSourceFilePath
  let newSource =
        Source.Source
          { sourceFilePath = newSourceFilePath,
            sourceModule = newModule,
            sourceHint = Source.sourceHint source
          }
  b <- Path.doesFileExist newSourceFilePath
  if b
    then return newSource
    else do
      relPath <- Source.getRelPathFromSourceDir source
      case Source.sourceHint source of
        Nothing -> do
          Throw.raiseError' $
            "the file `"
              <> T.pack (toFilePath relPath)
              <> "` is missing in the module `"
              <> MID.reify (moduleID newModule)
              <> "`"
        Just m -> do
          Throw.raiseError m $
            "the file `"
              <> T.pack (toFilePath relPath)
              <> "` is missing in the module `"
              <> MID.reify (moduleID newModule)
              <> "`"
