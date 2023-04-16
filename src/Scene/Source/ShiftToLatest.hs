module Scene.Source.ShiftToLatest (shiftToLatest) where

import Context.Antecedent qualified as Antecedent
import Context.App
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Entity.Module
import Entity.ModuleID qualified as MID
import Entity.Source (Source (sourceModule))
import Entity.Source qualified as Source
import Path

shiftToLatest :: Source.Source -> App Source.Source
shiftToLatest source = do
  case moduleID $ sourceModule source of
    MID.Main ->
      return source
    MID.Base ->
      return source
    MID.Library checksum -> do
      mNewChecksum <- Antecedent.lookup checksum
      case mNewChecksum of
        Nothing ->
          return source
        Just newModule -> do
          getNewerSource source newModule >>= shiftToLatest

getNewerSource :: Source.Source -> Module -> App Source.Source
getNewerSource source newModule = do
  relSourceFilePath <- Source.getRelPathFromSourceDir source
  let newSourceFilePath = getSourceDir newModule </> relSourceFilePath
  let newSource = Source.Source {sourceFilePath = newSourceFilePath, sourceModule = newModule}
  b <- Path.doesFileExist newSourceFilePath
  if b
    then return newSource
    else Throw.raiseError' "the module foo declares incompatible versions to be compatible"
