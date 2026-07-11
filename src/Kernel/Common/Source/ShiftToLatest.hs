module Kernel.Common.Source.ShiftToLatest
  ( Handle,
    new,
    shiftToLatest,
    shiftToLatestModule,
    shiftToLatestModuleID,
    ShiftMap,
  )
where

import App.App (App)
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Kernel.Common.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Module
import Kernel.Common.Source (Source (sourceModule))
import Kernel.Common.Source qualified as Source
import Language.Common.ModuleID qualified as MID
import Path ((</>))

type ShiftMap = Map.HashMap MID.ModuleID Module

newtype Handle = Handle
  { antecedentHandle :: Antecedent.Handle
  }

new :: Antecedent.Handle -> Handle
new antecedentHandle = do
  Handle {..}

shiftToLatest :: Handle -> Source.Source -> App Source.Source
shiftToLatest h source = do
  shiftMap <- liftIO $ Antecedent.get (antecedentHandle h)
  case Map.lookup (moduleID $ sourceModule source) shiftMap of
    Nothing ->
      return source
    Just newModule -> do
      getNewerSource source newModule

shiftToLatestModule :: Handle -> Module -> App Module
shiftToLatestModule h m = do
  shiftMap <- liftIO $ Antecedent.get (antecedentHandle h)
  case Map.lookup (moduleID m) shiftMap of
    Nothing ->
      return m
    Just newModule -> do
      return newModule

shiftToLatestModuleID :: Handle -> MID.ModuleID -> IO MID.ModuleID
shiftToLatestModuleID h mid = do
  shiftMap <- Antecedent.get (antecedentHandle h)
  case Map.lookup mid shiftMap of
    Nothing ->
      return mid
    Just newModule ->
      return $ moduleID newModule

getNewerSource :: Source.Source -> Module -> App Source.Source
getNewerSource source newModule = do
  relSourceFilePath <- Source.getRelPathFromSourceDir source
  let newSourceFilePath = getSourceDir newModule </> relSourceFilePath
  let newSource =
        Source.Source
          { sourceFilePath = newSourceFilePath,
            sourceModule = newModule,
            sourceHint = Source.sourceHint source,
            sourceImportLocator = Source.sourceImportLocator source
          }
  Source.ensureSourceExistence newSource
  return newSource
