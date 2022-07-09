module Context.Locator.Main (new) where

import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import qualified Data.Containers.ListUtils as ListUtils
import Data.IORef
import qualified Data.Text as T
import Entity.Const
import Entity.Hint hiding (new)
import Entity.Module
import Entity.ModuleChecksum
import qualified Entity.ModuleID as MID
import Entity.Source
import Path

new :: Locator.Config -> IO Locator.Context
new cfg = do
  currentGlobalLocator <- getGlobalLocator (Locator.mainModule cfg) (Locator.currentSource cfg)
  currentGlobalLocatorRef <- newIORef currentGlobalLocator
  currentLocalLocatorListRef <- newIORef []
  activeGlobalLocatorListRef <- newIORef [currentGlobalLocator]
  activeLocalLocatorListRef <- newIORef []
  return
    Locator.Context
      { Locator.pushToCurrentLocalLocator =
          pushToCurrentLocalLocator currentLocalLocatorListRef,
        Locator.popFromCurrentLocalLocator =
          popFromCurrentLocalLocator (Locator.throwCtx cfg) currentLocalLocatorListRef,
        Locator.attachCurrentLocator =
          attachCurrentLocator currentGlobalLocatorRef currentLocalLocatorListRef,
        Locator.activateGlobalLocator =
          modifyIORef' activeGlobalLocatorListRef . (:),
        Locator.activatePartialLocator =
          modifyIORef' activeLocalLocatorListRef . (:),
        Locator.clearActiveLocators =
          clearActiveLocators activeGlobalLocatorListRef activeLocalLocatorListRef,
        Locator.getPossibleReferents =
          getPossibleReferents
            currentGlobalLocatorRef
            currentLocalLocatorListRef
            activeGlobalLocatorListRef
            activeLocalLocatorListRef
      }

pushToCurrentLocalLocator :: IORef [T.Text] -> T.Text -> IO ()
pushToCurrentLocalLocator currentLocalLocatorListRef s = do
  localLocatorList <- readIORef currentLocalLocatorListRef
  case localLocatorList of
    [] ->
      writeIORef currentLocalLocatorListRef [s]
    headLocalLocator : _ ->
      writeIORef currentLocalLocatorListRef $ headLocalLocator <> nsSep <> s : localLocatorList

popFromCurrentLocalLocator :: Throw.Context -> IORef [T.Text] -> Hint -> IO T.Text
popFromCurrentLocalLocator ctx currentLocalLocatorListRef m = do
  localLocatorList <- readIORef currentLocalLocatorListRef
  case localLocatorList of
    [] ->
      Throw.raiseError ctx m "there is no section to end"
    headLocalLocator : rest -> do
      writeIORef currentLocalLocatorListRef rest
      return headLocalLocator

attachCurrentLocator :: IORef T.Text -> IORef [T.Text] -> T.Text -> IO T.Text
attachCurrentLocator currentGlobalLocatorRef currentLocalLocatorListRef name = do
  currentGlobalLocator <- readIORef currentGlobalLocatorRef
  currentLocalLocatorList <- readIORef currentLocalLocatorListRef
  case currentLocalLocatorList of
    [] ->
      return $ currentGlobalLocator <> definiteSep <> name
    currentLocalLocator : _ ->
      return $ currentGlobalLocator <> definiteSep <> currentLocalLocator <> nsSep <> name

clearActiveLocators :: IORef [T.Text] -> IORef [T.Text] -> IO ()
clearActiveLocators activeGlobalLocatorListRef activeLocalLocatorListRef = do
  writeIORef activeGlobalLocatorListRef []
  writeIORef activeLocalLocatorListRef []

getPossibleReferents ::
  IORef T.Text ->
  IORef [T.Text] ->
  IORef [T.Text] ->
  IORef [T.Text] ->
  T.Text ->
  Bool ->
  IO [T.Text]
getPossibleReferents currentGlobalLocatorRef currentLocalLocatorRef activeGlobalLocatorListRef activeLocalLocatorListRef name isDefinite = do
  currentGlobalLocator <- readIORef currentGlobalLocatorRef
  currentLocalLocator <- readIORef currentLocalLocatorRef
  if isDefinite
    then return [name]
    else do
      globalLocatorList <- readIORef activeGlobalLocatorListRef
      let globalNameList = mapPrefix definiteSep globalLocatorList name
      localLocatorList <- readIORef activeLocalLocatorListRef
      let localNameList = mapPrefix nsSep localLocatorList name
      let sectionalNameList = getSectionalNameList currentGlobalLocator currentLocalLocator name
      return $ ListUtils.nubOrd $ name : globalNameList ++ localNameList ++ sectionalNameList

mapPrefix :: T.Text -> [T.Text] -> T.Text -> [T.Text]
mapPrefix sep prefixList basename =
  map (<> sep <> basename) prefixList

getSectionalNameList :: T.Text -> [T.Text] -> T.Text -> [T.Text]
getSectionalNameList currentGlobalLocator currentLocalLocatorList name = do
  map (\localLocator -> currentGlobalLocator <> definiteSep <> localLocator <> nsSep <> name) currentLocalLocatorList

getGlobalLocator :: Module -> Source -> IO T.Text
getGlobalLocator mainModule source = do
  sigTail <- getLocatorTail source
  case MID.getModuleID mainModule $ sourceModule source of
    MID.This ->
      return $ T.intercalate "." $ defaultModulePrefix : sigTail
    MID.That (ModuleChecksum checksum) ->
      return $ T.intercalate "." $ checksum : sigTail

getLocatorTail :: Source -> IO [T.Text]
getLocatorTail source = do
  relFilePath <- stripProperPrefix (getSourceDir $ sourceModule source) $ sourceFilePath source
  (relFilePath', _) <- splitExtension relFilePath
  return $ T.splitOn "/" $ T.pack $ toFilePath relFilePath'
