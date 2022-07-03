module Context.Locator.Main (new) where

import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import qualified Data.Containers.ListUtils as ListUtils
import Data.IORef
import qualified Data.Text as T
import Entity.Global
import Entity.Hint hiding (new)

new :: Locator.Config -> IO Locator.Axis
new cfg = do
  currentGlobalLocatorRef <- newIORef $ Locator.currentGlobalLocator cfg
  currentLocalLocatorListRef <- newIORef []
  activeGlobalLocatorListRef <- newIORef []
  activeLocalLocatorListRef <- newIORef []
  return
    Locator.Axis
      { Locator.pushToCurrentLocalLocator =
          pushToCurrentLocalLocator currentLocalLocatorListRef,
        Locator.popFromCurrentLocalLocator =
          popFromCurrentLocalLocator (Locator.throwCtx cfg) currentLocalLocatorListRef,
        Locator.setCurrentGlobalLocator =
          writeIORef currentGlobalLocatorRef,
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
popFromCurrentLocalLocator context currentLocalLocatorListRef m = do
  localLocatorList <- readIORef currentLocalLocatorListRef
  case localLocatorList of
    [] ->
      Throw.raiseError context m "there is no section to end"
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
      return $ ListUtils.nubOrd $ globalNameList ++ localNameList ++ sectionalNameList

mapPrefix :: T.Text -> [T.Text] -> T.Text -> [T.Text]
mapPrefix sep prefixList basename =
  map (<> sep <> basename) prefixList

getSectionalNameList :: T.Text -> [T.Text] -> T.Text -> [T.Text]
getSectionalNameList currentGlobalLocator currentLocalLocatorList name = do
  -- currentGlobalLocator <- readIORef currentGlobalLocatorRef
  -- currentLocalLocatorList <- readIORef currentLocalLocatorListRef
  map (\localLocator -> currentGlobalLocator <> definiteSep <> localLocator <> nsSep <> name) currentLocalLocatorList

-- getSectionalNameList :: IORef T.Text -> IORef [T.Text] -> T.Text -> IO [T.Text]
-- getSectionalNameList currentGlobalLocatorRef currentLocalLocatorListRef name = do
--   currentGlobalLocator <- readIORef currentGlobalLocatorRef
--   currentLocalLocatorList <- readIORef currentLocalLocatorListRef
--   return $ map (\localLocator -> currentGlobalLocator <> definiteSep <> localLocator <> nsSep <> name) currentLocalLocatorList
