module Context.Locator.Main (new) where

import qualified Context.Locator as Locator
import Control.Monad.IO.Class
import qualified Data.Containers.ListUtils as ListUtils
import Data.IORef
import qualified Entity.BaseName as BN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.DefiniteLocator as DL
import qualified Entity.LocalLocator as LL
import Entity.Module
import qualified Entity.ModuleID as MID
import qualified Entity.Section as S
import Entity.Source
import Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import Path

new :: Locator.Config -> IO Locator.Context
new cfg = do
  currentGlobalLocator <- getGlobalLocator (Locator.mainModule cfg) (Locator.currentSource cfg)
  currentGlobalLocatorRef <- newIORef currentGlobalLocator
  currentSectionStackRef <- newIORef []
  activeGlobalLocatorListRef <- newIORef [currentGlobalLocator, SGL.llvmGlobalLocator]
  activeDefiniteLocatorListRef <- newIORef []
  return
    Locator.Context
      { Locator.withSection =
          withSection currentSectionStackRef,
        Locator.attachCurrentLocator =
          attachCurrentLocator currentGlobalLocatorRef currentSectionStackRef,
        Locator.activateGlobalLocator =
          modifyIORef' activeGlobalLocatorListRef . (:),
        Locator.activateDefiniteLocator =
          modifyIORef' activeDefiniteLocatorListRef . (:),
        Locator.clearActiveLocators =
          clearActiveLocators activeGlobalLocatorListRef activeDefiniteLocatorListRef,
        Locator.getPossibleReferents =
          getPossibleReferents
            currentGlobalLocatorRef
            currentSectionStackRef
            activeGlobalLocatorListRef
            activeDefiniteLocatorListRef
      }

withSection :: MonadIO m => IORef [S.Section] -> S.Section -> m a -> m a
withSection currentSectionStackRef section computation = do
  currentSectionStack <- liftIO $ readIORef currentSectionStackRef
  liftIO $ modifyIORef' currentSectionStackRef $ const $ section : currentSectionStack
  result <- computation
  liftIO $ writeIORef currentSectionStackRef currentSectionStack
  return result

attachCurrentLocator ::
  IORef SGL.StrictGlobalLocator ->
  IORef [S.Section] ->
  BN.BaseName ->
  IO DD.DefiniteDescription
attachCurrentLocator currentGlobalLocatorRef currentSectionStackRef name = do
  currentGlobalLocator <- readIORef currentGlobalLocatorRef
  currentSectionStack <- readIORef currentSectionStackRef
  return $
    DD.new currentGlobalLocator $
      LL.new currentSectionStack name

clearActiveLocators :: IORef [SGL.StrictGlobalLocator] -> IORef [DL.DefiniteLocator] -> IO ()
clearActiveLocators activeGlobalLocatorListRef activeDefiniteLocatorListRef = do
  writeIORef activeGlobalLocatorListRef []
  writeIORef activeDefiniteLocatorListRef []

getPossibleReferents ::
  IORef SGL.StrictGlobalLocator ->
  IORef [S.Section] ->
  IORef [SGL.StrictGlobalLocator] ->
  IORef [DL.DefiniteLocator] ->
  LL.LocalLocator ->
  IO [DD.DefiniteDescription]
getPossibleReferents currentGlobalLocatorRef currentSectionStackRef activeGlobalLocatorListRef activeDefiniteLocatorListRef localLocator = do
  currentGlobalLocator <- readIORef currentGlobalLocatorRef
  currentSectionStack <- readIORef currentSectionStackRef
  globalLocatorList <- readIORef activeGlobalLocatorListRef
  definiteLocatorList <- readIORef activeDefiniteLocatorListRef
  let dds1 = map (`DD.new` localLocator) globalLocatorList
  let dds2 = map (`DD.newByDefiniteLocator` localLocator) definiteLocatorList
  -- let dds3 = getSectionalNameList currentGlobalLocator currentSectionStack name
  let dd = getDefaultDefiniteDescription currentGlobalLocator currentSectionStack localLocator
  return $ ListUtils.nubOrd $ dd : dds1 ++ dds2

getDefaultDefiniteDescription :: SGL.StrictGlobalLocator -> [S.Section] -> LL.LocalLocator -> DD.DefiniteDescription
getDefaultDefiniteDescription gl sectionStack ll =
  DD.new gl $
    LL.new (LL.sectionStack ll ++ sectionStack) (LL.baseName ll)

-- getSectionalNameList :: SGL.StrictGlobalLocator -> [S.Section] -> T.Text -> [DD.DefiniteDescription]
-- getSectionalNameList gl currentSectionStack name = do
--   flip map currentSectionStack $ \sectionStack ->
--     DD.new gl $
--       LL.LocalLocator
--         { LL.sectionStack = sectionStack,
--           LL.baseName = name
--         }

getGlobalLocator :: Module -> Source -> IO SGL.StrictGlobalLocator
getGlobalLocator mainModule source = do
  sourceLocator <- getSourceLocator source
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = MID.getModuleID mainModule $ sourceModule source,
        SGL.sourceLocator = sourceLocator
      }

getSourceLocator :: Source -> IO SL.SourceLocator
getSourceLocator source = do
  relFilePath <- stripProperPrefix (getSourceDir $ sourceModule source) $ sourceFilePath source
  (relFilePath', _) <- splitExtension relFilePath
  return $ SL.SourceLocator relFilePath'
