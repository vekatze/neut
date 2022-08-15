module Case.Main.Locator
  ( Context (..),
    initialize,
    withSection,
    withLiftedSection,
    attachCurrentLocator,
    activateGlobalLocator,
    activateDefiniteLocator,
    clearActiveLocators,
    getPossibleReferents,
    getMainDefiniteDescription,
  )
where

import qualified Context.Env as Env
import qualified Context.Module as Module
import qualified Context.Path as Path
import qualified Context.Throw as Throw
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.HashMap.Strict as Map
import qualified Entity.BaseName as BN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.DefiniteLocator as DL
import qualified Entity.LocalLocator as LL
import qualified Entity.Module as Module
import qualified Entity.Section as S
import qualified Entity.Source as Source
import Entity.SourceLocator as SL
import qualified Entity.StrictGlobalLocator as SGL
import Path

class
  ( Module.Context m,
    Throw.Context m,
    Path.Context m,
    Env.Context m,
    MonadIO m,
    MonadThrow m
  ) =>
  Context m
  where
  setActiveGlobalLocatorList :: [SGL.StrictGlobalLocator] -> m ()
  getActiveGlobalLocatorList :: m [SGL.StrictGlobalLocator]
  setActiveDefiniteLocatorList :: [DL.DefiniteLocator] -> m ()
  getActiveDefiniteLocatorList :: m [DL.DefiniteLocator]
  setCurrentGlobalLocator :: SGL.StrictGlobalLocator -> m ()
  getCurrentGlobalLocator :: m SGL.StrictGlobalLocator

initialize :: Context m => m ()
initialize = do
  mainModule <- Env.getMainModule
  currentSource <- Env.getCurrentSource
  currentGlobalLocator <- constructGlobalLocator mainModule currentSource
  setCurrentGlobalLocator currentGlobalLocator
  Env.setCurrentSectionStack []
  setActiveGlobalLocatorList [currentGlobalLocator, SGL.llvmGlobalLocator]
  setActiveDefiniteLocatorList []

-- new :: Locator.Config -> m Locator.Context
-- new cfg = do
--   currentGlobalLocator <- getGlobalLocator (Locator.mainModule cfg) (Locator.currentSource cfg)
--   currentGlobalLocatorRef <- newIORef currentGlobalLocator
--   currentSectionStackRef <- newIORef []
--   activeGlobalLocatorListRef <- newIORef [currentGlobalLocator, SGL.llvmGlobalLocator]
--   activeDefiniteLocatorListRef <- newIORef []
--   return
--     Locator.Context
--       { Locator.withSection =
--           withSection currentSectionStackRef,
--         Locator.attachCurrentLocator =
--           attachCurrentLocator currentGlobalLocatorRef currentSectionStackRef,
--         Locator.activateGlobalLocator =
--           modifyIORef' activeGlobalLocatorListRef . (:),
--         Locator.activateDefiniteLocator =
--           modifyIORef' activeDefiniteLocatorListRef . (:),
--         Locator.clearActiveLocators =
--           clearActiveLocators activeGlobalLocatorListRef activeDefiniteLocatorListRef,
--         Locator.getPossibleReferents =
--           getPossibleReferents
--             currentGlobalLocatorRef
--             currentSectionStackRef
--             activeGlobalLocatorListRef
--             activeDefiniteLocatorListRef,
--         Locator.getMainDefiniteDescription =
--           getMainDefiniteDescription
--             (Locator.cfg)
--             currentGlobalLocatorRef
--             currentSectionStackRef
--       }

activateGlobalLocator :: Context m => SGL.StrictGlobalLocator -> m ()
activateGlobalLocator sgl = do
  activeGlobalLocatorList <- getActiveGlobalLocatorList
  setActiveGlobalLocatorList $ sgl : activeGlobalLocatorList

activateDefiniteLocator :: Context m => DL.DefiniteLocator -> m ()
activateDefiniteLocator sgl = do
  activeDefiniteLocatorList <- getActiveDefiniteLocatorList
  setActiveDefiniteLocatorList $ sgl : activeDefiniteLocatorList

withSection :: Context m => S.Section -> m a -> m a
withSection section computation = do
  currentSectionStack <- Env.getCurrentSectionStack
  Env.setCurrentSectionStack $ section : currentSectionStack
  -- liftIO $ modifyIORef' currentSectionStackRef $ const $ section : currentSectionStack
  result <- computation
  Env.setCurrentSectionStack currentSectionStack
  -- Env.popFromCurrentSectionStack
  -- liftIO $ writeIORef currentSectionStackRef currentSectionStack
  return result

withLiftedSection :: (Context m, MonadTrans t, Monad (t m)) => S.Section -> t m a -> t m a
withLiftedSection section computation = do
  currentSectionStack <- lift Env.getCurrentSectionStack
  lift $ Env.setCurrentSectionStack $ section : currentSectionStack
  -- liftIO $ modifyIORef' currentSectionStackRef $ const $ section : currentSectionStack
  result <- computation
  lift $ Env.setCurrentSectionStack currentSectionStack
  -- Env.popFromCurrentSectionStack
  -- liftIO $ writeIORef currentSectionStackRef currentSectionStack
  return result

attachCurrentLocator ::
  Context m =>
  BN.BaseName ->
  m DD.DefiniteDescription
attachCurrentLocator name = do
  currentGlobalLocator <- getCurrentGlobalLocator
  -- currentGlobalLocator <- readIORef currentGlobalLocatorRef
  currentSectionStack <- Env.getCurrentSectionStack
  return $
    DD.new currentGlobalLocator $
      LL.new currentSectionStack name

clearActiveLocators ::
  Context m =>
  m ()
clearActiveLocators = do
  -- writeIORef activeGlobalLocatorListRef []
  setActiveGlobalLocatorList []
  setActiveDefiniteLocatorList []

-- writeIORef activeDefiniteLocatorListRef []

getPossibleReferents ::
  Context m =>
  LL.LocalLocator ->
  m [DD.DefiniteDescription]
getPossibleReferents localLocator = do
  currentGlobalLocator <- getCurrentGlobalLocator
  currentSectionStack <- Env.getCurrentSectionStack
  -- globalLocatorList <- readIORef activeGlobalLocatorListRef
  globalLocatorList <- getActiveGlobalLocatorList
  definiteLocatorList <- getActiveDefiniteLocatorList
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

constructGlobalLocator :: Context m => Module.Module -> Source.Source -> m SGL.StrictGlobalLocator
constructGlobalLocator mainModule source = do
  sourceLocator <- getSourceLocator source
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = Module.getID mainModule $ Source.sourceModule source,
        SGL.sourceLocator = sourceLocator
      }

getSourceLocator :: Context m => Source.Source -> m SL.SourceLocator
getSourceLocator source = do
  relFilePath <- stripProperPrefix (Module.getSourceDir $ Source.sourceModule source) $ Source.sourceFilePath source
  (relFilePath', _) <- splitExtension relFilePath
  return $ SL.SourceLocator relFilePath'

getMainDefiniteDescription ::
  Context m =>
  Source.Source ->
  m (Maybe DD.DefiniteDescription)
getMainDefiniteDescription source = do
  b <- isMainFile source
  if b
    then Just <$> attachCurrentLocator BN.main
    else return Nothing

isMainFile ::
  Context m =>
  Source.Source ->
  m Bool
isMainFile source = do
  sourcePathList <- mapM Module.getSourcePath $ Map.elems $ Module.moduleTarget (Source.sourceModule source)
  return $ elem (Source.sourceFilePath source) sourcePathList

-- getMainDefiniteDescription :: Context -> m DD.DefiniteDescription
-- getMainDefiniteDescription ctx = do
--   attachCurrentLocator ctx BN.main

-- b <- isMainFile (App.ctx) source
-- if b
--   then return <$> Locator.getMainDefiniteDescription (App.locator ctx)
--   else return Nothing

-- getMainFunctionName :: App.Context -> Source -> m (Maybe DD.DefiniteDescription)
-- getMainFunctionName ctx source = do
--   b <- isMainFile (App.ctx) source
--   if b
--     then return <$> Locator.getMainDefiniteDescription (App.locator ctx)
--     else return Nothing

-- getSourcePath (Module.throwCtx cfg) (Module.pathCtx cfg) (Module.mainModule cfg)
