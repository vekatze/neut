module Context.Case.Main.Locator
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

import Context.Env qualified as Env
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.DefiniteLocator qualified as DL
import Entity.LocalLocator qualified as LL
import Entity.Module qualified as Module
import Entity.Section qualified as S
import Entity.Source qualified as Source
import Entity.SourceLocator as SL
import Entity.StrictGlobalLocator qualified as SGL
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
  result <- computation
  Env.setCurrentSectionStack currentSectionStack
  return result

withLiftedSection :: (Context m, MonadTrans t, Monad (t m)) => S.Section -> t m a -> t m a
withLiftedSection section computation = do
  currentSectionStack <- lift Env.getCurrentSectionStack
  lift $ Env.setCurrentSectionStack $ section : currentSectionStack
  result <- computation
  lift $ Env.setCurrentSectionStack currentSectionStack
  return result

attachCurrentLocator ::
  Context m =>
  BN.BaseName ->
  m DD.DefiniteDescription
attachCurrentLocator name = do
  currentGlobalLocator <- getCurrentGlobalLocator
  currentSectionStack <- Env.getCurrentSectionStack
  return $
    DD.new currentGlobalLocator $
      LL.new currentSectionStack name

clearActiveLocators :: Context m => m ()
clearActiveLocators = do
  setActiveGlobalLocatorList []
  setActiveDefiniteLocatorList []

getPossibleReferents ::
  Context m =>
  LL.LocalLocator ->
  m [DD.DefiniteDescription]
getPossibleReferents localLocator = do
  currentGlobalLocator <- getCurrentGlobalLocator
  currentSectionStack <- Env.getCurrentSectionStack
  globalLocatorList <- getActiveGlobalLocatorList
  definiteLocatorList <- getActiveDefiniteLocatorList
  let dds1 = map (`DD.new` localLocator) globalLocatorList
  let dds2 = map (`DD.newByDefiniteLocator` localLocator) definiteLocatorList
  let dd = getDefaultDefiniteDescription currentGlobalLocator currentSectionStack localLocator
  return $ ListUtils.nubOrd $ dd : dds1 ++ dds2

getDefaultDefiniteDescription :: SGL.StrictGlobalLocator -> [S.Section] -> LL.LocalLocator -> DD.DefiniteDescription
getDefaultDefiniteDescription gl sectionStack ll =
  DD.new gl $
    LL.new (LL.sectionStack ll ++ sectionStack) (LL.baseName ll)

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