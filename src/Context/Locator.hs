module Context.Locator
  ( initialize,
    withLiftedSection,
    attachCurrentLocator,
    activateGlobalLocator,
    activateDefiniteLocator,
    clearActiveLocators,
    getPossibleReferents,
    getMainDefiniteDescription,
    withSection,
  )
where

import Context.App
import Context.App.Internal
import Context.Module qualified as Module
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.DefiniteLocator as DL
import Entity.LocalLocator qualified as LL
import Entity.Module qualified as Module
import Entity.Section qualified as S
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Path

-- the structure of a name of a global variable:
--
--     some.path.to.item::foo.bar.buz.qux.some-function
--                            ---
--                            ↑ section name
--     -----------------  ---------------
--     ↑ global locator   ↑ local locator
--     ----------------------------------
--     ↑ definite locator
--     ------------------------------------------------
--     ↑ the definite description of a global variable `some-function` (up-to module alias)

-- terms:
--   - current locator
--     - current global locator: the global locator of the file that is being parsed
--     - current local locator: the local locator of the current section that is being parsed
--   - active locator
--     - active global locator: a global locator that is used when resolving global names
--     - active local locator: a local locator that is used when resolving global names

initialize :: App ()
initialize = do
  mainModule <- Module.getMainModule
  currentSource <- readRef "currentSource" currentSource
  cgl <- constructGlobalLocator mainModule currentSource
  writeRef currentGlobalLocator cgl
  writeRef' currentSectionStack []
  writeRef' activeGlobalLocatorList [cgl, SGL.llvmGlobalLocator]
  writeRef' activeDefiniteLocatorList []

activateGlobalLocator :: SGL.StrictGlobalLocator -> App ()
activateGlobalLocator sgl = do
  agls <- readRef' activeGlobalLocatorList
  writeRef' activeGlobalLocatorList $ sgl : agls

activateDefiniteLocator :: DL.DefiniteLocator -> App ()
activateDefiniteLocator sgl = do
  adls <- readRef' activeDefiniteLocatorList
  writeRef' activeDefiniteLocatorList $ sgl : adls

withLiftedSection :: (MonadTrans t, Monad (t App)) => S.Section -> t App a -> t App a
withLiftedSection section computation = do
  c <- lift $ readRef' currentSectionStack
  lift $ writeRef' currentSectionStack $ section : c
  result <- computation
  lift $ writeRef' currentSectionStack c
  return result

attachCurrentLocator ::
  BN.BaseName ->
  App DD.DefiniteDescription
attachCurrentLocator name = do
  cgl <- readRef "currentGlobalLocator" currentGlobalLocator
  currentSectionStack <- readRef' currentSectionStack
  return $ DD.new cgl $ LL.new currentSectionStack name

clearActiveLocators :: App ()
clearActiveLocators = do
  writeRef' activeGlobalLocatorList []
  writeRef' activeDefiniteLocatorList []

getPossibleReferents :: LL.LocalLocator -> App [DD.DefiniteDescription]
getPossibleReferents localLocator = do
  cgl <- readRef "currentGlobalLocator" currentGlobalLocator
  currentSectionStack <- readRef' currentSectionStack
  agls <- readRef' activeGlobalLocatorList
  adls <- readRef' activeDefiniteLocatorList
  let dds1 = map (`DD.new` localLocator) agls
  let dds2 = map (`DD.newByDefiniteLocator` localLocator) adls
  let dd = getDefaultDefiniteDescription cgl currentSectionStack localLocator
  return $ ListUtils.nubOrd $ dd : dds1 ++ dds2

getDefaultDefiniteDescription :: SGL.StrictGlobalLocator -> [S.Section] -> LL.LocalLocator -> DD.DefiniteDescription
getDefaultDefiniteDescription gl sectionStack ll =
  DD.new gl $
    LL.new (LL.sectionStack ll ++ sectionStack) (LL.baseName ll)

constructGlobalLocator :: Module.Module -> Source.Source -> App SGL.StrictGlobalLocator
constructGlobalLocator mainModule source = do
  sourceLocator <- getSourceLocator source
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = Module.getID mainModule $ Source.sourceModule source,
        SGL.sourceLocator = sourceLocator
      }

getSourceLocator :: Source.Source -> App SL.SourceLocator
getSourceLocator source = do
  relFilePath <- stripProperPrefix (Module.getSourceDir $ Source.sourceModule source) $ Source.sourceFilePath source
  (relFilePath', _) <- splitExtension relFilePath
  return $ SL.SourceLocator relFilePath'

getMainDefiniteDescription ::
  Source.Source ->
  App (Maybe DD.DefiniteDescription)
getMainDefiniteDescription source = do
  b <- isMainFile source
  if b
    then Just <$> attachCurrentLocator BN.main
    else return Nothing

isMainFile :: Source.Source -> App Bool
isMainFile source = do
  sourcePathList <- mapM Module.getSourcePath $ Map.elems $ Module.moduleTarget (Source.sourceModule source)
  return $ elem (Source.sourceFilePath source) sourcePathList

withSection :: S.Section -> App a -> App a
withSection section action =
  runIdentityT $ withLiftedSection section $ lift action
