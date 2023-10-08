module Context.Locator
  ( initialize,
    attachCurrentLocator,
    attachPublicCurrentLocator,
    getCurrentGlobalLocator,
    activateSpecifiedNames,
    clearActiveLocators,
    getPossibleReferents,
    getMainDefiniteDescription,
  )
where

import Context.App
import Context.App.Internal
import Context.Module qualified as Module
import Context.Throw qualified as Throw
import Control.Monad
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.Maybe (maybeToList)
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Module qualified as Module
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.TopNameMap (TopNameMap)
import Path


-- the structure of a name of a global variable:
--
--     some.path.to.item.some-function
--     ----------------- -------------
--     ↑ global locator  ↑ local locator
--     ------------------------------------------------
--     ↑ the definite description of a global variable `some-function` (up-to module alias)

initialize :: App ()
initialize = do
  mainModule <- Module.getMainModule
  currentSource <- readRef "currentSource" currentSource
  cgl <- constructGlobalLocator mainModule currentSource
  writeRef currentGlobalLocator cgl
  writeRef' activeGlobalLocatorList [cgl, SGL.llvmGlobalLocator, SGL.natGlobalLocator]
  writeRef' activeDefiniteDescriptionList Map.empty

activateSpecifiedNames :: TopNameMap -> SGL.StrictGlobalLocator -> [(Hint, LL.LocalLocator)] -> App ()
activateSpecifiedNames topNameMap sgl lls = do
  forM_ lls $ \(m, ll) -> do
    let dd = DD.new sgl ll
    case Map.lookup dd topNameMap of
      Nothing ->
        Throw.raiseError m $ "the name `" <> LL.reify ll <> "` isn't defined in the module"
      Just _ -> do
       aenv <- readRef' activeDefiniteDescriptionList
       when (Map.member ll aenv) $ do
         Throw.raiseError m $ "the top-level name `" <> LL.reify ll <> "` is already imported"
       modifyRef' activeDefiniteDescriptionList $ Map.insert ll dd

attachCurrentLocator ::
  BN.BaseName ->
  App DD.DefiniteDescription
attachCurrentLocator name = do
  cgl <- getCurrentGlobalLocator
  return $ DD.new cgl $ LL.new name

attachPublicCurrentLocator ::
  BN.BaseName ->
  App DD.DefiniteDescription
attachPublicCurrentLocator name = do
  cgl <- getCurrentGlobalLocator
  return $ DD.new cgl $ LL.new name

getCurrentGlobalLocator :: App SGL.StrictGlobalLocator
getCurrentGlobalLocator =
  readRef "currentGlobalLocator" currentGlobalLocator

clearActiveLocators :: App ()
clearActiveLocators = do
  writeRef' activeGlobalLocatorList []

getPossibleReferents :: LL.LocalLocator -> App [DD.DefiniteDescription]
getPossibleReferents localLocator = do
  cgl <- getCurrentGlobalLocator
  agls <- readRef' activeGlobalLocatorList
  importedDDs <- getImportedReferents localLocator
  let dds = map (`DD.new` localLocator) agls
  let dd = DD.new cgl localLocator
  return $ ListUtils.nubOrd $ dd : dds ++ importedDDs

getImportedReferents :: LL.LocalLocator -> App [DD.DefiniteDescription]
getImportedReferents ll = do
  maybeToList . Map.lookup ll <$> readRef' activeDefiniteDescriptionList

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
  let sourcePathList = Module.getTargetPathList $ Source.sourceModule source
  return $ elem (Source.sourceFilePath source) sourcePathList
