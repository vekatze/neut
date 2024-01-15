module Context.Locator
  ( initialize,
    attachCurrentLocator,
    attachPublicCurrentLocator,
    getCurrentGlobalLocator,
    activateSpecifiedNames,
    clearActiveLocators,
    getPossibleReferents,
    getMainDefiniteDescription,
    getNameLifter,
    getMainDefiniteDescriptionByTarget,
  )
where

import Context.App
import Context.App.Internal
import Context.Env (getCurrentSource)
import Context.Module (getMainModule)
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Control.Monad
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.LocalLocator qualified as LL
import Entity.Module qualified as Module
import Entity.ModuleID qualified as MID
import Entity.Source qualified as Source
import Entity.SourceLocator qualified as SL
import Entity.StrictGlobalLocator qualified as SGL
import Entity.Target qualified as Target
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
  currentSource <- readRef "currentSource" currentSource
  cgl <- constructGlobalLocator currentSource
  writeRef currentGlobalLocator cgl
  writeRef' activeGlobalLocatorList [cgl, SGL.llvmGlobalLocator]
  writeRef' activeDefiniteDescriptionList Map.empty

activateSpecifiedNames :: TopNameMap -> SGL.StrictGlobalLocator -> [(Hint, LL.LocalLocator)] -> App ()
activateSpecifiedNames topNameMap sgl lls = do
  forM_ lls $ \(m, ll) -> do
    let dd = DD.new sgl ll
    case Map.lookup dd topNameMap of
      Nothing ->
        Throw.raiseError m $ "the name `" <> LL.reify ll <> "` isn't defined in the module"
      Just (mDef, gn) -> do
        Tag.insertGlobalVar m dd (GN.getIsConstLike gn) mDef
        aenv <- readRef' activeDefiniteDescriptionList
        case Map.lookup ll aenv of
          Just existingDD
            | dd /= existingDD -> do
                current <- getCurrentSource
                let dd' = DD.getReadableDD (Source.sourceModule current) dd
                let existingDD' = DD.getReadableDD (Source.sourceModule current) existingDD
                Throw.raiseError m $
                  "this `"
                    <> LL.reify ll
                    <> "` is ambiguous since it could refer to:\n- "
                    <> dd'
                    <> "\n- "
                    <> existingDD'
          _ ->
            modifyRef' activeDefiniteDescriptionList $ Map.insert ll dd

attachCurrentLocator ::
  BN.BaseName ->
  App DD.DefiniteDescription
attachCurrentLocator name = do
  cgl <- getCurrentGlobalLocator
  return $ DD.new cgl $ LL.new name

getNameLifter ::
  App (BN.BaseName -> DD.DefiniteDescription)
getNameLifter = do
  cgl <- getCurrentGlobalLocator
  return $ \name -> DD.new cgl $ LL.new name

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

constructGlobalLocator :: Source.Source -> App SGL.StrictGlobalLocator
constructGlobalLocator source = do
  sourceLocator <- getSourceLocator source
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = Module.moduleID $ Source.sourceModule source,
        SGL.sourceLocator = sourceLocator
      }

getSourceLocator :: Source.Source -> App SL.SourceLocator
getSourceLocator source = do
  relFilePath <- stripProperPrefix (Module.getSourceDir $ Source.sourceModule source) $ Source.sourceFilePath source
  relFilePath' <- removeExtension relFilePath
  return $ SL.SourceLocator relFilePath'

removeExtension :: Path a File -> App (Path a File)
removeExtension path =
  case splitExtension path of
    Just (path', _) ->
      return path'
    Nothing ->
      Throw.raiseError' $ "file extension is missing in `" <> T.pack (toFilePath path) <> "`"

getMainDefiniteDescription ::
  Source.Source ->
  App (Maybe DD.DefiniteDescription)
getMainDefiniteDescription source = do
  b <- isMainFile source
  if b
    then Just <$> attachCurrentLocator BN.mainName
    else return Nothing

isMainFile :: Source.Source -> App Bool
isMainFile source = do
  case Module.moduleID $ Source.sourceModule source of
    MID.Main -> do
      let sourcePathList = Module.getTargetPathList $ Source.sourceModule source
      return $ elem (Source.sourceFilePath source) sourcePathList
    _ ->
      return False

getMainDefiniteDescriptionByTarget :: Target.Target -> App DD.DefiniteDescription
getMainDefiniteDescriptionByTarget targetOrZen = do
  mainModule <- getMainModule
  case targetOrZen of
    Target.Target target -> do
      case Map.lookup target (Module.moduleTarget mainModule) of
        Nothing ->
          Throw.raiseError' $ "no such target is defined: " <> target
        Just sourceLocator -> do
          relPathToDD (SL.reify sourceLocator) BN.mainName
    Target.ZenTarget path -> do
      relPath <- Module.getRelPathFromSourceDir mainModule path
      relPathToDD relPath BN.zenName

relPathToDD :: Path Rel File -> BN.BaseName -> App DD.DefiniteDescription
relPathToDD relPath baseName = do
  sourceLocator <- SL.SourceLocator <$> removeExtension relPath
  let sgl = SGL.StrictGlobalLocator {moduleID = MID.Main, sourceLocator = sourceLocator}
  let ll = LL.new baseName
  return $ DD.new sgl ll
