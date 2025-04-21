module Move.Context.Locator
  ( initialize,
    attachCurrentLocator,
    attachPublicCurrentLocator,
    getCurrentGlobalLocator,
    activateSpecifiedNames,
    getStaticFileContent,
    activateStaticFile,
    isMainFile,
    getPossibleReferents,
    getMainDefiniteDescription,
    getNameLifter,
    getMainDefiniteDescriptionByTarget,
    checkIfEntryPointIsNecessary,
    getReadableDD,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as B
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Env (getCurrentSource, getMainModule)
import Move.Context.Path (doesFileExist)
import Move.Context.Tag qualified as Tag
import Move.Context.Throw qualified as Throw
import Path
import Rule.AliasInfo (MustUpdateTag)
import Rule.BaseName qualified as BN
import Rule.DefiniteDescription qualified as DD
import Rule.GlobalName qualified as GN
import Rule.Hint
import Rule.LocalLocator qualified as LL
import Rule.Module (extractModule)
import Rule.Module qualified as Module
import Rule.ModuleID qualified as MID
import Rule.Source qualified as Source
import Rule.SourceLocator qualified as SL
import Rule.StrictGlobalLocator qualified as SGL
import Rule.Target qualified as Target
import Rule.TopNameMap (TopNameMap)

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
  writeRef' activeStaticFileList Map.empty

activateSpecifiedNames :: TopNameMap -> MustUpdateTag -> SGL.StrictGlobalLocator -> [(Hint, LL.LocalLocator)] -> App ()
activateSpecifiedNames topNameMap mustUpdateTag sgl lls = do
  forM_ lls $ \(m, ll) -> do
    let dd = DD.new sgl ll
    case Map.lookup dd topNameMap of
      Nothing ->
        Throw.raiseError m $ "The name `" <> LL.reify ll <> "` is not defined in the module"
      Just (mDef, gn) -> do
        when mustUpdateTag $
          Tag.insertGlobalVar m dd (GN.getIsConstLike gn) mDef
        aenv <- readRef' activeDefiniteDescriptionList
        case Map.lookup ll aenv of
          Just existingDD
            | dd /= existingDD -> do
                current <- getCurrentSource
                let dd' = DD.getReadableDD (Source.sourceModule current) dd
                let existingDD' = DD.getReadableDD (Source.sourceModule current) existingDD
                Throw.raiseError m $
                  "This `"
                    <> LL.reify ll
                    <> "` is ambiguous since it could refer to:\n- "
                    <> dd'
                    <> "\n- "
                    <> existingDD'
          _ ->
            modifyRef' activeDefiniteDescriptionList $ Map.insert ll dd

activateStaticFile :: Hint -> T.Text -> Path Abs File -> App ()
activateStaticFile m key path = do
  b <- doesFileExist path
  if b
    then do
      content <- liftIO $ fmap decodeUtf8 $ B.readFile $ toFilePath path
      modifyRef' activeStaticFileList $ Map.insert key (path, content)
    else
      Throw.raiseError m $
        "The static file `" <> key <> "` does not exist at: " <> T.pack (toFilePath path)

getStaticFileContent :: T.Text -> App (Maybe (Path Abs File, T.Text))
getStaticFileContent key = do
  env <- readRef' activeStaticFileList
  return $ Map.lookup key env

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
      Throw.raiseError' $ "File extension is missing in `" <> T.pack (toFilePath path) <> "`"

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

getMainDefiniteDescriptionByTarget :: Target.MainTarget -> App DD.DefiniteDescription
getMainDefiniteDescriptionByTarget targetOrZen = do
  mainModule <- getMainModule
  case targetOrZen of
    Target.Named target _ -> do
      case Map.lookup target (Module.moduleTarget $ extractModule mainModule) of
        Nothing ->
          Throw.raiseError' $ "No such target is defined: " <> target
        Just targetSummary -> do
          relPathToDD (SL.reify $ Target.entryPoint targetSummary) BN.mainName
    Target.Zen path _ -> do
      relPath <- Module.getRelPathFromSourceDir (extractModule mainModule) path
      relPathToDD relPath BN.zenName

relPathToDD :: Path Rel File -> BN.BaseName -> App DD.DefiniteDescription
relPathToDD relPath baseName = do
  sourceLocator <- SL.SourceLocator <$> removeExtension relPath
  let sgl = SGL.StrictGlobalLocator {moduleID = MID.Main, sourceLocator = sourceLocator}
  let ll = LL.new baseName
  return $ DD.new sgl ll

checkIfEntryPointIsNecessary :: Target.MainTarget -> Source.Source -> App Bool
checkIfEntryPointIsNecessary target source = do
  case target of
    Target.Named {} -> do
      isMainFile source
    Target.Zen path _ -> do
      return $ Source.sourceFilePath source == path

getReadableDD :: DD.DefiniteDescription -> App T.Text
getReadableDD dd = do
  mainModule <- getMainModule
  return $ DD.getReadableDD (extractModule mainModule) dd
