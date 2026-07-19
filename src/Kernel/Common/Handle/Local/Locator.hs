module Kernel.Common.Handle.Local.Locator
  ( Handle (..),
    ImportBinding (..),
    new,
    attachCurrentLocatorWithin,
    activateImportedEntries,
    getStaticFileContent,
    activateStaticFile,
    lookupImportBinding,
    getCurrentGlobalLocator,
    constructGlobalLocator,
  )
where

import App.App (App)
import App.Run (raiseError, raiseError')
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.ModulePath (renderDD)
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Import (MustUpdateTag)
import Kernel.Common.Import qualified as I
import Kernel.Common.Module qualified as Module
import Kernel.Common.Source qualified as Source
import Kernel.Common.TopNameMap (TopNameMap)
import Language.Common.Availability qualified as AV
import Language.Common.BaseName qualified as BN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.LocalLocator qualified as LL
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint
import Path
import Path.IO

-- the structure of a name of a global variable:
--
--     module.path::some.path.to.item::some-function
--     ------------------------  -------------
--     ↑ global locator          ↑ local locator
--     ----------------------------------------
--     ↑ the definite description of a global variable `some-function` (up-to module alias)

data Handle = Handle
  { _tagHandle :: Tag.Handle,
    _modulePathMap :: ModulePath.ModulePathMap,
    _importEnvRef :: IORef (Map.HashMap BN.BaseName ImportBinding),
    _activeStaticFileMapRef :: IORef (Map.HashMap T.Text (Path Abs File, BS.ByteString)),
    _currentGlobalLocator :: SGL.StrictGlobalLocator
  }

data ImportBinding
  = ImportedName DD.DefiniteDescription (Maybe Hint)
  | NamespaceView SGL.StrictGlobalLocator Hint

new :: Tag.Handle -> ModulePath.ModulePathMap -> Source.Source -> App Handle
new _tagHandle _modulePathMap source = do
  cgl <- constructGlobalLocator source
  _importEnvRef <- liftIO $ newIORef Map.empty
  _activeStaticFileMapRef <- liftIO $ newIORef Map.empty
  let _currentGlobalLocator = cgl
  return $ Handle {..}

activateImportedEntries ::
  Handle ->
  Source.Source ->
  TopNameMap ->
  MustUpdateTag ->
  SGL.StrictGlobalLocator ->
  [I.ImportedEntry] ->
  App ()
activateImportedEntries h currentSource topNameMap mustUpdateTag sgl entries = do
  currentGlobalLocator <- constructGlobalLocator currentSource
  forM_ entries $ \entry ->
    case entry of
      I.ImportedName mImportedName ll explicitAliasOrNone ->
        activateImportedName h currentGlobalLocator topNameMap mustUpdateTag sgl mImportedName ll explicitAliasOrNone
      I.NamespaceView mImportAlias importAlias ->
        activateNamespaceView h currentSource mustUpdateTag mImportAlias importAlias sgl

activateImportedName ::
  Handle ->
  SGL.StrictGlobalLocator ->
  TopNameMap ->
  MustUpdateTag ->
  SGL.StrictGlobalLocator ->
  Hint ->
  LL.LocalLocator ->
  Maybe (Hint, BN.BaseName) ->
  App ()
activateImportedName h currentGlobalLocator topNameMap mustUpdateTag sgl mImportedName ll explicitAliasOrNone = do
  let dd = DD.new sgl ll
  let mImportAlias = maybe mImportedName fst explicitAliasOrNone
  let importAlias = maybe (LL.baseName ll) snd explicitAliasOrNone
  case Map.lookup dd topNameMap of
    Nothing ->
      raiseError mImportAlias $ "The name `" <> LL.reify ll <> "` is not defined in the file"
    Just _
      | not (AV.allows currentGlobalLocator [] dd) ->
          raiseError mImportAlias $ "The name `" <> LL.reify ll <> "` is not visible from this source"
    Just (mDef, _tag, gn) -> do
      when mustUpdateTag $ do
        let len = T.length $ BN.reify importAlias
        case (gn, explicitAliasOrNone) of
          (GN.Namespace, Just _) -> do
            liftIO $ Tag.insertNamespaceView (_tagHandle h) mImportAlias (BN.reify importAlias) mImportAlias mImportedName
            let importedLen = T.length $ LL.reify ll
            liftIO $ Tag.insertLocator (_tagHandle h) mImportedName dd (GN.getIsConstLike gn) importedLen mDef
          _ -> do
            liftIO $ Tag.insertLocator (_tagHandle h) mImportAlias dd (GN.getIsConstLike gn) len mImportedName
            let importedLen = T.length $ LL.reify ll
            liftIO $ Tag.insertLocator (_tagHandle h) mImportedName dd (GN.getIsConstLike gn) importedLen mDef
      importEnv <- liftIO $ readIORef (_importEnvRef h)
      case Map.lookup importAlias importEnv of
        Just (ImportedName existingDD _)
          | dd /= existingDD -> do
              let dd' = renderDD (_modulePathMap h) dd
              let existingDD' = renderDD (_modulePathMap h) existingDD
              raiseError mImportAlias $
                "The import alias `"
                  <> BN.reify importAlias
                  <> "` is ambiguous since it could refer to:\n- "
                  <> dd'
                  <> "\n- "
                  <> existingDD'
        Just (NamespaceView _ _) ->
          raiseError mImportAlias $
            "The import alias `" <> BN.reify importAlias <> "` is already used for a namespace view"
        _ ->
          liftIO $ modifyIORef' (_importEnvRef h) $ Map.insert importAlias (ImportedName dd (fst <$> explicitAliasOrNone))

-- registers a namespace view under an import alias chosen by the importing file
activateNamespaceView :: Handle -> Source.Source -> MustUpdateTag -> Hint -> BN.BaseName -> SGL.StrictGlobalLocator -> App ()
activateNamespaceView h currentSource mustUpdateTag mImportAlias importAlias sgl = do
  when mustUpdateTag $
    liftIO $
      Tag.insertNamespaceView
        (_tagHandle h)
        mImportAlias
        (BN.reify importAlias)
        mImportAlias
        (newSourceHint $ Source.sourceFilePath currentSource)
  importEnv <- liftIO $ readIORef (_importEnvRef h)
  case Map.lookup importAlias importEnv of
    Just (NamespaceView existingSGL _)
      | existingSGL /= sgl ->
          raiseError mImportAlias $
            "The import alias `" <> BN.reify importAlias <> "` is already used for a different source"
    Just (ImportedName _ _) ->
      raiseError mImportAlias $
        "The import alias `" <> BN.reify importAlias <> "` is already used for an imported name"
    _ ->
      liftIO $ modifyIORef' (_importEnvRef h) $ Map.insert importAlias (NamespaceView sgl mImportAlias)

activateStaticFile :: Handle -> Hint -> T.Text -> Path Abs File -> App ()
activateStaticFile h m key path = do
  b <- doesFileExist path
  if b
    then do
      content <- liftIO $ BS.readFile $ toFilePath path
      liftIO $ modifyIORef' (_activeStaticFileMapRef h) $ Map.insert key (path, content)
    else
      raiseError m $
        "The static file `" <> key <> "` does not exist at: " <> T.pack (toFilePath path)

getStaticFileContent :: Handle -> T.Text -> IO (Maybe (Path Abs File, BS.ByteString))
getStaticFileContent h key = do
  activeStaticFileList <- readIORef (_activeStaticFileMapRef h)
  return $ Map.lookup key activeStaticFileList

attachCurrentLocatorWithin ::
  Handle ->
  [BN.BaseName] ->
  BN.BaseName ->
  DD.DefiniteDescription
attachCurrentLocatorWithin h nsPath name =
  DD.new (getCurrentGlobalLocator h) $ LL.prepend nsPath $ LL.new name

getCurrentGlobalLocator :: Handle -> SGL.StrictGlobalLocator
getCurrentGlobalLocator h = do
  _currentGlobalLocator h

lookupImportBinding :: Handle -> BN.BaseName -> IO (Maybe ImportBinding)
lookupImportBinding h name = do
  importEnv <- readIORef (_importEnvRef h)
  return $ Map.lookup name importEnv

constructGlobalLocator :: Source.Source -> App SGL.StrictGlobalLocator
constructGlobalLocator source = do
  sourceLocator <- getSourceLocator source
  return $ SGL.new (Module.moduleID $ Source.sourceModule source) sourceLocator

getSourceLocator :: Source.Source -> App SL.SourceLocator
getSourceLocator source = do
  relFilePath <- stripProperPrefix (Module.getSourceDir $ Source.sourceModule source) $ Source.sourceFilePath source
  relFilePath' <- removeExtension relFilePath
  case SL.fromPath relFilePath' of
    Just sourceLocator ->
      return sourceLocator
    Nothing ->
      raiseError' $ "The source path `" <> T.pack (toFilePath relFilePath) <> "` contains the reserved segment `this`"

removeExtension :: Path a File -> App (Path a File)
removeExtension path =
  case splitExtension path of
    Just (path', _) ->
      return path'
    Nothing ->
      raiseError' $ "File extension is missing in `" <> T.pack (toFilePath path) <> "`"
