module Move.Context.Locator
  ( Handle,
    new,
    initialize,
    attachCurrentLocator,
    attachPublicCurrentLocator,
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
import Control.Monad.Reader (asks)
import Data.ByteString qualified as B
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Data.Text.Encoding
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseError, raiseError')
import Move.Context.Env qualified as Env
import Move.Context.Tag qualified as Tag
import Path
import Path.IO
import Rule.AliasInfo (MustUpdateTag)
import Rule.BaseName qualified as BN
import Rule.DefiniteDescription qualified as DD
import Rule.GlobalName qualified as GN
import Rule.Hint
import Rule.LocalLocator qualified as LL
import Rule.Module (MainModule, extractModule)
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

data Handle
  = Handle
  { tagHandle :: Tag.Handle,
    envHandle :: Env.Handle,
    activeDefiniteDescriptionListRef :: IORef (Map.HashMap LL.LocalLocator DD.DefiniteDescription),
    activeStaticFileListRef :: IORef (Map.HashMap T.Text (Path Abs File, T.Text)),
    activeGlobalLocatorListRef :: IORef [SGL.StrictGlobalLocator],
    currentGlobalLocator :: IORef (Maybe SGL.StrictGlobalLocator)
  }

new :: Env.Handle -> App Handle
new envHandle = do
  tagHandle <- Tag.new
  activeDefiniteDescriptionListRef <- asks App.activeDefiniteDescriptionList
  activeStaticFileListRef <- asks App.activeStaticFileList
  activeGlobalLocatorListRef <- asks App.activeGlobalLocatorList
  currentGlobalLocator <- asks App.currentGlobalLocator
  return $ Handle {..}

initialize :: Handle -> EIO ()
initialize h = do
  currentSource <- Env.getCurrentSource (envHandle h)
  cgl <- constructGlobalLocator currentSource
  liftIO $ writeIORef (currentGlobalLocator h) (Just cgl)
  liftIO $ writeIORef (activeGlobalLocatorListRef h) [cgl, SGL.llvmGlobalLocator]
  liftIO $ writeIORef (activeDefiniteDescriptionListRef h) Map.empty
  liftIO $ writeIORef (activeStaticFileListRef h) Map.empty

activateSpecifiedNames ::
  Handle ->
  TopNameMap ->
  MustUpdateTag ->
  SGL.StrictGlobalLocator ->
  [(Hint, LL.LocalLocator)] ->
  EIO ()
activateSpecifiedNames h topNameMap mustUpdateTag sgl lls = do
  forM_ lls $ \(m, ll) -> do
    let dd = DD.new sgl ll
    case Map.lookup dd topNameMap of
      Nothing ->
        raiseError m $ "The name `" <> LL.reify ll <> "` is not defined in the module"
      Just (mDef, gn) -> do
        when mustUpdateTag $
          liftIO $
            Tag.insertGlobalVar (tagHandle h) m dd (GN.getIsConstLike gn) mDef
        activeDefiniteDescriptionList <- liftIO $ readIORef (activeDefiniteDescriptionListRef h)
        case Map.lookup ll activeDefiniteDescriptionList of
          Just existingDD
            | dd /= existingDD -> do
                current <- Env.getCurrentSource (envHandle h)
                let dd' = DD.getReadableDD (Source.sourceModule current) dd
                let existingDD' = DD.getReadableDD (Source.sourceModule current) existingDD
                raiseError m $
                  "This `"
                    <> LL.reify ll
                    <> "` is ambiguous since it could refer to:\n- "
                    <> dd'
                    <> "\n- "
                    <> existingDD'
          _ ->
            liftIO $ modifyIORef' (activeDefiniteDescriptionListRef h) $ Map.insert ll dd

activateStaticFile :: Handle -> Hint -> T.Text -> Path Abs File -> EIO ()
activateStaticFile h m key path = do
  b <- doesFileExist path
  if b
    then do
      content <- liftIO $ fmap decodeUtf8 $ B.readFile $ toFilePath path
      liftIO $ modifyIORef' (activeStaticFileListRef h) $ Map.insert key (path, content)
    else
      raiseError m $
        "The static file `" <> key <> "` does not exist at: " <> T.pack (toFilePath path)

getStaticFileContent :: Handle -> T.Text -> IO (Maybe (Path Abs File, T.Text))
getStaticFileContent h key = do
  activeStaticFileList <- readIORef (activeStaticFileListRef h)
  return $ Map.lookup key activeStaticFileList

attachCurrentLocator ::
  Handle ->
  BN.BaseName ->
  IO DD.DefiniteDescription
attachCurrentLocator h name = do
  cgl <- getCurrentGlobalLocator h
  return $ DD.new cgl $ LL.new name

getNameLifter ::
  Handle ->
  IO (BN.BaseName -> DD.DefiniteDescription)
getNameLifter h = do
  cgl <- getCurrentGlobalLocator h
  return $ \name -> DD.new cgl $ LL.new name

attachPublicCurrentLocator ::
  Handle ->
  BN.BaseName ->
  IO DD.DefiniteDescription
attachPublicCurrentLocator h name = do
  cgl <- getCurrentGlobalLocator h
  return $ DD.new cgl $ LL.new name

getCurrentGlobalLocator :: Handle -> IO SGL.StrictGlobalLocator
getCurrentGlobalLocator h = do
  sglOrNone <- readIORef (currentGlobalLocator h)
  case sglOrNone of
    Just sgl ->
      return sgl
    Nothing ->
      error $ T.unpack "[compiler bug] `currentGlobalLocator` is uninitialized"

getPossibleReferents :: Handle -> LL.LocalLocator -> IO [DD.DefiniteDescription]
getPossibleReferents h localLocator = do
  cgl <- getCurrentGlobalLocator h
  agls <- readIORef (activeGlobalLocatorListRef h)
  importedDDs <- getImportedReferents h localLocator
  let dds = map (`DD.new` localLocator) agls
  let dd = DD.new cgl localLocator
  return $ ListUtils.nubOrd $ dd : dds ++ importedDDs

getImportedReferents :: Handle -> LL.LocalLocator -> IO [DD.DefiniteDescription]
getImportedReferents h ll = do
  activeDefiniteDescriptionList <- readIORef (activeDefiniteDescriptionListRef h)
  return $ maybeToList $ Map.lookup ll activeDefiniteDescriptionList

constructGlobalLocator :: Source.Source -> EIO SGL.StrictGlobalLocator
constructGlobalLocator source = do
  sourceLocator <- getSourceLocator source
  return $
    SGL.StrictGlobalLocator
      { SGL.moduleID = Module.moduleID $ Source.sourceModule source,
        SGL.sourceLocator = sourceLocator
      }

getSourceLocator :: Source.Source -> EIO SL.SourceLocator
getSourceLocator source = do
  relFilePath <- stripProperPrefix (Module.getSourceDir $ Source.sourceModule source) $ Source.sourceFilePath source
  relFilePath' <- removeExtension relFilePath
  return $ SL.SourceLocator relFilePath'

removeExtension :: Path a File -> EIO (Path a File)
removeExtension path =
  case splitExtension path of
    Just (path', _) ->
      return path'
    Nothing ->
      raiseError' $ "File extension is missing in `" <> T.pack (toFilePath path) <> "`"

getMainDefiniteDescription ::
  Handle ->
  Source.Source ->
  IO (Maybe DD.DefiniteDescription)
getMainDefiniteDescription h source = do
  if isMainFile source
    then Just <$> attachCurrentLocator h BN.mainName
    else return Nothing

isMainFile :: Source.Source -> Bool
isMainFile source = do
  case Module.moduleID $ Source.sourceModule source of
    MID.Main -> do
      let sourcePathList = Module.getTargetPathList $ Source.sourceModule source
      Source.sourceFilePath source `elem` sourcePathList
    _ ->
      False

getMainDefiniteDescriptionByTarget :: Handle -> Target.MainTarget -> EIO DD.DefiniteDescription
getMainDefiniteDescriptionByTarget h targetOrZen = do
  mainModule <- Env.getMainModule (envHandle h)
  case targetOrZen of
    Target.Named target _ -> do
      case Map.lookup target (Module.moduleTarget $ extractModule mainModule) of
        Nothing ->
          raiseError' $ "No such target is defined: " <> target
        Just targetSummary -> do
          relPathToDD (SL.reify $ Target.entryPoint targetSummary) BN.mainName
    Target.Zen path _ -> do
      relPath <- Module.getRelPathFromSourceDir (extractModule mainModule) path
      relPathToDD relPath BN.zenName

relPathToDD :: Path Rel File -> BN.BaseName -> EIO DD.DefiniteDescription
relPathToDD relPath baseName = do
  sourceLocator <- SL.SourceLocator <$> removeExtension relPath
  let sgl = SGL.StrictGlobalLocator {moduleID = MID.Main, sourceLocator = sourceLocator}
  let ll = LL.new baseName
  return $ DD.new sgl ll

checkIfEntryPointIsNecessary :: Target.MainTarget -> Source.Source -> Bool
checkIfEntryPointIsNecessary target source = do
  case target of
    Target.Named {} -> do
      isMainFile source
    Target.Zen path _ -> do
      Source.sourceFilePath source == path

getReadableDD :: MainModule -> DD.DefiniteDescription -> T.Text
getReadableDD mainModule dd = do
  DD.getReadableDD (extractModule mainModule) dd
