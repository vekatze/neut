module Kernel.Common.Move.Handle.Local.Locator
  ( new,
    attachCurrentLocator,
    activateSpecifiedNames,
    getStaticFileContent,
    activateStaticFile,
    getPossibleReferents,
    getNameLifter,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Kernel.Common.Move.Handle.Local.Tag qualified as Tag
import Kernel.Common.Rule.AliasInfo (MustUpdateTag)
import Kernel.Common.Rule.GlobalName qualified as GN
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Handle.Local.Locator
import Kernel.Common.Rule.Handle.Local.Tag qualified as Tag
import Kernel.Common.Rule.Module qualified as Module
import Kernel.Common.Rule.Source qualified as Source
import Kernel.Common.Rule.TopNameMap (TopNameMap)
import Language.Common.Move.Raise (raiseError, raiseError')
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.SourceLocator qualified as SL
import Language.Common.Rule.StrictGlobalLocator qualified as SGL
import Logger.Rule.Hint
import Path
import Path.IO
import Path.Move.Read (readText)

new :: Env.Handle -> Tag.Handle -> Source.Source -> EIO Handle
new _envHandle _tagHandle source = do
  cgl <- constructGlobalLocator source
  _activeDefiniteDescriptionListRef <- liftIO $ newIORef Map.empty
  _activeStaticFileListRef <- liftIO $ newIORef Map.empty
  _activeGlobalLocatorListRef <- liftIO $ newIORef [cgl, SGL.llvmGlobalLocator]
  _currentGlobalLocatorRef <- liftIO $ newIORef (Just cgl)
  return $ Handle {..}

activateSpecifiedNames ::
  Handle ->
  Source.Source ->
  TopNameMap ->
  MustUpdateTag ->
  SGL.StrictGlobalLocator ->
  [(Hint, LL.LocalLocator)] ->
  EIO ()
activateSpecifiedNames h currentSource topNameMap mustUpdateTag sgl lls = do
  forM_ lls $ \(m, ll) -> do
    let dd = DD.new sgl ll
    case Map.lookup dd topNameMap of
      Nothing ->
        raiseError m $ "The name `" <> LL.reify ll <> "` is not defined in the module"
      Just (mDef, gn) -> do
        when mustUpdateTag $
          liftIO $
            Tag.insertGlobalVar (_tagHandle h) m dd (GN.getIsConstLike gn) mDef
        activeDefiniteDescriptionList <- liftIO $ readIORef (_activeDefiniteDescriptionListRef h)
        case Map.lookup ll activeDefiniteDescriptionList of
          Just existingDD
            | dd /= existingDD -> do
                let dd' = DD.getReadableDD' (Source.sourceModule currentSource) dd
                let existingDD' = DD.getReadableDD' (Source.sourceModule currentSource) existingDD
                raiseError m $
                  "This `"
                    <> LL.reify ll
                    <> "` is ambiguous since it could refer to:\n- "
                    <> dd'
                    <> "\n- "
                    <> existingDD'
          _ ->
            liftIO $ modifyIORef' (_activeDefiniteDescriptionListRef h) $ Map.insert ll dd

activateStaticFile :: Handle -> Hint -> T.Text -> Path Abs File -> EIO ()
activateStaticFile h m key path = do
  b <- doesFileExist path
  if b
    then do
      content <- liftIO $ readText path
      liftIO $ modifyIORef' (_activeStaticFileListRef h) $ Map.insert key (path, content)
    else
      raiseError m $
        "The static file `" <> key <> "` does not exist at: " <> T.pack (toFilePath path)

getStaticFileContent :: Handle -> T.Text -> IO (Maybe (Path Abs File, T.Text))
getStaticFileContent h key = do
  activeStaticFileList <- readIORef (_activeStaticFileListRef h)
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

getCurrentGlobalLocator :: Handle -> IO SGL.StrictGlobalLocator
getCurrentGlobalLocator h = do
  sglOrNone <- readIORef (_currentGlobalLocatorRef h)
  case sglOrNone of
    Just sgl ->
      return sgl
    Nothing ->
      error $ T.unpack "[compiler bug] `currentGlobalLocatorRef` is uninitialized"

getPossibleReferents :: Handle -> LL.LocalLocator -> IO [DD.DefiniteDescription]
getPossibleReferents h localLocator = do
  cgl <- getCurrentGlobalLocator h
  agls <- readIORef (_activeGlobalLocatorListRef h)
  importedDDs <- getImportedReferents h localLocator
  let dds = map (`DD.new` localLocator) agls
  let dd = DD.new cgl localLocator
  return $ ListUtils.nubOrd $ dd : dds ++ importedDDs

getImportedReferents :: Handle -> LL.LocalLocator -> IO [DD.DefiniteDescription]
getImportedReferents h ll = do
  activeDefiniteDescriptionList <- readIORef (_activeDefiniteDescriptionListRef h)
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
