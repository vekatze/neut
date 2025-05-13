module Kernel.Common.Move.Handle.Local.Locator
  ( new,
    attachCurrentLocator,
    activateSpecifiedNames,
    getStaticFileContent,
    activateStaticFile,
    getPossibleReferents,
  )
where

import Error.Move.Run (raiseError, raiseError')
import Error.Rule.EIO (EIO)
import Logger.Rule.Hint
import Path.Move.Read (readText)
import Control.Monad
import Control.Monad.IO.Class
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Kernel.Common.Move.Handle.Local.Tag qualified as Tag
import Kernel.Common.Rule.AliasInfo (MustUpdateTag)
import Kernel.Common.Rule.GlobalName qualified as GN
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Handle.Local.Locator
import Kernel.Common.Rule.Handle.Local.Tag qualified as Tag
import Kernel.Common.Rule.Module qualified as Module
import Kernel.Common.Rule.ReadableDD
import Kernel.Common.Rule.Source qualified as Source
import Kernel.Common.Rule.TopNameMap (TopNameMap)
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.SourceLocator qualified as SL
import Language.Common.Rule.StrictGlobalLocator qualified as SGL
import Path
import Path.IO

new :: Env.Handle -> Tag.Handle -> Source.Source -> EIO Handle
new _envHandle _tagHandle source = do
  cgl <- constructGlobalLocator source
  _activeDefiniteDescriptionListRef <- liftIO $ newIORef Map.empty
  _activeStaticFileListRef <- liftIO $ newIORef Map.empty
  let _activeGlobalLocatorList = [cgl, SGL.llvmGlobalLocator]
  let _currentGlobalLocator = cgl
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
                let dd' = readableDD' (Source.sourceModule currentSource) dd
                let existingDD' = readableDD' (Source.sourceModule currentSource) existingDD
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
  DD.DefiniteDescription
attachCurrentLocator h name = do
  let cgl = getCurrentGlobalLocator h
  DD.new cgl $ LL.new name

getCurrentGlobalLocator :: Handle -> SGL.StrictGlobalLocator
getCurrentGlobalLocator h = do
  _currentGlobalLocator h

getPossibleReferents :: Handle -> LL.LocalLocator -> IO [DD.DefiniteDescription]
getPossibleReferents h localLocator = do
  let cgl = getCurrentGlobalLocator h
  let agls = _activeGlobalLocatorList h
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
