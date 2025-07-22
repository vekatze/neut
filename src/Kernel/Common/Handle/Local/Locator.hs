module Kernel.Common.Handle.Local.Locator
  ( Handle (..),
    new,
    attachCurrentLocator,
    activateSpecifiedNames,
    getStaticFileContent,
    activateStaticFile,
    getPossibleReferents,
  )
where

import App.App (App)
import App.Run (raiseError, raiseError')
import Control.Monad
import Control.Monad.IO.Class
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Kernel.Common.AliasInfo (MustUpdateTag)
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Module qualified as Module
import Kernel.Common.ReadableDD
import Kernel.Common.Source qualified as Source
import Kernel.Common.TopNameMap (TopNameMap)
import Language.Common.BaseName qualified as BN
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.LocalLocator qualified as LL
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
import Logger.Hint
import Path
import Path.IO
import Path.Read (readTextFromPath)

-- the structure of a name of a global variable:
--
--     some.path.to.item.some-function
--     ----------------- -------------
--     ↑ global locator  ↑ local locator
--     ------------------------------------------------
--     ↑ the definite description of a global variable `some-function` (up-to module alias)

data Handle = Handle
  { _tagHandle :: Tag.Handle,
    _envHandle :: Env.Handle,
    _activeDefiniteDescriptionListRef :: IORef (Map.HashMap LL.LocalLocator DD.DefiniteDescription),
    _activeStaticFileListRef :: IORef (Map.HashMap T.Text (Path Abs File, T.Text)),
    _activeGlobalLocatorList :: [SGL.StrictGlobalLocator],
    _currentGlobalLocator :: SGL.StrictGlobalLocator
  }

new :: Env.Handle -> Tag.Handle -> Source.Source -> App Handle
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
  App ()
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

activateStaticFile :: Handle -> Hint -> T.Text -> Path Abs File -> App ()
activateStaticFile h m key path = do
  b <- doesFileExist path
  if b
    then do
      content <- readTextFromPath path
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
      raiseError' $ "File extension is missing in `" <> T.pack (toFilePath path) <> "`"
