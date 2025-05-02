module Move.Context.Unused
  ( Handle,
    new,
    insertGlobalLocator,
    insertLocalLocator,
    insertPreset,
    insertStaticFile,
    insertVariable,
    deleteGlobalLocator,
    deleteLocalLocator,
    deletePreset,
    deleteStaticFile,
    deleteVariable,
    getGlobalLocator,
    getLocalLocator,
    getPreset,
    getStaticFile,
    getVariable,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify
import Rule.LocalLocator qualified as LL
import Rule.UnusedGlobalLocators (UnusedGlobalLocators)
import Rule.UnusedLocalLocators
import Rule.VarDefKind
import Prelude hiding (lookup, read)

type ModuleIDText =
  T.Text

data Handle
  = Handle
  { unusedGlobalLocatorMapRef :: IORef (Map.HashMap T.Text [(Hint, T.Text)]), -- (SGL ~> [(hint, locatorText)])
    unusedLocalLocatorMapRef :: IORef (Map.HashMap LL.LocalLocator Hint),
    unusedPresetMapRef :: IORef (Map.HashMap T.Text Hint),
    unusedStaticFileMapRef :: IORef (Map.HashMap T.Text Hint),
    unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind))
  }

new :: IO Handle
new = do
  unusedGlobalLocatorMapRef <- newIORef Map.empty
  unusedLocalLocatorMapRef <- newIORef Map.empty
  unusedPresetMapRef <- newIORef Map.empty
  unusedStaticFileMapRef <- newIORef Map.empty
  unusedVariableMapRef <- newIORef IntMap.empty
  return $ Handle {..}

insertGlobalLocator :: Handle -> T.Text -> Hint -> T.Text -> IO ()
insertGlobalLocator h sglText m locatorText =
  modifyIORef' (unusedGlobalLocatorMapRef h) $ Map.insertWith (++) sglText [(m, locatorText)]

insertLocalLocator :: Handle -> LL.LocalLocator -> Hint -> IO ()
insertLocalLocator h ll m =
  modifyIORef' (unusedLocalLocatorMapRef h) $ Map.insert ll m

insertPreset :: Handle -> ModuleIDText -> Hint -> IO ()
insertPreset h presetName m =
  modifyIORef' (unusedPresetMapRef h) $ Map.insert presetName m

insertStaticFile :: Handle -> T.Text -> Hint -> IO ()
insertStaticFile h ll m =
  modifyIORef' (unusedStaticFileMapRef h) $ Map.insert ll m

insertVariable :: Handle -> Hint -> Ident -> VarDefKind -> IO ()
insertVariable h m x k =
  modifyIORef' (unusedVariableMapRef h) $ IntMap.insert (toInt x) (m, x, k)

deleteGlobalLocator :: Handle -> T.Text -> IO ()
deleteGlobalLocator h sglText =
  modifyIORef' (unusedGlobalLocatorMapRef h) $ Map.delete sglText

deleteLocalLocator :: Handle -> LL.LocalLocator -> IO ()
deleteLocalLocator h ll =
  modifyIORef' (unusedLocalLocatorMapRef h) $ Map.delete ll

deletePreset :: Handle -> ModuleIDText -> IO ()
deletePreset h presetName =
  modifyIORef' (unusedPresetMapRef h) $ Map.delete presetName

deleteStaticFile :: Handle -> T.Text -> IO ()
deleteStaticFile h ll =
  modifyIORef' (unusedStaticFileMapRef h) $ Map.delete ll

deleteVariable :: Handle -> Ident -> IO ()
deleteVariable h x = do
  modifyIORef' (unusedVariableMapRef h) $ IntMap.delete (toInt x)

getGlobalLocator :: Handle -> IO UnusedGlobalLocators
getGlobalLocator h = do
  uenv <- readIORef (unusedGlobalLocatorMapRef h)
  return $ Map.toList uenv

getLocalLocator :: Handle -> IO UnusedLocalLocators
getLocalLocator h = do
  uenv <- readIORef (unusedLocalLocatorMapRef h)
  return $ Map.toList uenv

getPreset :: Handle -> IO (Map.HashMap T.Text Hint)
getPreset h =
  readIORef (unusedPresetMapRef h)

getStaticFile :: Handle -> IO [(T.Text, Hint)]
getStaticFile h = do
  uenv <- readIORef (unusedStaticFileMapRef h)
  return $ Map.toList uenv

getVariable :: Handle -> IO [(Hint, Ident, VarDefKind)]
getVariable h = do
  vars <- readIORef (unusedVariableMapRef h)
  return $ filter (\(_, var, _) -> not (isHole var)) $ IntMap.elems vars
