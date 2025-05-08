module Kernel.Parse.Move.Internal.Handle.Unused
  ( Handle,
    new,
    insertGlobalLocator,
    insertLocalLocator,
    insertStaticFile,
    insertVariable,
    deleteGlobalLocator,
    deleteLocalLocator,
    deleteStaticFile,
    deleteVariable,
    getGlobalLocator,
    getLocalLocator,
    getStaticFile,
    getVariable,
  )
where

import Aux.Logger.Rule.Hint
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Kernel.Parse.Rule.VarDefKind
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.UnusedGlobalLocators (UnusedGlobalLocators)
import Language.Common.Rule.UnusedLocalLocators
import Prelude hiding (lookup, read)

data Handle = Handle
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

getStaticFile :: Handle -> IO [(T.Text, Hint)]
getStaticFile h = do
  uenv <- readIORef (unusedStaticFileMapRef h)
  return $ Map.toList uenv

getVariable :: Handle -> IO [(Hint, Ident, VarDefKind)]
getVariable h = do
  vars <- readIORef (unusedVariableMapRef h)
  return $ filter (\(_, var, _) -> not (isHole var)) $ IntMap.elems vars
