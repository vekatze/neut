module Move.Context.Tag
  ( initialize,
    insertFileLocIO,
    insertLocalVarIO,
    insertGlobalVarIO,
    insertBinderIO,
    insertLocatorIO,
    insertExternalNameIO,
  )
where

import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef')
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal
import Rule.Binder
import Rule.DefiniteDescription qualified as DD
import Rule.ExternalName qualified as EN
import Rule.Hint
import Rule.Ident
import Rule.IsConstLike
import Rule.LocationTree qualified as LT
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' tagMap LT.empty

insertFileLocIO :: IORef LT.LocationTree -> Hint -> Int -> Hint -> IO ()
insertFileLocIO ref mUse nameLength mDef = do
  when (metaShouldSaveLocation mUse) $ do
    let (l, c) = metaLocation mUse
    modifyIORef' ref $ LT.insert LT.FileLoc (l, (c, c + nameLength)) mDef

insertLocalVarIO :: IORef LT.LocationTree -> Hint -> Ident -> Hint -> IO ()
insertLocalVarIO ref mUse ident@(I (var, varID)) mDef = do
  unless (isHole ident) $ do
    let nameLength = T.length var
    let symbolLoc = LT.SymbolLoc (LT.Local varID nameLength)
    insertIO ref mUse symbolLoc nameLength mDef

insertBinderIO :: IORef LT.LocationTree -> BinderF a -> IO ()
insertBinderIO h (m, ident, _) =
  insertLocalVarIO h m ident m

insertGlobalVarIO :: IORef LT.LocationTree -> Hint -> DD.DefiniteDescription -> IsConstLike -> Hint -> IO ()
insertGlobalVarIO ref mUse dd isConstLike mDef = do
  let nameLength = T.length (DD.localLocator dd)
  let symbolLoc = LT.SymbolLoc (LT.Global dd isConstLike)
  insertIO ref mUse symbolLoc nameLength mDef

insertIO :: IORef LT.LocationTree -> Hint -> LT.LocType -> Int -> Hint -> IO ()
insertIO ref mUse locType nameLength mDef = do
  when (metaShouldSaveLocation mUse) $ do
    let (l, c) = metaLocation mUse
    modifyIORef' ref $ LT.insert locType (l, (c, c + nameLength)) mDef

insertLocatorIO :: IORef LT.LocationTree -> Hint -> DD.DefiniteDescription -> IsConstLike -> Int -> Hint -> IO ()
insertLocatorIO ref mUse dd isConstLike nameLength mDef = do
  insertIO ref mUse (LT.SymbolLoc (LT.Global dd isConstLike)) nameLength mDef

insertExternalNameIO :: IORef LT.LocationTree -> Hint -> EN.ExternalName -> Hint -> IO ()
insertExternalNameIO ref mUse externalName mDef = do
  let symbolLoc = LT.SymbolLoc (LT.Foreign externalName)
  let nameLength = T.length $ EN.reify externalName
  insertIO ref mUse symbolLoc nameLength mDef
