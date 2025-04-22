module Move.Context.Tag
  ( initialize,
    insertLocalVar,
    insertGlobalVar,
    insertExternalName,
    insertLocator,
    insertFileLoc,
    insertBinder,
    get,
    insertFileLocIO,
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

get :: App LT.LocationTree
get = do
  readRef' tagMap

insertLocalVar :: Hint -> Ident -> Hint -> App ()
insertLocalVar mUse ident@(I (var, varID)) mDef = do
  unless (isHole ident) $ do
    let nameLength = T.length var
    let symbolLoc = LT.SymbolLoc (LT.Local varID nameLength)
    insert mUse symbolLoc nameLength mDef

insertGlobalVar :: Hint -> DD.DefiniteDescription -> IsConstLike -> Hint -> App ()
insertGlobalVar mUse dd isConstLike mDef = do
  let nameLength = T.length (DD.localLocator dd)
  let symbolLoc = LT.SymbolLoc (LT.Global dd isConstLike)
  insert mUse symbolLoc nameLength mDef

insertExternalName :: Hint -> EN.ExternalName -> Hint -> App ()
insertExternalName mUse externalName mDef = do
  let symbolLoc = LT.SymbolLoc (LT.Foreign externalName)
  let nameLength = T.length $ EN.reify externalName
  insert mUse symbolLoc nameLength mDef

insertLocator :: Hint -> DD.DefiniteDescription -> IsConstLike -> Int -> Hint -> App ()
insertLocator mUse dd isConstLike nameLength mDef = do
  insert mUse (LT.SymbolLoc (LT.Global dd isConstLike)) nameLength mDef

insert :: Hint -> LT.LocType -> Int -> Hint -> App ()
insert mUse locType nameLength mDef = do
  when (metaShouldSaveLocation mUse) $ do
    let (l, c) = metaLocation mUse
    modifyRef' tagMap $ LT.insert locType (l, (c, c + nameLength)) mDef

insertFileLoc :: Hint -> Int -> Hint -> App ()
insertFileLoc mUse nameLength mDef = do
  when (metaShouldSaveLocation mUse) $ do
    let (l, c) = metaLocation mUse
    modifyRef' tagMap $ LT.insert LT.FileLoc (l, (c, c + nameLength)) mDef

insertBinder :: BinderF a -> App ()
insertBinder (m, ident, _) =
  insertLocalVar m ident m

insertFileLocIO :: IORef LT.LocationTree -> Hint -> Int -> Hint -> IO ()
insertFileLocIO ref mUse nameLength mDef = do
  when (metaShouldSaveLocation mUse) $ do
    let (l, c) = metaLocation mUse
    modifyIORef' ref $ LT.insert LT.FileLoc (l, (c, c + nameLength)) mDef
