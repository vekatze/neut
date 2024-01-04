module Context.Tag
  ( initialize,
    insert,
    insertLocalVar,
    insertGlobalVar,
    insertFileLoc,
    insertBinder,
    get,
  )
where

import Context.App
import Context.App.Internal
import Control.Monad (unless, when)
import Data.Text qualified as T
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident
import Entity.LocationTree qualified as LT
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
    let symbolLoc = LT.SymbolLoc (LT.Local varID)
    insert mUse symbolLoc nameLength mDef

insertGlobalVar :: Hint -> DD.DefiniteDescription -> Hint -> App ()
insertGlobalVar mUse dd mDef = do
  let nameLength = T.length (DD.localLocator dd)
  let symbolLoc = LT.SymbolLoc (LT.Global dd)
  insert mUse symbolLoc nameLength mDef

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
