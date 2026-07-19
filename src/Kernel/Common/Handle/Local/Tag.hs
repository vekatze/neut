module Kernel.Common.Handle.Local.Tag
  ( Handle (..),
    new,
    get,
    insertLocalVar,
    insertGlobalVar,
    insertBinder,
    insertLocator,
    insertNamespaceView,
    insertModuleFile,
    insertExternalName,
    insertStaticFile,
    insertResolvedSourceFile,
    retarget,
  )
where

import Control.Monad (unless, when)
import Data.IORef
import Data.Text qualified as T
import Kernel.Common.LocationTree qualified as LT
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Ident
import Language.Common.IsConstLike
import Logger.Hint
import Path (Abs, File, Path, toFilePath)
import Prelude hiding (lookup, read)

newtype Handle = Handle
  { _tagMapRef :: IORef LT.LocationTree
  }

new :: IO Handle
new = do
  _tagMapRef <- newIORef LT.empty
  return $ Handle {..}

get :: Handle -> IO LT.LocationTree
get h =
  readIORef $ _tagMapRef h

insertLocalVar :: Handle -> Hint -> Ident -> Hint -> IO ()
insertLocalVar h mUse ident@(I (var, varID)) mDef = do
  unless (isHole ident) $ do
    let nameLength = T.length var
    let symbolName = LT.Local varID nameLength (metaFileName mDef) (metaLocation mDef)
    insert h mUse symbolName nameLength mDef

insertBinder :: Handle -> BinderF a -> IO ()
insertBinder h (m, _, ident, _) =
  insertLocalVar h m ident m

insertGlobalVar :: Handle -> Hint -> DD.DefiniteDescription -> IsConstLike -> Hint -> IO ()
insertGlobalVar h mUse dd isConstLike mDef = do
  let nameLength = T.length (DD.baseNameText dd)
  insert h mUse (LT.Global dd isConstLike) nameLength mDef

insert :: Handle -> Hint -> LT.SymbolName -> Int -> Hint -> IO ()
insert h mUse symbolName nameLength mDef = do
  when (metaShouldSaveLocation mUse) $ do
    let (l, c) = metaLocation mUse
    modifyIORef' (_tagMapRef h) $ LT.insert symbolName (l, (c, c + nameLength)) mDef

insertLocator :: Handle -> Hint -> DD.DefiniteDescription -> IsConstLike -> Int -> Hint -> IO ()
insertLocator h mUse dd isConstLike nameLength mDef = do
  insert h mUse (LT.Global dd isConstLike) nameLength mDef

insertNamespaceView :: Handle -> Hint -> T.Text -> Hint -> Hint -> IO ()
insertNamespaceView h mUse importAliasText mImportAlias mDef = do
  let symbolName = LT.NamespaceView (metaFileName mImportAlias) (metaLocation mImportAlias)
  insert h mUse symbolName (T.length importAliasText) mDef

insertModuleFile :: Handle -> Hint -> T.Text -> Path Abs File -> IO ()
insertModuleFile h mUse modulePathText path = do
  let symbolName = LT.ModuleFile $ toFilePath path
  insert h mUse symbolName (T.length modulePathText) (newSourceHint path)

insertExternalName :: Handle -> Hint -> EN.ExternalName -> Hint -> IO ()
insertExternalName h mUse externalName mDef = do
  let nameLength = T.length $ EN.reify externalName
  insert h mUse (LT.Foreign externalName) nameLength mDef

insertStaticFile :: Handle -> Hint -> T.Text -> Hint -> IO ()
insertStaticFile h mUse key mDef = do
  let nameLength = T.length key
  insert h mUse (LT.StaticFile key) nameLength mDef

insertResolvedSourceFile :: Handle -> Hint -> T.Text -> T.Text -> Hint -> IO ()
insertResolvedSourceFile h mUse sourceText resolvedLocator mDef = do
  insert h mUse (LT.SourceFile resolvedLocator) (T.length sourceText) mDef

retarget :: Handle -> Hint -> Hint -> IO ()
retarget h mUse mDef = do
  when (metaShouldSaveLocation mUse) $ do
    let (l, c) = metaLocation mUse
    modifyIORef' (_tagMapRef h) $ LT.retarget (l, c) mDef
