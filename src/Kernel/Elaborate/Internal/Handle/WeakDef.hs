module Kernel.Elaborate.Internal.Handle.WeakDef
  ( Handle,
    new,
    DefMap,
    Entry,
    insertMany,
    makeEntry,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Gensym.Gensym qualified as GensymCount
import Gensym.Handle qualified as Gensym
import Kernel.Common.HashMapRef (insertEntries)
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Opacity qualified as O
import Language.WeakTerm.WeakTerm
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint
import Prelude hiding (lookup, read)

type DefMap =
  Map.HashMap DD.DefiniteDescription WeakTerm

newtype Handle = Handle
  { weakDefMapRef :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm)
  }

new :: IO Handle
new = do
  weakDefMapRef <- newIORef Map.empty
  return $ Handle {..}

data Entry = Entry
  { entryName :: DD.DefiniteDescription,
    entryBodyWithIdentity :: Int -> WeakTerm
  }

makeEntry ::
  O.Opacity ->
  Bool ->
  Hint ->
  DD.DefiniteDescription ->
  [BinderF WeakType] ->
  [BinderF WeakType] ->
  [(BinderF WeakType, WeakTerm)] ->
  WeakType ->
  WeakTerm ->
  [Entry]
makeEntry opacity isDestPassing m name impArgs expArgs defaultArgs codType e =
  [Entry name $ \i -> m :< WT.PiIntro (AttrL.normal' (Just $ DD.localLocator name) isDestPassing i codType) impArgs expArgs defaultArgs e | opacity == O.Clear]

insertMany :: Handle -> Gensym.Handle -> [Entry] -> IO ()
insertMany h gensymHandle entries = do
  definitions <- forM entries $ \(Entry {entryName, entryBodyWithIdentity}) -> do
    i <- GensymCount.newCount gensymHandle
    return (entryName, entryBodyWithIdentity i)
  insertEntries (weakDefMapRef h) definitions
