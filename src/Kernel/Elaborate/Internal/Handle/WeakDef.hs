module Kernel.Elaborate.Internal.Handle.WeakDef
  ( Handle,
    new,
    DefMap,
    insert',
    read',
    lookup',
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
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

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    weakDefMapRef :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm)
  }

new :: Gensym.Handle -> IO Handle
new gensymHandle = do
  weakDefMapRef <- newIORef Map.empty
  return $ Handle {..}

insert' ::
  Handle ->
  O.Opacity ->
  Hint ->
  DD.DefiniteDescription ->
  [BinderF WeakTerm] ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  WeakTerm ->
  IO ()
insert' h opacity m name impArgs expArgs codType e =
  when (opacity == O.Clear) $ do
    i <- Gensym.newCount (gensymHandle h)
    modifyIORef' (weakDefMapRef h) $
      Map.insert name (m :< WT.PiIntro (AttrL.normal' (Just $ DD.localLocator name) i codType) (map (,Nothing) impArgs) expArgs e)

read' :: Handle -> IO DefMap
read' h =
  readIORef (weakDefMapRef h)

lookup' :: Handle -> DD.DefiniteDescription -> IO (Maybe WeakTerm)
lookup' h name = do
  weakDefMap <- readIORef (weakDefMapRef h)
  return $ Map.lookup name weakDefMap
