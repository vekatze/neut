module Kernel.Elaborate.Move.Internal.Handle.WeakDef
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
import Gensym.Move.Gensym qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.Binder
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Opacity qualified as O
import Language.WeakTerm.Rule.WeakTerm
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Logger.Rule.Hint
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
      Map.insert name (m :< WT.PiIntro (AttrL.normal i codType) impArgs expArgs e)

read' :: Handle -> IO DefMap
read' h =
  readIORef (weakDefMapRef h)

lookup' :: Handle -> DD.DefiniteDescription -> IO (Maybe WeakTerm)
lookup' h name = do
  weakDefMap <- readIORef (weakDefMapRef h)
  return $ Map.lookup name weakDefMap
