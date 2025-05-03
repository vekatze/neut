module Main.Move.Scene.Elaborate.Handle.WeakDef
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
import Main.Move.Language.Utility.Gensym qualified as Gensym
import Main.Rule.Attr.Lam qualified as AttrL
import Main.Rule.Binder
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.Hint
import Main.Rule.Opacity qualified as O
import Main.Rule.WeakTerm
import Main.Rule.WeakTerm qualified as WT
import Prelude hiding (lookup, read)

type DefMap =
  Map.HashMap DD.DefiniteDescription WeakTerm

data Handle
  = Handle
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
