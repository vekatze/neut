module Move.Context.WeakDefinition
  ( Handle,
    new,
    initialize,
    DefMap,
    insert',
    read',
    lookup',
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Language.Utility.Gensym qualified as GensymNew
import Rule.Attr.Lam qualified as AttrL
import Rule.Binder
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Opacity qualified as O
import Rule.WeakTerm
import Rule.WeakTerm qualified as WT
import Prelude hiding (lookup, read)

type DefMap =
  Map.HashMap DD.DefiniteDescription WeakTerm

data Handle
  = Handle
  { gensymHandle :: GensymNew.Handle,
    weakDefMapRef :: IORef (Map.HashMap DD.DefiniteDescription WT.WeakTerm)
  }

initialize :: Handle -> IO ()
initialize h = do
  writeIORef (weakDefMapRef h) Map.empty

new :: App Handle
new = do
  gensymHandle <- GensymNew.new
  weakDefMapRef <- asks App.weakDefMap
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
    i <- GensymNew.newCount (gensymHandle h)
    modifyIORef' (weakDefMapRef h) $
      Map.insert name (m :< WT.PiIntro (AttrL.normal i codType) impArgs expArgs e)

read' :: Handle -> IO DefMap
read' h =
  readIORef (weakDefMapRef h)

lookup' :: Handle -> DD.DefiniteDescription -> IO (Maybe WeakTerm)
lookup' h name = do
  weakDefMap <- readIORef (weakDefMapRef h)
  return $ Map.lookup name weakDefMap
