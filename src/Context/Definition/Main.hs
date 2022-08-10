module Context.Definition.Main (new) where

import qualified Context.Definition as Definition
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Hint as H
import Entity.LamKind (LamKindF (LamKindNormal))
import Entity.Opacity
import Entity.WeakTerm
import Prelude hiding (lookup, read)

-- type DefMap =
--   Map.HashMap DD.DefiniteDescription WeakTerm

new :: Definition.Config -> IO Definition.Context
new _ = do
  defMapRef <- newIORef Map.empty
  return $
    Definition.Context
      { Definition.insert =
          insert defMapRef,
        Definition.read =
          read defMapRef,
        Definition.lookup =
          Map.lookup
      }

read :: IORef DefMap -> IO DefMap
read = readIORef

insert ::
  IORef DefMap ->
  Opacity ->
  H.Hint ->
  DD.DefiniteDescription ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ()
insert defMapRef opacity m name xts e = do
  when (opacity == OpacityTransparent) $
    modifyIORef' defMapRef $ Map.insert name (m :< WeakTermPiIntro LamKindNormal xts e)
