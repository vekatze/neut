module Context.WeakDefinition
  ( initialize,
    insert,
    read,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Context.Gensym qualified as Gensym
import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.Attr.Lam qualified as AttrL
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Opacity qualified as O
import Entity.WeakTerm
import Entity.WeakTerm qualified as WT
import Prelude hiding (lookup, read)

type DefMap =
  Map.HashMap DD.DefiniteDescription WeakTerm

initialize :: App ()
initialize = do
  writeRef' weakDefMap Map.empty

insert ::
  O.Opacity ->
  Hint ->
  DD.DefiniteDescription ->
  [BinderF WeakTerm] ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  App ()
insert opacity m name impArgs expArgs e =
  when (opacity == O.Clear) $ do
    i <- Gensym.newCount
    modifyRef' weakDefMap $
      Map.insert name (m :< WT.PiIntro (AttrL.normal i) impArgs expArgs e)

read :: App DefMap
read =
  readRef' weakDefMap

lookup :: DD.DefiniteDescription -> App (Maybe WeakTerm)
lookup name = do
  denv <- readRef' weakDefMap
  return $ Map.lookup name denv
