module Move.Context.WeakDefinition
  ( initialize,
    insert,
    read,
    lookup,
    DefMap,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Gensym qualified as Gensym
import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
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
  WeakTerm ->
  App ()
insert opacity m name impArgs expArgs codType e =
  when (opacity == O.Clear) $ do
    i <- Gensym.newCount
    modifyRef' weakDefMap $
      Map.insert name (m :< WT.PiIntro (AttrL.normal i codType) impArgs expArgs e)

read :: App DefMap
read =
  readRef' weakDefMap

lookup :: DD.DefiniteDescription -> App (Maybe WeakTerm)
lookup name = do
  denv <- readRef' weakDefMap
  return $ Map.lookup name denv
