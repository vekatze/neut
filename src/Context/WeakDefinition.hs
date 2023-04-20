module Context.WeakDefinition
  ( insert,
    read,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.LamKind qualified as LK
import Entity.Opacity qualified as O
import Entity.WeakTerm
import Entity.WeakTerm qualified as WT
import Prelude hiding (lookup, read)

type DefMap =
  Map.HashMap DD.DefiniteDescription WeakTerm

insert :: O.Opacity -> Hint -> DD.DefiniteDescription -> [BinderF WeakTerm] -> WeakTerm -> App ()
insert opacity m name xts e =
  when (opacity == O.Transparent) $
    modifyRef' weakDefMap $
      Map.insert name (m :< WT.PiIntro (LK.Normal opacity) xts e)

read :: App DefMap
read =
  readRef' weakDefMap

lookup :: DD.DefiniteDescription -> App (Maybe WeakTerm)
lookup name = do
  denv <- readRef' weakDefMap
  return $ Map.lookup name denv