module Context.Definition
  ( Context (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Opacity
import Entity.WeakTerm
import Prelude hiding (lookup, read)

type DefMap =
  Map.HashMap DD.DefiniteDescription WeakTerm

class Monad m => Context m where
  insert :: Opacity -> Hint -> DD.DefiniteDescription -> [BinderF WeakTerm] -> WeakTerm -> m ()
  read :: m DefMap
  lookup :: m (DD.DefiniteDescription -> DefMap -> Maybe WeakTerm)
