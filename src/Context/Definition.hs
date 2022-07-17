module Context.Definition
  ( Context (..),
    Config (..),
  )
where

import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.Hint
import Entity.Opacity
import Entity.WeakTerm
import Prelude hiding (lookup, read)

data Context = forall defMap.
  Context
  { insert :: Opacity -> Hint -> DD.DefiniteDescription -> [BinderF WeakTerm] -> WeakTerm -> IO (),
    read :: IO defMap,
    lookup :: DD.DefiniteDescription -> defMap -> Maybe WeakTerm
  }

data Config = Config
  {
  }
