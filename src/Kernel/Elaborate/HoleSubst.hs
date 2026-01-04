module Kernel.Elaborate.HoleSubst
  ( HoleSubst (..),
  )
where

import Data.IntMap qualified as IntMap
import Language.Common.Ident
import Language.WeakTerm.WeakTerm

newtype HoleSubst = HoleSubst (IntMap.IntMap ([Ident], WeakTerm))
