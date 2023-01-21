module Entity.DefiniteLocator
  ( DefiniteLocator (..),
    new,
  )
where

import Data.Binary
import Entity.Section qualified as S
import Entity.StrictGlobalLocator qualified as SGL
import GHC.Generics

data DefiniteLocator = MakeDefiniteLocator
  { globalLocator :: SGL.StrictGlobalLocator,
    sectionStack :: [S.Section]
  }
  deriving (Generic)

instance Binary DefiniteLocator

new :: SGL.StrictGlobalLocator -> S.SectionStack -> DefiniteLocator
new gl ss =
  MakeDefiniteLocator
    { globalLocator = gl,
      sectionStack = ss
    }
