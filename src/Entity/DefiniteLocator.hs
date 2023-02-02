module Entity.DefiniteLocator
  ( DefiniteLocator (..),
    new,
  )
where

import Data.Binary
import qualified Entity.Section as S
import qualified Entity.StrictGlobalLocator as SGL
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
