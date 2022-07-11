module Entity.DefiniteDescription
  ( DefiniteDescription (..),
    new,
    newByGlobalLocator,
    newByDefiniteLocator,
    extend,
    extendLL,
  )
where

import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import Entity.Const
import qualified Entity.DefiniteLocator as DL
import qualified Entity.LocalLocator as LL
import qualified Entity.Section as Section
import qualified Entity.StrictGlobalLocator as SGL
import GHC.Generics

data DefiniteDescription = MakeDefiniteDescription
  { globalLocator :: SGL.StrictGlobalLocator,
    localLocator :: LL.LocalLocator,
    reify :: T.Text -- cache
  }
  deriving (Generic, Show)

instance Eq DefiniteDescription where
  dd1 == dd2 = reify dd1 == reify dd2

instance Ord DefiniteDescription where
  compare dd1 dd2 = compare (reify dd1) (reify dd2)

instance Binary DefiniteDescription

instance Hashable DefiniteDescription

new :: SGL.StrictGlobalLocator -> LL.LocalLocator -> DefiniteDescription
new gl ll =
  MakeDefiniteDescription
    { globalLocator = gl,
      localLocator = ll,
      reify = SGL.reify gl <> definiteSep <> LL.reify ll
    }

newByGlobalLocator :: SGL.StrictGlobalLocator -> [Section.Section] -> BN.BaseName -> DefiniteDescription
newByGlobalLocator gl sectionStack name = do
  new gl $ LL.new sectionStack name

newByDefiniteLocator :: DL.DefiniteLocator -> LL.LocalLocator -> DefiniteDescription
newByDefiniteLocator dl ll = do
  let gl = DL.globalLocator dl
  let ll' = LL.new (LL.sectionStack ll ++ DL.sectionStack dl) (LL.baseName ll)
  new gl ll'

extend :: DefiniteDescription -> T.Text -> DefiniteDescription
extend dd newName = do
  let gl = globalLocator dd
  let outer = localLocator dd
  let inner = LL.reflect newName
  let ll = LL.extend outer inner
  new gl ll

extendLL :: DefiniteDescription -> LL.LocalLocator -> DefiniteDescription
extendLL dd inner = do
  let gl = globalLocator dd
  let outer = localLocator dd
  new gl $ LL.extend outer inner

-- LL.LocalLocator
--   { LL.sectionStack = LL.sectionStack ll ++ DL.sectionStack dl,
--     LL.baseName = LL.baseName ll
--   }-- newtype DefiniteDescription = MakeDefiniteDescription {reify :: T.Text}
--   deriving (Semigroup, Monoid, Generic, Eq, Ord)

-- instance Show DefiniteDescription where
--   show dd = T.unpack $ reify dd

-- new :: SGL.StrictGlobalLocator -> LL.LocalLocator -> T.Text -> DefiniteDescription
-- new gl ll name =
--   MakeDefiniteDescription
--     { reify = SGL.reify gl <> definiteSep <> LL.reify ll <> nsSep <> name
--     }

-- data DefiniteDescription = MakeDefiniteDescription
--   { reify :: T.Text,
--     (...)
--   }
--   deriving (Generic, Eq, Ord)
-- type GlobalLocatorMap = Map.HashMap GL.GlobalLocator GL.GlobalLocator

-- newByDefiniteLocator :: DL.DefiniteLocator -> T.Text -> DefiniteDescription
-- newByDefiniteLocator dl baseName = do
--   let gl = DL.globalLocator dl
--   let ll =
--         LL.LocalLocator
--           { LL.sectionStack = DL.sectionStack dl,
--             LL.baseName = baseName
--           }
--   new gl ll

-- new
--   gl
--   LL.LocalLocator
--     { LL.sectionStack = Section.Section (LL.baseName ll) : LL.sectionStack ll,
--       LL.baseName = newName
--     }

-- MakeDefiniteDescription
--   { reify = DL.reify dl <> nsSep <> name
--   }

-- extractGlobalLocator :: DefiniteDescription -> SGL.StrictGlobalLocator
-- extractGlobalLocator dd =
--   case T.breakOn definiteSep (reify dd) of
--     (globalLocator, _)
--       | not (T.null rest) ->
--         globalLocator
--       | otherwise ->
--         dd
