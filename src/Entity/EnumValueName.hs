module Entity.EnumValueName where

-- ( EnumValueName (..),
--   new,
--   new',
-- )

-- import qualified Context.Throw as Throw
-- import Data.Binary
-- import qualified Data.Text as T
-- import qualified Entity.DefiniteDescription as DD
-- import qualified Entity.EnumTypeName as ET
-- import qualified Entity.Hint as H
-- import qualified Entity.LocalLocator as LL
-- import GHC.Generics

-- newtype EnumValueName = EnumValueName {reify :: DD.DefiniteDescription}
--   deriving (Generic, Eq, Ord)

-- instance Binary EnumValueName

-- instance Show EnumValueName where
--   show v = T.unpack $ DD.reify $ reify v

-- new :: Throw.Context m => H.Hint -> ET.EnumTypeName -> T.Text -> m EnumValueName
-- new m (ET.EnumTypeName enumTypeName) valueBaseName = do
--   EnumValueName <$> DD.extend m enumTypeName valueBaseName

-- new' :: ET.EnumTypeName -> LL.LocalLocator -> EnumValueName
-- new' (ET.EnumTypeName enumTypeName) valueBaseName = do
--   EnumValueName $ DD.extendLL enumTypeName valueBaseName
