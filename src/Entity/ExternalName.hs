module Entity.ExternalName where

import Data.Binary
import Data.ByteString.Builder
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics

newtype ExternalName = ExternalName {reify :: T.Text}
  deriving (Generic, Show, Eq, Ord)

instance Binary ExternalName

instance Hashable ExternalName

malloc :: ExternalName
malloc =
  ExternalName "malloc"

free :: ExternalName
free =
  ExternalName "free"

toBuilder :: ExternalName -> Builder
toBuilder (ExternalName rawTxt) =
  TE.encodeUtf8Builder rawTxt
