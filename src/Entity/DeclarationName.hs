module Entity.DeclarationName where

import Data.ByteString.Builder
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import qualified Data.Text.Encoding as TE
import qualified Entity.DefiniteDescription as DD
import qualified Entity.ExternalName as EN
import Entity.LowType
import GHC.Generics

data DeclarationName
  = In DD.DefiniteDescription
  | Ext EN.ExternalName
  deriving (Eq, Ord, Show, Generic)

instance Hashable DeclarationName

type DeclEnv = Map.HashMap DeclarationName ([LowType], LowType)

malloc :: DeclarationName
malloc =
  Ext EN.malloc

free :: DeclarationName
free =
  Ext EN.free

toBuilder :: DeclarationName -> Builder
toBuilder dn =
  case dn of
    In dd ->
      DD.toBuilder dd
    Ext (EN.ExternalName rawTxt) ->
      TE.encodeUtf8Builder rawTxt
