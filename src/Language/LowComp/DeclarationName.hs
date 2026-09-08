module Language.LowComp.DeclarationName
  ( DeclarationName (..),
    Variadicity (..),
    DeclEnv,
    reify,
    toBuilder,
  )
where

import Data.ByteString.Builder
import Data.HashMap.Strict qualified as Map
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics
import Language.Common.BaseLowType
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType qualified as F

data DeclarationName
  = In DD.DefiniteDescription
  | Ext EN.ExternalName
  deriving (Eq, Ord, Show, Generic)

instance Hashable DeclarationName

data Variadicity
  = Fixed
  | Variadic
  deriving (Eq, Ord, Show, Generic)

type DeclEnv = Map.HashMap DeclarationName ([BaseLowType], F.ForeignCodType BaseLowType, Variadicity)

toBuilder :: DeclarationName -> Builder
toBuilder dn =
  case dn of
    In dd ->
      DD.toBuilder dd
    Ext extName ->
      EN.toBuilder extName

reify :: DeclarationName -> T.Text
reify dn =
  case dn of
    In dd ->
      DD.reify dd
    Ext (EN.ExternalName rawTxt) ->
      rawTxt
