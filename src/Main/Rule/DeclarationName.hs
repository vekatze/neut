module Main.Rule.DeclarationName
  ( DeclarationName (..),
    DeclEnv,
    reify,
    toBuilder,
  )
where

import Data.ByteString.Builder
import Data.HashMap.Strict qualified as Map
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Main.Rule.BaseLowType
import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.ExternalName qualified as EN
import Main.Rule.ForeignCodType qualified as F

data DeclarationName
  = In DD.DefiniteDescription
  | Ext EN.ExternalName
  deriving (Eq, Ord, Show, Generic)

instance Hashable DeclarationName

type DeclEnv = Map.HashMap DeclarationName ([BaseLowType], F.ForeignCodType BaseLowType)

toBuilder :: DeclarationName -> Builder
toBuilder dn =
  case dn of
    In dd ->
      DD.toBuilder dd
    Ext (EN.ExternalName rawTxt) ->
      TE.encodeUtf8Builder rawTxt

reify :: DeclarationName -> T.Text
reify dn =
  case dn of
    In dd ->
      DD.reify dd
    Ext (EN.ExternalName rawTxt) ->
      rawTxt
