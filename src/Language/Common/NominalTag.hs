module Language.Common.NominalTag
  ( NominalTag (..),
    nominalTagToText,
    isTermTag,
    isMacroTag,
    isDestPassingTag,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics (Generic)

data NominalTag
  = Define
  | DestPassing
  | DestPassingInline
  | Inline
  | Constant
  | ConstantMeta
  | Macro
  | MacroInline
  | Alias
  | AliasOpaque
  | Data
  | DataRaw
  | Resource
  deriving (Eq, Ord, Show, Generic)

instance Binary NominalTag

nominalTagToText :: NominalTag -> T.Text
nominalTagToText tag =
  case tag of
    Define ->
      "define"
    DestPassing ->
      "define"
    DestPassingInline ->
      "inline"
    Inline ->
      "inline"
    Constant ->
      "constant"
    ConstantMeta ->
      "constant-meta"
    Macro ->
      "define-meta"
    MacroInline ->
      "inline-meta"
    Alias ->
      "alias"
    AliasOpaque ->
      "alias-opaque"
    Data ->
      "data"
    DataRaw ->
      "data-raw"
    Resource ->
      "resource"

isTermTag :: NominalTag -> Bool
isTermTag tag =
  case tag of
    Define ->
      True
    DestPassing ->
      True
    DestPassingInline ->
      True
    Inline ->
      True
    Constant ->
      True
    ConstantMeta ->
      True
    Macro ->
      True
    MacroInline ->
      True
    Alias ->
      False
    AliasOpaque ->
      False
    Data ->
      False
    DataRaw ->
      False
    Resource ->
      False

isMacroTag :: NominalTag -> Bool
isMacroTag tag =
  case tag of
    Macro ->
      True
    MacroInline ->
      True
    ConstantMeta ->
      False
    _ ->
      False

isDestPassingTag :: NominalTag -> Bool
isDestPassingTag tag =
  case tag of
    DestPassing ->
      True
    DestPassingInline ->
      True
    _ ->
      False
