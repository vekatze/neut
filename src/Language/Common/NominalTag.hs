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
  | Macro
  | MacroInline
  | Alias
  | AliasOpaque
  | Data
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
    Resource ->
      False

isMacroTag :: NominalTag -> Bool
isMacroTag tag =
  case tag of
    Macro ->
      True
    MacroInline ->
      True
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
