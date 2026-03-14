module Language.Common.NominalTag
  ( NominalTag (..),
    nominalTagToText,
    isTermTag,
    isMacroTag,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics (Generic)

data NominalTag
  = Define
  | Script
  | Inline
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
    Script ->
      "script"
    Inline ->
      "inline"
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
    Script ->
      True
    Inline ->
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
