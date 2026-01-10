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
  | Inline
  | Macro
  | MacroInline
  | Alias
  | AliasOpaque
  | Data
  deriving (Eq, Ord, Show, Generic)

instance Binary NominalTag

nominalTagToText :: NominalTag -> T.Text
nominalTagToText tag =
  case tag of
    Define ->
      "define"
    Inline ->
      "inline"
    Macro ->
      "macro"
    MacroInline ->
      "macro-inline"
    Alias ->
      "alias"
    AliasOpaque ->
      "alias-opaque"
    Data ->
      "data"

isTermTag :: NominalTag -> Bool
isTermTag tag =
  case tag of
    Define ->
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

isMacroTag :: NominalTag -> Bool
isMacroTag tag =
  case tag of
    Macro ->
      True
    MacroInline ->
      True
    _ ->
      False
