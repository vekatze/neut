module Language.Common.NominalTag
  ( NominalTag (..),
    nominalTagToText,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics (Generic)

data NominalTag
  = Define
  | Inline
  | Alias
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
    Alias ->
      "alias"
    Data ->
      "data"
