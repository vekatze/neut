module Language.Common.LocalDefKind
  ( LocalDefKind (..),
    toOpacity,
    isMeta,
    keyword,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics
import Language.Common.Opacity qualified as O

data LocalDefKind
  = Define
  | Inline
  | DefineMeta
  | InlineMeta
  deriving (Show, Eq, Generic)

instance Binary LocalDefKind

toOpacity :: LocalDefKind -> O.Opacity
toOpacity k =
  case k of
    Define ->
      O.Opaque
    Inline ->
      O.Clear
    DefineMeta ->
      O.Opaque
    InlineMeta ->
      O.Clear

isMeta :: LocalDefKind -> Bool
isMeta k =
  case k of
    Define ->
      False
    Inline ->
      False
    DefineMeta ->
      True
    InlineMeta ->
      True

keyword :: LocalDefKind -> T.Text
keyword k =
  case k of
    Define ->
      "define"
    Inline ->
      "inline"
    DefineMeta ->
      "define-meta"
    InlineMeta ->
      "inline-meta"
