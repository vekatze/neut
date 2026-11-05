module Language.Common.LamKind
  ( LamKindF (..),
    fromLamKind,
    getCodType,
    getIsDestPassing,
  )
where

import Binary.Derive (deriveBinary)
import Data.Text qualified as T
import Language.Common.Binder
import Language.Common.IsDestPassing
import Language.Common.LocalDefKind (LocalDefKind)

data LamKindF a
  = Normal (Maybe T.Text) IsDestPassing a
  | Fix LocalDefKind IsDestPassing (BinderF a)
  deriving (Show, Functor, Foldable, Traversable)

fromLamKind :: LamKindF a -> Maybe (BinderF a)
fromLamKind k =
  case k of
    Fix _ _ x ->
      Just x
    _ ->
      Nothing

getCodType :: LamKindF a -> a
getCodType k =
  case k of
    Normal _ _ codType ->
      codType
    Fix _ _ (_, _, _, codType) ->
      codType

getIsDestPassing :: LamKindF a -> IsDestPassing
getIsDestPassing k =
  case k of
    Normal _ isDestPassing _ ->
      isDestPassing
    Fix _ isDestPassing _ ->
      isDestPassing

deriveBinary [''LamKindF]
