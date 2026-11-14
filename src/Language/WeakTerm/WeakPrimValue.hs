module Language.WeakTerm.WeakPrimValue
  ( WeakPrimValue (..),
  )
where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Language.Common.ModuleID qualified as MID
import Language.Common.PrimOp
import Language.Common.Rune qualified as RU

data WeakPrimValue a
  = Int a Integer
  | Float a Double
  | Op PrimOp
  | String MID.ModuleID a BS.ByteString
  | NoeticString a T.Text
  | NoeticBinary a BS.ByteString
  | Text T.Text
  | Blob BS.ByteString
  | Rune RU.Rune
  deriving (Show)

instance Functor WeakPrimValue where
  fmap f primValue =
    case primValue of
      Int t v ->
        Int (f t) v
      Float t v ->
        Float (f t) v
      Op op ->
        Op op
      String coreModuleID t bytes ->
        String coreModuleID (f t) bytes
      NoeticString t text ->
        NoeticString (f t) text
      NoeticBinary t bytes ->
        NoeticBinary (f t) bytes
      Text text ->
        Text text
      Blob bytes ->
        Blob bytes
      Rune r ->
        Rune r

instance Foldable WeakPrimValue where
  foldMap f primValue =
    case primValue of
      Int t _ ->
        f t
      Float t _ ->
        f t
      Op _ ->
        mempty
      String _ t _ ->
        f t
      NoeticString t _ ->
        f t
      NoeticBinary t _ ->
        f t
      Text _ ->
        mempty
      Blob _ ->
        mempty
      Rune _ ->
        mempty

instance Traversable WeakPrimValue where
  traverse f primValue =
    case primValue of
      Int t v ->
        Int <$> f t <*> pure v
      Float t v ->
        Float <$> f t <*> pure v
      Op op ->
        pure $ Op op
      String coreModuleID t bytes ->
        String coreModuleID <$> f t <*> pure bytes
      NoeticString t text ->
        NoeticString <$> f t <*> pure text
      NoeticBinary t bytes ->
        NoeticBinary <$> f t <*> pure bytes
      Text text ->
        pure $ Text text
      Blob bytes ->
        pure $ Blob bytes
      Rune r ->
        pure $ Rune r
