module Language.Term.PrimValue
  ( PrimValue (..),
  )
where

import Data.Binary
import Data.ByteString qualified as BS
import Data.Text qualified as T
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import GHC.Generics (Generic)
import Language.Common.PrimNumSize
import Language.Common.PrimOp
import Language.Common.Rune qualified as RU

data PrimValue a
  = Int a IntSize Integer
  | Float a FloatSize Double
  | Op PrimOp
  | NoeticString a T.Text
  | NoeticBinary a BS.ByteString
  | Text T.Text
  | Blob BS.ByteString
  | Rune RU.Rune
  deriving (Show, Generic)

instance (Binary a) => Binary (PrimValue a) where
  put primValue = do
    case primValue of
      Int t size value -> do
        putWord8 0
        put t
        put size
        put value
      Float t size value -> do
        putWord8 1
        put t
        put size
        put $ castDoubleToWord64 value
      Op op -> do
        putWord8 2
        put op
      NoeticString t text -> do
        putWord8 3
        put t
        put text
      NoeticBinary t bytes -> do
        putWord8 4
        put t
        put bytes
      Text text -> do
        putWord8 5
        put text
      Blob bytes -> do
        putWord8 6
        put bytes
      Rune rune -> do
        putWord8 7
        put rune

  get = do
    tag <- getWord8
    case tag of
      0 ->
        Int <$> get <*> get <*> get
      1 -> do
        t <- get
        size <- get
        Float t size . castWord64ToDouble <$> get
      2 ->
        Op <$> get
      3 ->
        NoeticString <$> get <*> get
      4 ->
        NoeticBinary <$> get <*> get
      5 ->
        Text <$> get
      6 ->
        Blob <$> get
      7 ->
        Rune <$> get
      _ ->
        fail "invalid PrimValue tag"
