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
  deriving (Show, Functor, Foldable, Traversable)
