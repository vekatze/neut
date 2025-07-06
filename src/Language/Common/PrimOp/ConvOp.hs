module Language.Common.PrimOp.ConvOp
  ( ConvOp (..),
    asConvOp,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics qualified as G
import Language.Common.PrimNumSize.ToInt
import Language.Common.PrimType qualified as PT

data ConvOp
  = Trunc
  | Zext
  | Sext
  | Fptrunc
  | Fpext
  | Fptoui
  | Fptosi
  | Uitofp
  | Sitofp
  deriving (Eq, Ord, G.Generic)

instance Binary ConvOp

instance Show ConvOp where
  show convOp =
    case convOp of
      Trunc ->
        "trunc"
      Zext ->
        "zext"
      Sext ->
        "sext"
      Fptrunc ->
        "fptrunc"
      Fpext ->
        "fpext"
      Fptoui ->
        "fptoui"
      Fptosi ->
        "fptosi"
      Uitofp ->
        "uitofp"
      Sitofp ->
        "sitofp"

asConvOp :: T.Text -> PT.PrimType -> PT.PrimType -> Maybe ConvOp
asConvOp name domType codType =
  case name of
    "trunc"
      | PT.Int i1 <- domType,
        PT.Int i2 <- codType,
        intSizeToInt i1 > intSizeToInt i2 ->
          return Trunc
    "zext"
      | PT.Int i1 <- domType,
        PT.Int i2 <- codType,
        intSizeToInt i1 < intSizeToInt i2 ->
          return Zext
    "sext"
      | PT.Int i1 <- domType,
        PT.Int i2 <- codType,
        intSizeToInt i1 < intSizeToInt i2 ->
          return Sext
    "fptrunc"
      | PT.Float size1 <- domType,
        PT.Float size2 <- codType,
        floatSizeToInt size1 > floatSizeToInt size2 ->
          return Fptrunc
    "fpext"
      | PT.Float size1 <- domType,
        PT.Float size2 <- codType,
        floatSizeToInt size1 < floatSizeToInt size2 ->
          return Fpext
    "fptoui"
      | PT.Float _ <- domType,
        PT.Int _ <- codType ->
          return Fptoui
    "fptosi"
      | PT.Float _ <- domType,
        PT.Int _ <- codType ->
          return Fptosi
    "uitofp"
      | PT.Int _ <- domType,
        PT.Float _ <- codType ->
          return Uitofp
    "sitofp"
      | PT.Int _ <- domType,
        PT.Float _ <- codType ->
          return Sitofp
    _ ->
      Nothing
