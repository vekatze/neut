module Language.Common.PrimType.FromBasePrimType (fromBasePrimType) where

import Language.Common.BasePrimType qualified as BPT
import Language.Common.PrimType qualified as PT

fromBasePrimType :: BPT.BasePrimType -> PT.PrimType
fromBasePrimType bpt =
  case bpt of
    BPT.Int size -> do
      case size of
        BPT.Explicit s ->
          PT.Int s
        BPT.Implicit s ->
          PT.Int s
    BPT.Float size -> do
      case size of
        BPT.Explicit s ->
          PT.Float s
        BPT.Implicit s ->
          PT.Float s
