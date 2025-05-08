module Language.Common.Rule.PrimType.FromBasePrimType (fromBasePrimType) where

import Language.Common.Rule.BasePrimType qualified as BPT
import Language.Common.Rule.PrimType qualified as PT

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
