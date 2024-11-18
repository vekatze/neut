module Entity.PrimType.FromBasePrimType (fromBasePrimType) where

import Entity.BasePrimType qualified as BPT
import Entity.PrimType qualified as PT

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
