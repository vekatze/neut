module Entity.PrimType.FromWeakPrimType (fromWeakPrimType) where

import Entity.PrimType qualified as PT
import Entity.WeakPrimType qualified as WPT

fromWeakPrimType :: WPT.WeakPrimType -> PT.PrimType
fromWeakPrimType wpt =
  case wpt of
    WPT.Int size -> do
      case size of
        WPT.Explicit s ->
          PT.Int s
        WPT.Implicit s ->
          PT.Int s
    WPT.Float size -> do
      case size of
        WPT.Explicit s ->
          PT.Float s
        WPT.Implicit s ->
          PT.Float s
