module Entity.WeakPrim where

import Data.Binary
import Entity.PrimOp
import qualified Entity.PrimType as PT
import qualified Entity.WeakPrimValue as PV
import qualified GHC.Generics as G

data WeakPrim a
  = Op PrimOp
  | Type PT.PrimType
  | Value (PV.WeakPrimValue a)
  deriving (Show, G.Generic)

instance (Binary a) => Binary (WeakPrim a)

instance Functor WeakPrim where
  fmap f prim =
    case prim of
      Value primValue ->
        Value (fmap f primValue)
      Op primOp ->
        Op primOp
      Type primType ->
        Type primType

instance Foldable WeakPrim where
  foldMap f prim =
    case prim of
      Value primValue ->
        foldMap f primValue
      Op _ ->
        mempty
      Type _ ->
        mempty

instance Traversable WeakPrim where
  traverse f prim =
    case prim of
      Value primValue ->
        Value <$> traverse f primValue
      Op primOp ->
        pure (Op primOp)
      Type primType ->
        pure (Type primType)
