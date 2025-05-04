module Language.WeakTerm.Rule.WeakPrim (WeakPrim (..)) where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.Rule.PrimType qualified as PT
import Language.WeakTerm.Rule.WeakPrimValue qualified as PV

data WeakPrim a
  = Type PT.PrimType
  | Value (PV.WeakPrimValue a)
  deriving (Show, G.Generic)

instance (Binary a) => Binary (WeakPrim a)

instance Functor WeakPrim where
  fmap f prim =
    case prim of
      Value primValue ->
        Value (fmap f primValue)
      Type primType ->
        Type primType

instance Foldable WeakPrim where
  foldMap f prim =
    case prim of
      Value primValue ->
        foldMap f primValue
      Type _ ->
        mempty

instance Traversable WeakPrim where
  traverse f prim =
    case prim of
      Value primValue ->
        Value <$> traverse f primValue
      Type primType ->
        pure (Type primType)
