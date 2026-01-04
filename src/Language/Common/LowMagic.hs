module Language.Common.LowMagic (LowMagic (..)) where

import Data.Bifunctor
import Data.Binary
import GHC.Generics qualified as G
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType qualified as FCT

-- lt: lowtype, ty: type expression, a: term
data LowMagic lt ty a
  = Cast ty ty a -- Cast fromType toType value
  | Store lt ty a a -- Store valueType unitType value pointer
  | Load lt a -- Load valueType pointer
  | Alloca lt a -- Alloca elemType size
  | External [lt] (FCT.ForeignCodType lt) EN.ExternalName [a] [(a, lt)]
  | Global EN.ExternalName lt
  | OpaqueValue a
  | CallType a a a
  deriving (Show, Eq, G.Generic)

instance (Binary lt, Binary ty, Binary a) => Binary (LowMagic lt ty a)

instance Functor (LowMagic lt ty) where
  fmap f der =
    case der of
      Cast from to value ->
        Cast from to (f value)
      Store lt unit value pointer ->
        Store lt unit (f value) (f pointer)
      Load lt pointer ->
        Load lt (f pointer)
      Alloca lt size ->
        Alloca lt (f size)
      External domList cod extFunName args varArgs -> do
        let varArgs' = map (first f) varArgs
        External domList cod extFunName (fmap f args) varArgs'
      Global name lt ->
        Global name lt
      OpaqueValue e ->
        OpaqueValue (f e)
      CallType func arg1 arg2 ->
        CallType (f func) (f arg1) (f arg2)

instance Foldable (LowMagic lt ty) where
  foldMap f der =
    case der of
      Cast _ _ value ->
        f value
      Store _ _ value pointer ->
        f value <> f pointer
      Load _ pointer ->
        f pointer
      Alloca _ size ->
        f size
      External _ _ _ args varArgs ->
        foldMap f (args ++ map fst varArgs)
      Global {} ->
        mempty
      OpaqueValue e ->
        f e
      CallType func arg1 arg2 ->
        f func <> f arg1 <> f arg2

instance Traversable (LowMagic lt ty) where
  traverse f der =
    case der of
      Cast from to value ->
        Cast from to <$> f value
      Store lt unit value pointer ->
        Store lt unit <$> f value <*> f pointer
      Load lt pointer ->
        Load lt <$> f pointer
      Alloca lt size ->
        Alloca lt <$> f size
      External domList cod extFunName args varArgs -> do
        let swap (x, y) = (y, x)
        let varArgs' = traverse (fmap swap . traverse f . swap) varArgs
        External domList cod extFunName <$> traverse f args <*> varArgs'
      Global name lt ->
        pure $ Global name lt
      OpaqueValue e ->
        OpaqueValue <$> f e
      CallType func arg1 arg2 ->
        CallType <$> f func <*> f arg1 <*> f arg2
