module Language.Common.LowMagic (LowMagic (..)) where

import Data.Bifunctor
import Data.Binary
import GHC.Generics qualified as G
import Language.Common.BaseLowType
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType qualified as FCT

data LowMagic t a
  = Cast a a a
  | Store t a a a
  | Load t a
  | Alloca t a
  | External [t] (FCT.ForeignCodType t) EN.ExternalName [a] [(a, t)]
  | Global EN.ExternalName t
  | OpaqueValue a
  | CallType a a a
  deriving (Show, Eq, G.Generic)

instance (Binary a) => Binary (LowMagic BaseLowType a)

instance Functor (LowMagic BaseLowType) where
  fmap f der =
    case der of
      Cast from to value ->
        Cast (f from) (f to) (f value)
      Store lt unit value pointer ->
        Store lt (f unit) (f value) (f pointer)
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

instance Foldable (LowMagic BaseLowType) where
  foldMap f der =
    case der of
      Cast from to value ->
        f from <> f to <> f value
      Store _ unit value pointer ->
        f unit <> f value <> f pointer
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

instance Traversable (LowMagic BaseLowType) where
  traverse f der =
    case der of
      Cast from to value ->
        Cast <$> f from <*> f to <*> f value
      Store lt unit value pointer ->
        Store lt <$> f unit <*> f value <*> f pointer
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
