module Language.Common.Rule.Magic
  ( Magic (..),
    WeakMagic (..),
  )
where

import Data.Bifunctor
import Data.Binary
import GHC.Generics qualified as G
import Language.Common.Rule.BaseLowType
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.ForeignCodType qualified as FCT

data Magic t a
  = Cast a a a
  | Store t a a a
  | Load t a
  | Alloca t a
  | External [t] (FCT.ForeignCodType t) EN.ExternalName [a] [(a, t)]
  | Global EN.ExternalName t
  | OpaqueValue a
  deriving (Show, Eq, G.Generic)

instance (Binary a) => Binary (Magic BaseLowType a)

instance Functor (Magic BaseLowType) where
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

instance Foldable (Magic BaseLowType) where
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

instance Traversable (Magic BaseLowType) where
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

newtype WeakMagic a = WeakMagic (Magic a a)

instance Functor WeakMagic where
  fmap f (WeakMagic der) =
    case der of
      Cast from to value ->
        WeakMagic (Cast (f from) (f to) (f value))
      Store t unit value pointer ->
        WeakMagic (Store (f t) (f unit) (f value) (f pointer))
      Load t pointer ->
        WeakMagic (Load (f t) (f pointer))
      Alloca t size ->
        WeakMagic (Alloca (f t) (f size))
      External domList cod extFunName args varArgs -> do
        let domList' = map f domList
        let cod' = fmap f cod
        let varArgs' = map (bimap f f) varArgs
        WeakMagic (External domList' cod' extFunName (fmap f args) varArgs')
      Global name t ->
        WeakMagic (Global name (f t))
      OpaqueValue e ->
        WeakMagic (OpaqueValue (f e))

instance Foldable WeakMagic where
  foldMap f (WeakMagic der) =
    case der of
      Cast from to value ->
        f from <> f to <> f value
      Store t unit value pointer ->
        f t <> f unit <> f value <> f pointer
      Load t pointer ->
        f t <> f pointer
      Alloca t size ->
        f t <> f size
      External domList cod _ args varArgs -> do
        let varArgs' = concatMap (\(x, y) -> [x, y]) varArgs
        case cod of
          FCT.Cod t ->
            foldMap f (domList ++ [t] ++ args ++ varArgs')
          FCT.Void ->
            foldMap f (domList ++ args ++ varArgs')
      Global _ t ->
        f t
      OpaqueValue e ->
        f e

instance Traversable WeakMagic where
  traverse f (WeakMagic der) =
    case der of
      Cast from to value -> do
        from' <- f from
        to' <- f to
        value' <- f value
        return $ WeakMagic $ Cast from' to' value'
      Store t unit value pointer -> do
        t' <- f t
        unit' <- f unit
        value' <- f value
        pointer' <- f pointer
        return $ WeakMagic $ Store t' unit' value' pointer'
      Load t pointer -> do
        t' <- f t
        pointer' <- f pointer
        return $ WeakMagic $ Load t' pointer'
      Alloca t size -> do
        t' <- f t
        size' <- f size
        return $ WeakMagic $ Alloca t' size'
      External domList cod extFunName args varArgs -> do
        domList' <- traverse f domList
        cod' <- traverse f cod
        args' <- traverse f args
        let (xs, ys) = unzip varArgs
        xs' <- traverse f xs
        ys' <- traverse f ys
        return $ WeakMagic $ External domList' cod' extFunName args' (zip xs' ys')
      Global name t -> do
        t' <- f t
        return $ WeakMagic $ Global name t'
      OpaqueValue e -> do
        e' <- f e
        return $ WeakMagic $ OpaqueValue e'
