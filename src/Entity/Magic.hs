module Entity.Magic where

import Data.Bifunctor
import Data.Binary
import Entity.ExternalName qualified as EN
import Entity.LowType
import GHC.Generics qualified as G

data Magic t a
  = Cast a a a
  | Store LowType a a
  | Load LowType a
  | Alloca LowType a
  | External [t] t EN.ExternalName [a] [(a, t)]
  | Global EN.ExternalName LowType
  deriving (Show, Eq, G.Generic)

instance (Binary a) => Binary (Magic LowType a)

instance Functor (Magic LowType) where
  fmap f der =
    case der of
      Cast from to value ->
        Cast (f from) (f to) (f value)
      Store lt value pointer ->
        Store lt (f value) (f pointer)
      Load lt pointer ->
        Load lt (f pointer)
      Alloca lt size ->
        Alloca lt (f size)
      External domList cod extFunName args varArgs -> do
        let varArgs' = map (first f) varArgs
        External domList cod extFunName (fmap f args) varArgs'
      Global name lt ->
        Global name lt

instance Foldable (Magic LowType) where
  foldMap f der =
    case der of
      Cast from to value ->
        f from <> f to <> f value
      Store _ value pointer ->
        f value <> f pointer
      Load _ pointer ->
        f pointer
      Alloca {} ->
        mempty
      External _ _ _ args varArgs ->
        foldMap f (args ++ map fst varArgs)
      Global {} ->
        mempty

instance Traversable (Magic LowType) where
  traverse f der =
    case der of
      Cast from to value ->
        Cast <$> f from <*> f to <*> f value
      Store lt value pointer ->
        Store lt <$> f value <*> f pointer
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

newtype WeakMagic a = WeakMagic (Magic a a)

instance Functor WeakMagic where
  fmap f (WeakMagic der) =
    case der of
      Cast from to value ->
        WeakMagic (Cast (f from) (f to) (f value))
      Store lt value pointer ->
        WeakMagic (Store lt (f value) (f pointer))
      Load lt pointer ->
        WeakMagic (Load lt (f pointer))
      Alloca lt size ->
        WeakMagic (Alloca lt (f size))
      External domList cod extFunName args varArgs -> do
        let domList' = map f domList
        let cod' = f cod
        let varArgs' = map (bimap f f) varArgs
        WeakMagic (External domList' cod' extFunName (fmap f args) varArgs')
      Global name lt ->
        WeakMagic (Global name lt)

instance Foldable WeakMagic where
  foldMap f (WeakMagic der) =
    case der of
      Cast from to value ->
        f from <> f to <> f value
      Store _ value pointer ->
        f value <> f pointer
      Load _ pointer ->
        f pointer
      Alloca {} ->
        mempty
      External domList cod _ args varArgs -> do
        let varArgs' = concatMap (\(x, y) -> [x, y]) varArgs
        foldMap f (domList ++ [cod] ++ args ++ varArgs')
      Global {} ->
        mempty

instance Traversable WeakMagic where
  traverse f (WeakMagic der) =
    case der of
      Cast from to value -> do
        from' <- f from
        to' <- f to
        value' <- f value
        return $ WeakMagic $ Cast from' to' value'
      Store lt value pointer -> do
        value' <- f value
        pointer' <- f pointer
        return $ WeakMagic $ Store lt value' pointer'
      Load lt pointer -> do
        pointer' <- f pointer
        return $ WeakMagic $ Load lt pointer'
      Alloca lt size -> do
        size' <- f size
        return $ WeakMagic $ Alloca lt size'
      External domList cod extFunName args varArgs -> do
        domList' <- traverse f domList
        cod' <- f cod
        args' <- traverse f args
        let (xs, ys) = unzip varArgs
        xs' <- traverse f xs
        ys' <- traverse f ys
        return $ WeakMagic $ External domList' cod' extFunName args' (zip xs' ys')
      Global name lt -> do
        return $ WeakMagic $ Global name lt
