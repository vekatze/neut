module Entity.Magic where

import Data.Bifunctor
import Data.Binary
import Entity.BaseLowType
import Entity.ExternalName qualified as EN
import Entity.ForeignCodType qualified as FCT
import Entity.LowType
import GHC.Generics qualified as G

data Magic t a
  = Cast a a a
  | Store LowType a a
  | Load LowType a
  | Alloca LowType a
  | External [t] (FCT.ForeignCodType t) EN.ExternalName [a] [(a, t)]
  | Global EN.ExternalName LowType
  deriving (Show, Eq, G.Generic)

instance (Binary a) => Binary (Magic BaseLowType a)

instance Functor (Magic BaseLowType) where
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

instance Foldable (Magic BaseLowType) where
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

instance Traversable (Magic BaseLowType) where
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
        let cod' = fmap f cod
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
        case cod of
          FCT.Cod t ->
            foldMap f (domList ++ [t] ++ args ++ varArgs')
          FCT.Void ->
            foldMap f (domList ++ args ++ varArgs')
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
        cod' <- traverse f cod
        args' <- traverse f args
        let (xs, ys) = unzip varArgs
        xs' <- traverse f xs
        ys' <- traverse f ys
        return $ WeakMagic $ External domList' cod' extFunName args' (zip xs' ys')
      Global name lt -> do
        return $ WeakMagic $ Global name lt
