module Entity.Magic where

import Data.Bifunctor
import Data.Binary
import Data.Text qualified as T
import Entity.ExternalName qualified as EN
import Entity.LowType
import GHC.Generics qualified as G

data Magic a
  = Cast a a a
  | Store LowType a a
  | Load LowType a
  | External [LowType] LowType EN.ExternalName [a] [(a, LowType)]
  | Global EN.ExternalName LowType
  deriving (Show, Eq, G.Generic)

instance (Binary a) => Binary (Magic a)

instance Functor Magic where
  fmap f der =
    case der of
      Cast from to value ->
        Cast (f from) (f to) (f value)
      Store lt value pointer ->
        Store lt (f value) (f pointer)
      Load lt pointer ->
        Load lt (f pointer)
      External domList cod extFunName args varArgs -> do
        let varArgs' = map (first f) varArgs
        External domList cod extFunName (fmap f args) varArgs'
      Global name lt ->
        Global name lt

instance Foldable Magic where
  foldMap f der =
    case der of
      Cast from to value ->
        f from <> f to <> f value
      Store _ value pointer ->
        f value <> f pointer
      Load _ pointer ->
        f pointer
      External _ _ _ args varArgs ->
        foldMap f (args ++ map fst varArgs)
      Global {} ->
        mempty

instance Traversable Magic where
  traverse f der =
    case der of
      Cast from to value ->
        Cast <$> f from <*> f to <*> f value
      Store lt value pointer ->
        Store lt <$> f value <*> f pointer
      Load lt pointer ->
        Load lt <$> f pointer
      External domList cod extFunName args varArgs -> do
        let swap (x, y) = (y, x)
        let varArgs' = traverse (fmap swap . traverse f . swap) varArgs
        External domList cod extFunName <$> traverse f args <*> varArgs'
      Global name lt ->
        pure $ Global name lt

getMagicName :: Magic a -> T.Text
getMagicName d =
  case d of
    External {} ->
      "external"
    Load {} ->
      "load"
    Store {} ->
      "store"
    Cast {} ->
      "nop"
    Global {} ->
      "global"
