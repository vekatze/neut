module Entity.Magic where

import Data.Binary
import Data.Text qualified as T
import Entity.ExternalName qualified as EN
import Entity.LowType
import GHC.Generics qualified as G

data Magic a
  = Cast a a a
  | Store LowType a a
  | Load LowType a
  | External [LowType] LowType EN.ExternalName [a] [(LowType, a)]
  | Global LowType EN.ExternalName
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
      External domList cod extFunName args varArgs ->
        External domList cod extFunName (fmap f args) (fmap (fmap f) varArgs)
      Global lt name ->
        Global lt name

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
        foldMap f (args ++ map snd varArgs)
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
      External domList cod extFunName args varArgs ->
        External domList cod extFunName <$> traverse f args <*> traverse (traverse f) varArgs
      Global lt name ->
        pure $ Global lt name

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
