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
  | External EN.ExternalName [a]
  | Global LowType EN.ExternalName
  deriving (Show, Eq, G.Generic)

instance (Binary a) => Binary (Magic a)

instance Functor Magic where
  fmap f der =
    case der of
      Cast from to value ->
        Cast (f from) (f to) (f value)
      Store lt pointer value ->
        Store lt (f pointer) (f value)
      Load lt pointer ->
        Load lt (f pointer)
      External extFunName args ->
        External extFunName $ fmap f args
      Global lt name ->
        Global lt name

instance Foldable Magic where
  foldMap f der =
    case der of
      Cast from to value ->
        f from <> f to <> f value
      Store _ pointer value ->
        f pointer <> f value
      Load _ pointer ->
        f pointer
      External _ args ->
        foldMap f args
      Global {} ->
        mempty

instance Traversable Magic where
  traverse f der =
    case der of
      Cast from to value ->
        Cast <$> f from <*> f to <*> f value
      Store lt pointer value ->
        Store lt <$> f pointer <*> f value
      Load lt pointer ->
        Load lt <$> f pointer
      External extFunName args ->
        External extFunName <$> traverse f args
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
