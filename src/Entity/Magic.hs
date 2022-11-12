module Entity.Magic where

import Data.Binary
import qualified Data.Text as T
import qualified Entity.ExternalName as EN
import Entity.LowType
import qualified GHC.Generics as G

data Magic a
  = Cast a a a
  | Store LowType a a
  | Load LowType a
  | Syscall Integer [a]
  | External EN.ExternalName [a]
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
      Syscall syscallNum args ->
        Syscall syscallNum $ fmap f args
      External extFunName args ->
        External extFunName $ fmap f args

instance Foldable Magic where
  foldMap f der =
    case der of
      Cast from to value ->
        f from <> f to <> f value
      Store _ pointer value ->
        f pointer <> f value
      Load _ pointer ->
        f pointer
      Syscall _ args ->
        foldMap f args
      External _ args ->
        foldMap f args

instance Traversable Magic where
  traverse f der =
    case der of
      Cast from to value ->
        Cast <$> f from <*> f to <*> f value
      Store lt pointer value ->
        Store lt <$> f pointer <*> f value
      Load lt pointer ->
        Load lt <$> f pointer
      Syscall syscallNum args ->
        Syscall syscallNum <$> traverse f args
      External extFunName args ->
        External extFunName <$> traverse f args

getMagicName :: Magic a -> T.Text
getMagicName d =
  case d of
    Syscall {} ->
      "syscall"
    External {} ->
      "external"
    Load {} ->
      "load"
    Store {} ->
      "store"
    Cast {} ->
      "nop"
