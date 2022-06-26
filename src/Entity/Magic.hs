{-# LANGUAGE DeriveGeneric #-}

module Entity.Magic where

import Data.Binary
import qualified Data.Text as T
import Entity.LowType
import qualified GHC.Generics as G

data Magic a
  = MagicCast a a a
  | MagicStore LowType a a
  | MagicLoad LowType a
  | MagicSyscall Integer [a]
  | MagicExternal T.Text [a]
  deriving (Show, Eq, G.Generic)

instance (Binary a) => Binary (Magic a)

instance Functor Magic where
  fmap f der =
    case der of
      MagicCast from to value ->
        MagicCast (f from) (f to) (f value)
      MagicStore lt pointer value ->
        MagicStore lt (f pointer) (f value)
      MagicLoad lt pointer ->
        MagicLoad lt (f pointer)
      MagicSyscall syscallNum args ->
        MagicSyscall syscallNum $ fmap f args
      MagicExternal extFunName args ->
        MagicExternal extFunName $ fmap f args

instance Foldable Magic where
  foldMap f der =
    case der of
      MagicCast from to value ->
        f from <> f to <> f value
      MagicStore _ pointer value ->
        f pointer <> f value
      MagicLoad _ pointer ->
        f pointer
      MagicSyscall _ args ->
        foldMap f args
      MagicExternal _ args ->
        foldMap f args

instance Traversable Magic where
  traverse f der =
    case der of
      MagicCast from to value ->
        MagicCast <$> f from <*> f to <*> f value
      MagicStore lt pointer value ->
        MagicStore lt <$> f pointer <*> f value
      MagicLoad lt pointer ->
        MagicLoad lt <$> f pointer
      MagicSyscall syscallNum args ->
        MagicSyscall syscallNum <$> traverse f args
      MagicExternal extFunName args ->
        MagicExternal extFunName <$> traverse f args

getMagicName :: Magic a -> T.Text
getMagicName d =
  case d of
    MagicSyscall {} ->
      "syscall"
    MagicExternal {} ->
      "external"
    MagicLoad {} ->
      "load"
    MagicStore {} ->
      "store"
    MagicCast {} ->
      "nop"
