module Language.Common.Magic
  ( Magic (..),
    WeakMagic (..),
  )
where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.LowMagic qualified as LM
import Language.Common.ModuleID qualified as MID

data Magic lt ty a
  = LowMagic (LM.LowMagic lt ty a)
  | Malloc ty a -- Malloc sizeType size
  | Realloc ty a a -- Realloc sizeType ptr size
  | Free ty a -- Free unitType ptr
  | InspectType MID.ModuleID ty ty -- typeValueExpr, e (both types)
  | EqType MID.ModuleID ty ty
  | ShowType ty ty
  | TextCons ty a a
  | TextUncons MID.ModuleID a
  | CompileError ty a
  deriving (Show, Eq, G.Generic)

instance (Binary lt, Binary ty, Binary a) => Binary (Magic lt ty a)

instance Functor (Magic lt ty) where
  fmap f der =
    case der of
      LowMagic magic ->
        LowMagic (fmap f magic)
      Malloc sizeType size ->
        Malloc sizeType (f size)
      Realloc sizeType ptr size ->
        Realloc sizeType (f ptr) (f size)
      Free unitType ptr ->
        Free unitType (f ptr)
      InspectType mid typeValueExpr e ->
        InspectType mid typeValueExpr e
      EqType mid t1 t2 ->
        EqType mid t1 t2
      ShowType textTypeExpr typeExpr ->
        ShowType textTypeExpr typeExpr
      TextCons textTypeExpr rune text ->
        TextCons textTypeExpr (f rune) (f text)
      TextUncons mid text ->
        TextUncons mid (f text)
      CompileError typeExpr msg ->
        CompileError typeExpr (f msg)

instance Foldable (Magic lt ty) where
  foldMap f der =
    case der of
      LowMagic magic ->
        foldMap f magic
      Malloc _ size ->
        f size
      Realloc _ ptr size ->
        f ptr <> f size
      Free _ ptr ->
        f ptr
      InspectType {} ->
        mempty
      EqType {} ->
        mempty
      ShowType {} ->
        mempty
      TextCons _ rune text ->
        f rune <> f text
      TextUncons _ text ->
        f text
      CompileError _ msg ->
        f msg

instance Traversable (Magic lt ty) where
  traverse f der =
    case der of
      LowMagic magic ->
        LowMagic <$> traverse f magic
      Malloc sizeType size ->
        Malloc sizeType <$> f size
      Realloc sizeType ptr size ->
        Realloc sizeType <$> f ptr <*> f size
      Free unitType ptr ->
        Free unitType <$> f ptr
      InspectType mid typeValueExpr e ->
        pure $ InspectType mid typeValueExpr e
      EqType mid t1 t2 ->
        pure $ EqType mid t1 t2
      ShowType textTypeExpr typeExpr ->
        pure $ ShowType textTypeExpr typeExpr
      TextCons textTypeExpr rune text ->
        TextCons textTypeExpr <$> f rune <*> f text
      TextUncons mid text ->
        TextUncons mid <$> f text
      CompileError typeExpr msg ->
        CompileError typeExpr <$> f msg

newtype WeakMagic lt ty a = WeakMagic (Magic lt ty a)
