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
  | Calloc ty a a -- Calloc sizeType num size
  | Malloc ty a -- Malloc sizeType size
  | Realloc ty a a -- Realloc sizeType ptr size
  | Free ty a -- Free unitType ptr
  | InspectType MID.ModuleID ty ty -- typeValueExpr, e (both types)
  | EqType MID.ModuleID ty ty
  | ShowType ty -- typeExpr
  | AssertMixable MID.ModuleID ty ty -- unitTypeExpr, targetTypeExpr
  | TextCons a a -- rune, text
  | TextUncons MID.ModuleID a -- text
  | CompileError a -- msg
  | GetOriginFileName
  | GetOriginLine
  | GetOriginColumn
  deriving (Show, Eq, G.Generic)

instance (Binary lt, Binary ty, Binary a) => Binary (Magic lt ty a)

instance Functor (Magic lt ty) where
  fmap f der =
    case der of
      LowMagic magic ->
        LowMagic (fmap f magic)
      Calloc sizeType num size ->
        Calloc sizeType (f num) (f size)
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
      ShowType typeExpr ->
        ShowType typeExpr
      AssertMixable mid unitTypeExpr typeExpr ->
        AssertMixable mid unitTypeExpr typeExpr
      TextCons rune text ->
        TextCons (f rune) (f text)
      TextUncons mid text ->
        TextUncons mid (f text)
      CompileError msg ->
        CompileError (f msg)
      GetOriginFileName ->
        GetOriginFileName
      GetOriginLine ->
        GetOriginLine
      GetOriginColumn ->
        GetOriginColumn

instance Foldable (Magic lt ty) where
  foldMap f der =
    case der of
      LowMagic magic ->
        foldMap f magic
      Calloc _ num size ->
        f num <> f size
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
      AssertMixable {} ->
        mempty
      TextCons rune text ->
        f rune <> f text
      TextUncons _ text ->
        f text
      CompileError msg ->
        f msg
      GetOriginFileName ->
        mempty
      GetOriginLine ->
        mempty
      GetOriginColumn ->
        mempty

instance Traversable (Magic lt ty) where
  traverse f der =
    case der of
      LowMagic magic ->
        LowMagic <$> traverse f magic
      Calloc sizeType num size ->
        Calloc sizeType <$> f num <*> f size
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
      ShowType typeExpr ->
        pure $ ShowType typeExpr
      AssertMixable mid unitTypeExpr typeExpr ->
        pure $ AssertMixable mid unitTypeExpr typeExpr
      TextCons rune text ->
        TextCons <$> f rune <*> f text
      TextUncons mid text ->
        TextUncons mid <$> f text
      CompileError msg ->
        CompileError <$> f msg
      GetOriginFileName ->
        pure GetOriginFileName
      GetOriginLine ->
        pure GetOriginLine
      GetOriginColumn ->
        pure GetOriginColumn

newtype WeakMagic lt ty a = WeakMagic (Magic lt ty a)
