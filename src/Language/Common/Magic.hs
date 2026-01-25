module Language.Common.Magic
  ( Magic (..),
    WeakMagic (..),
  )
where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.LowMagic qualified as LM
import Language.Common.ModuleID qualified as MID
import Language.Common.StrictGlobalLocator qualified as SGL

data Magic lt ty a
  = LowMagic (LM.LowMagic lt ty a)
  | GetTypeTag MID.ModuleID ty ty -- typeTagExpr, e (both types)
  | GetDataArgs SGL.StrictGlobalLocator ty ty -- listExpr, typeExpr (both types)
  | GetConsSize ty
  | GetConstructorArgTypes SGL.StrictGlobalLocator ty ty a -- listExpr, typeExpr (types), index (term)
  | GetConsName ty ty a -- textType, type, index
  | GetConsConstFlag ty ty a -- boolType, type, index
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
      GetTypeTag mid typeTagExpr e ->
        GetTypeTag mid typeTagExpr e
      GetDataArgs sgl listExpr typeExpr ->
        GetDataArgs sgl listExpr typeExpr
      GetConsSize typeExpr ->
        GetConsSize typeExpr
      GetConstructorArgTypes sgl listExpr typeExpr index ->
        GetConstructorArgTypes sgl listExpr typeExpr (f index)
      GetConsName textType typeExpr index ->
        GetConsName textType typeExpr (f index)
      GetConsConstFlag boolType typeExpr index ->
        GetConsConstFlag boolType typeExpr (f index)
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
      GetTypeTag {} ->
        mempty
      GetDataArgs {} ->
        mempty
      GetConsSize {} ->
        mempty
      GetConstructorArgTypes _ _ _ index ->
        f index
      GetConsName _ _ index ->
        f index
      GetConsConstFlag _ _ index ->
        f index
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
      GetTypeTag mid typeTagExpr e ->
        pure $ GetTypeTag mid typeTagExpr e
      GetDataArgs sgl listExpr typeExpr ->
        pure $ GetDataArgs sgl listExpr typeExpr
      GetConsSize typeExpr ->
        pure $ GetConsSize typeExpr
      GetConstructorArgTypes sgl listExpr typeExpr index ->
        GetConstructorArgTypes sgl listExpr typeExpr <$> f index
      GetConsName textType typeExpr index ->
        GetConsName textType typeExpr <$> f index
      GetConsConstFlag boolType typeExpr index ->
        GetConsConstFlag boolType typeExpr <$> f index
      ShowType textTypeExpr typeExpr ->
        pure $ ShowType textTypeExpr typeExpr
      TextCons textTypeExpr rune text ->
        TextCons textTypeExpr <$> f rune <*> f text
      TextUncons mid text ->
        TextUncons mid <$> f text
      CompileError typeExpr msg ->
        CompileError typeExpr <$> f msg

newtype WeakMagic lt ty a = WeakMagic (Magic lt ty a)
