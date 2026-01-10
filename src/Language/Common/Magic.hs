module Language.Common.Magic
  ( Magic (..),
    WeakMagic (..),
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics qualified as G
import Language.Common.LowMagic qualified as LM
import Language.Common.ModuleID qualified as MID
import Language.Common.StrictGlobalLocator qualified as SGL

data Magic lt ty a
  = LowMagic (LM.LowMagic lt ty a)
  | GetTypeTag MID.ModuleID ty ty -- typeTagExpr, e (both types)
  | GetDataArgs SGL.StrictGlobalLocator ty ty -- listExpr, typeExpr (both types)
  | GetConsSize ty
  | GetWrapperContentType ty
  | GetConstructorArgTypes SGL.StrictGlobalLocator ty ty a -- listExpr, typeExpr (types), index (term)
  | CompileError T.Text
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
      GetWrapperContentType typeExpr ->
        GetWrapperContentType typeExpr
      GetConstructorArgTypes sgl listExpr typeExpr index ->
        GetConstructorArgTypes sgl listExpr typeExpr (f index)
      CompileError msg ->
        CompileError msg

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
      GetWrapperContentType {} ->
        mempty
      GetConstructorArgTypes _ _ _ index ->
        f index
      CompileError _ ->
        mempty

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
      GetWrapperContentType typeExpr ->
        pure $ GetWrapperContentType typeExpr
      GetConstructorArgTypes sgl listExpr typeExpr index ->
        GetConstructorArgTypes sgl listExpr typeExpr <$> f index
      CompileError msg ->
        pure $ CompileError msg

newtype WeakMagic lt ty a = WeakMagic (Magic lt ty a)
