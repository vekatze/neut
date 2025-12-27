module Language.Common.Magic
  ( Magic (..),
    WeakMagic (..),
  )
where

import Data.Bifunctor
import Data.Binary
import Data.Text qualified as T
import GHC.Generics qualified as G
import Language.Common.BaseLowType
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.LowMagic qualified as LM
import Language.Common.StrictGlobalLocator qualified as SGL

data Magic t a
  = LowMagic (LM.LowMagic t a)
  | GetTypeTag a
  | GetConsSize a
  | GetConstructorArgTypes SGL.StrictGlobalLocator a a a
  | CompileError T.Text
  deriving (Show, Eq, G.Generic)

instance (Binary a) => Binary (Magic BaseLowType a)

instance Functor (Magic BaseLowType) where
  fmap f der =
    case der of
      LowMagic magic ->
        LowMagic (fmap f magic)
      GetTypeTag e ->
        GetTypeTag (f e)
      GetConsSize typeExpr ->
        GetConsSize (f typeExpr)
      GetConstructorArgTypes sgl listExpr typeExpr index ->
        GetConstructorArgTypes sgl (f listExpr) (f typeExpr) (f index)
      CompileError msg ->
        CompileError msg

instance Foldable (Magic BaseLowType) where
  foldMap f der =
    case der of
      LowMagic magic ->
        foldMap f magic
      GetTypeTag e ->
        f e
      GetConsSize typeExpr ->
        f typeExpr
      GetConstructorArgTypes _ listExpr typeExpr index ->
        f listExpr <> f typeExpr <> f index
      CompileError _ ->
        mempty

instance Traversable (Magic BaseLowType) where
  traverse f der =
    case der of
      LowMagic magic ->
        LowMagic <$> traverse f magic
      GetTypeTag e ->
        GetTypeTag <$> f e
      GetConsSize typeExpr ->
        GetConsSize <$> f typeExpr
      GetConstructorArgTypes sgl listExpr typeExpr index ->
        GetConstructorArgTypes sgl <$> f listExpr <*> f typeExpr <*> f index
      CompileError msg ->
        pure $ CompileError msg

newtype WeakMagic a = WeakMagic (Magic a a)

instance Functor WeakMagic where
  fmap f (WeakMagic der) =
    case der of
      LowMagic magic ->
        WeakMagic $
          LowMagic $
            case magic of
              LM.Cast from to value ->
                LM.Cast (f from) (f to) (f value)
              LM.Store t unit value pointer ->
                LM.Store (f t) (f unit) (f value) (f pointer)
              LM.Load t pointer ->
                LM.Load (f t) (f pointer)
              LM.Alloca t size ->
                LM.Alloca (f t) (f size)
              LM.External domList cod extFunName args varArgs -> do
                let domList' = map f domList
                let cod' = fmap f cod
                let varArgs' = map (bimap f f) varArgs
                LM.External domList' cod' extFunName (fmap f args) varArgs'
              LM.Global name t ->
                LM.Global name (f t)
              LM.OpaqueValue e ->
                LM.OpaqueValue (f e)
              LM.CallType func arg1 arg2 ->
                LM.CallType (f func) (f arg1) (f arg2)
      GetTypeTag e ->
        WeakMagic (GetTypeTag (f e))
      GetConsSize typeExpr ->
        WeakMagic (GetConsSize (f typeExpr))
      GetConstructorArgTypes sgl listExpr typeExpr index ->
        WeakMagic (GetConstructorArgTypes sgl (f listExpr) (f typeExpr) (f index))
      CompileError msg ->
        WeakMagic (CompileError msg)

instance Foldable WeakMagic where
  foldMap f (WeakMagic der) =
    case der of
      LowMagic magic ->
        case magic of
          LM.Cast from to value ->
            f from <> f to <> f value
          LM.Store t unit value pointer ->
            f t <> f unit <> f value <> f pointer
          LM.Load t pointer ->
            f t <> f pointer
          LM.Alloca t size ->
            f t <> f size
          LM.External domList cod _ args varArgs -> do
            let varArgs' = concatMap (\(x, y) -> [x, y]) varArgs
            case cod of
              FCT.Cod t ->
                foldMap f (domList ++ [t] ++ args ++ varArgs')
              FCT.Void ->
                foldMap f (domList ++ args ++ varArgs')
          LM.Global _ t ->
            f t
          LM.OpaqueValue e ->
            f e
          LM.CallType func arg1 arg2 ->
            f func <> f arg1 <> f arg2
      GetTypeTag e ->
        f e
      GetConsSize typeExpr ->
        f typeExpr
      GetConstructorArgTypes _ listExpr typeExpr index ->
        f listExpr <> f typeExpr <> f index
      CompileError _ ->
        mempty

instance Traversable WeakMagic where
  traverse f (WeakMagic der) =
    case der of
      LowMagic magic ->
        case magic of
          LM.Cast from to value -> do
            from' <- f from
            to' <- f to
            value' <- f value
            return $ WeakMagic $ LowMagic $ LM.Cast from' to' value'
          LM.Store t unit value pointer -> do
            t' <- f t
            unit' <- f unit
            value' <- f value
            pointer' <- f pointer
            return $ WeakMagic $ LowMagic $ LM.Store t' unit' value' pointer'
          LM.Load t pointer -> do
            t' <- f t
            pointer' <- f pointer
            return $ WeakMagic $ LowMagic $ LM.Load t' pointer'
          LM.Alloca t size -> do
            t' <- f t
            size' <- f size
            return $ WeakMagic $ LowMagic $ LM.Alloca t' size'
          LM.External domList cod extFunName args varArgs -> do
            domList' <- traverse f domList
            cod' <- traverse f cod
            args' <- traverse f args
            let (xs, ys) = unzip varArgs
            xs' <- traverse f xs
            ys' <- traverse f ys
            return $ WeakMagic $ LowMagic $ LM.External domList' cod' extFunName args' (zip xs' ys')
          LM.Global name t -> do
            t' <- f t
            return $ WeakMagic $ LowMagic $ LM.Global name t'
          LM.OpaqueValue e -> do
            e' <- f e
            return $ WeakMagic $ LowMagic $ LM.OpaqueValue e'
          LM.CallType func arg1 arg2 -> do
            func' <- f func
            arg1' <- f arg1
            arg2' <- f arg2
            return $ WeakMagic $ LowMagic $ LM.CallType func' arg1' arg2'
      GetTypeTag e -> do
        e' <- f e
        return $ WeakMagic $ GetTypeTag e'
      GetConsSize typeExpr -> do
        typeExpr' <- f typeExpr
        return $ WeakMagic $ GetConsSize typeExpr'
      GetConstructorArgTypes sgl listExpr typeExpr index -> do
        listExpr' <- f listExpr
        typeExpr' <- f typeExpr
        index' <- f index
        return $ WeakMagic $ GetConstructorArgTypes sgl listExpr' typeExpr' index'
      CompileError msg ->
        pure $ WeakMagic $ CompileError msg
