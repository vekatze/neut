module Scene.Parse.LowType (interpretLowType) where

import Control.Comonad.Cofree
import Entity.DataSize qualified as DS
import Entity.Error
import Entity.LowType
import Entity.LowType qualified as LT
import Entity.PrimType.FromText qualified as PT
import Entity.Tree

interpretLowType :: DS.DataSize -> Tree -> EE LowType
interpretLowType dataSize t = do
  case t of
    _ :< Atom _ -> do
      (_, sym) <- getSymbol t
      case sym of
        "ptr" ->
          return LT.Pointer
        "unit" ->
          return LT.Void
        _ -> do
          case PT.fromText dataSize sym of
            Just primNum ->
              return $ LT.PrimNum primNum
            Nothing ->
              Left $ unknownLowTypeError t
    m :< Node ts -> do
      (headTree, rest) <- treeUncons m ts
      (_, sym) <- getSymbol headTree
      case sym of
        "array" -> do
          (_, size@(mSize :< _), lt) <- getTreeListOfSize3 (m, ts)
          case getInt size of
            Just (_, size') -> do
              lt' <- interpretLowType dataSize lt
              return $ LT.Array size' lt'
            Nothing ->
              Left $ newError mSize $ "an integer is expected, but found: " <> showTree size
        "struct" -> do
          rest' <- mapM (interpretLowType dataSize) rest
          return $ LT.Struct rest'
        _ -> do
          Left $ unknownLowTypeError t
    _ :< List _ ->
      Left $ unknownLowTypeError t

unknownLowTypeError :: Tree -> Error
unknownLowTypeError t@(m :< _) =
  newError m $ "no such lowtype is defined: " <> showTree t
