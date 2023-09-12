module Scene.Parse.LowType (interpretLowType) where

import Control.Comonad.Cofree
import Entity.Atom qualified as AT
import Entity.DataSize qualified as DS
import Entity.Error
import Entity.LowType
import Entity.LowType qualified as LT
import Entity.PrimType.FromText qualified as PT
import Entity.Tree

interpretLowType :: DS.DataSize -> Tree -> Either Error LowType
interpretLowType dataSize t = do
  case t of
    _ :< Atom (AT.Symbol atom)
      | atom == "ptr" ->
          return LT.Pointer
      | atom == "unit" ->
          return LT.Void
      | Just primNum <- PT.fromText dataSize atom ->
          return $ LT.PrimNum primNum
    _ :< Node ((_ :< Atom (AT.Symbol atom)) : rest)
      | atom == "array",
        [size, lt] <- rest,
        Just (_, size') <- getInt size -> do
          lt' <- interpretLowType dataSize lt
          return $ LT.Array size' lt'
      | atom == "struct" -> do
          rest' <- mapM (interpretLowType dataSize) rest
          return $ LT.Struct rest'
    m :< _ ->
      Left $ newError m "interpretLowType"
