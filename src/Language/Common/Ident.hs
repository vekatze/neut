module Language.Common.Ident
  ( Ident (..),
    isHole,
    isCartesian,
  )
where

import Binary.Derive (deriveBinary)
import Data.Text qualified as T
import Language.Common.Const

newtype Ident
  = I (T.Text, Int)

instance Eq Ident where
  I (_, i1) == I (_, i2) = i1 == i2

instance Ord Ident where
  I (_, i1) `compare` I (_, i2) = i1 `compare` i2

instance Show Ident where
  show (I (s, i)) =
    T.unpack s ++ "_" ++ show i

isHole :: Ident -> Bool
isHole (I (varName, _)) =
  holeVarPrefix `T.isPrefixOf` varName

isCartesian :: Ident -> Bool
isCartesian (I (varName, _)) =
  expVarPrefix `T.isPrefixOf` varName

deriveBinary [''Ident]
