module Entity.Ident
  ( Ident (..),
    isHole,
    isCartesian,
  )
where

import Data.Binary
import Data.Text qualified as T
import Entity.Const
import GHC.Generics

newtype Ident
  = I (T.Text, Int)
  deriving (Generic)

instance Eq Ident where
  I (_, i1) == I (_, i2) = i1 == i2

instance Ord Ident where
  I (_, i1) `compare` I (_, i2) = i1 `compare` i2

instance Show Ident where
  show (I (s, i)) =
    T.unpack s ++ "-" ++ show i

instance Binary Ident

isHole :: Ident -> Bool
isHole (I (varName, _)) =
  holeVarPrefix `T.isPrefixOf` varName

isCartesian :: Ident -> Bool
isCartesian (I (varName, _)) =
  expVarPrefix `T.isPrefixOf` varName
