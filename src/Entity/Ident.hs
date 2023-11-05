module Entity.Ident where

import Data.Binary
import Data.Text qualified as T
import Entity.Const
import GHC.Generics

newtype Ident
  = I (T.Text, Int)
  deriving (Ord, Generic)

instance Eq Ident where
  I (_, i1) == I (_, i2) = i1 == i2

instance Show Ident where
  show (I (s, i)) =
    T.unpack s ++ "-" ++ show i

instance Binary Ident

attachHolePrefix :: Ident -> Ident
attachHolePrefix (I (varName, i)) =
  I (holeVarPrefix <> varName, i)

isHole :: Ident -> Bool
isHole (I (varName, _)) =
  holeVarPrefix `T.isPrefixOf` varName

innerLength :: Ident -> Int
innerLength (I (varName, _)) =
  T.length varName
