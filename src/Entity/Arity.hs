module Entity.Arity where

import Data.Binary
import GHC.Generics (Generic)

newtype Arity = Arity {reify :: Integer}
  deriving (Show, Generic, Ord, Eq)

instance Binary Arity

fromInt :: Int -> Arity
fromInt i =
  Arity
    { reify = toInteger i
    }

-- S4@(switch, target)
arityS4 :: Arity
arityS4 =
  Arity 2
