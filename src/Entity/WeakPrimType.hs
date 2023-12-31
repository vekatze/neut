module Entity.WeakPrimType where

data WeakPrimType
  = Int (Maybe Int)
  | Float (Maybe Int)
  deriving (Show, Eq, Ord)
