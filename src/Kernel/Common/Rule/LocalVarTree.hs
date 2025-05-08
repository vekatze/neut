module Kernel.Common.Rule.LocalVarTree
  ( LocalVarTree (..),
    empty,
    insert,
    collect,
  )
where

import Aux.Logger.Rule.Hint
import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Rule.Ident

data LocalVarTree
  = Leaf
  | Node Loc Loc Ident LocalVarTree LocalVarTree
  deriving (Generic)

instance Binary LocalVarTree

empty :: LocalVarTree
empty =
  Leaf

insert :: Loc -> Loc -> Ident -> LocalVarTree -> LocalVarTree
insert startLoc endLoc x tree = do
  case tree of
    Leaf ->
      Node startLoc endLoc x Leaf Leaf
    Node startLoc' endLoc' x' t1 t2 ->
      case compare startLoc startLoc' of
        LT ->
          Node startLoc' endLoc' x' (insert startLoc endLoc x t1) t2
        GT ->
          Node startLoc' endLoc' x' t1 (insert startLoc endLoc x t2)
        EQ ->
          Node startLoc endLoc x t1 t2

collect :: Loc -> LocalVarTree -> [Ident]
collect loc tree = do
  case tree of
    Leaf ->
      []
    Node startLoc endLoc x t1 t2
      | loc < startLoc ->
          collect loc t1
      | otherwise ->
          if loc < endLoc
            then x : collect loc t1 ++ collect loc t2
            else collect loc t1 ++ collect loc t2
