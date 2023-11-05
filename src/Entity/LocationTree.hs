module Entity.LocationTree
  ( LocationTree,
    empty,
    insert,
    find,
    toList,
  )
where

import Data.Binary
import Entity.Hint
import Entity.Hint.Reify
import GHC.Generics (Generic)

type ColInterval =
  (Int, Int)

-- I'll do balancing stuff later
data LocationTree
  = Leaf
  | Node (Line, ColInterval) SavedHint LocationTree LocationTree
  deriving (Show, Generic)

instance Binary LocationTree

empty :: LocationTree
empty =
  Leaf

insert :: (Int, (Int, Int)) -> Hint -> LocationTree -> LocationTree
insert loc value t =
  case t of
    Leaf ->
      Node loc (SavedHint value) Leaf Leaf
    Node loc' value' t1 t2 -> do
      case cmp loc loc' of
        LT ->
          Node loc' value' (insert loc value t1) t2
        GT ->
          Node loc' value' t1 (insert loc value t2)
        EQ ->
          t

find :: Int -> Int -> LocationTree -> Maybe Hint
find line col t =
  case t of
    Leaf ->
      Nothing
    Node (line', (colFrom, colTo)) (SavedHint value) t1 t2 ->
      case compare line line' of
        LT ->
          find line col t1
        GT ->
          find line col t2
        EQ ->
          case (col < colFrom, colTo < col) of
            (True, _) ->
              find line col t1
            (_, True) ->
              find line col t2
            _ ->
              Just value

cmp :: (Int, (Int, Int)) -> (Int, (Int, Int)) -> Ordering
cmp (line, (colFrom, _)) (line', (colFrom', _)) =
  case compare line line' of
    LT ->
      LT
    GT ->
      GT
    EQ ->
      compare colFrom colFrom'

toList :: LocationTree -> [((Line, ColInterval), String)]
toList t =
  case t of
    Leaf ->
      []
    Node loc (SavedHint m) t1 t2 ->
      (loc, toString m) : toList t1 ++ toList t2
