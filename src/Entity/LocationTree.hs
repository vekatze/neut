module Entity.LocationTree
  ( LocationTree,
    LocType (..),
    empty,
    insert,
    find,
    findRef,
  )
where

import Data.Binary
import Entity.Hint
import GHC.Generics (Generic)

type ColInterval =
  (Int, Int)

data LocType
  = FileLoc
  | SymbolLoc
  deriving (Show, Generic)

-- I'll do balancing stuff later
data LocationTree
  = Leaf
  | Node LocType (Line, ColInterval) SavedHint LocationTree LocationTree
  deriving (Show, Generic)

instance Binary LocationTree

instance Binary LocType

empty :: LocationTree
empty =
  Leaf

insert :: LocType -> (Int, (Int, Int)) -> Hint -> LocationTree -> LocationTree
insert lt loc value t =
  case t of
    Leaf ->
      Node lt loc (SavedHint value) Leaf Leaf
    Node lt' loc' value' t1 t2 -> do
      case cmp loc loc' of
        LT ->
          Node lt' loc' value' (insert lt loc value t1) t2
        GT ->
          Node lt' loc' value' t1 (insert lt loc value t2)
        EQ ->
          t

find :: Int -> Int -> LocationTree -> Maybe (Hint, ColInterval)
find line col t =
  case t of
    Leaf ->
      Nothing
    Node _ (line', (colFrom, colTo)) (SavedHint value) t1 t2 ->
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
              Just (value, (colFrom, colTo))

findRef :: Loc -> LocationTree -> [(FilePath, (Line, ColInterval))]
findRef loc t =
  case t of
    Leaf ->
      []
    Node lt locRange (SavedHint m') left right
      | loc == metaLocation m',
        SymbolLoc <- lt ->
          (metaFileName m', locRange) : findRef loc left ++ findRef loc right
      | otherwise ->
          findRef loc left ++ findRef loc right

cmp :: (Int, (Int, Int)) -> (Int, (Int, Int)) -> Ordering
cmp (line, (colFrom, _)) (line', (colFrom', _)) =
  case compare line line' of
    LT ->
      LT
    GT ->
      GT
    EQ ->
      compare colFrom colFrom'
