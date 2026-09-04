module Kernel.Common.LocalVarTree
  ( LocalVarTree (..),
    Entry,
    empty,
    fromList,
    collect,
  )
where

import Data.Binary
import Data.List (sortOn)
import GHC.Generics (Generic)
import Language.Common.Ident
import Logger.Hint

data LocalVarTree
  = Leaf
  | Node Loc Loc Ident LocalVarTree LocalVarTree
  deriving (Generic)

instance Binary LocalVarTree

type Entry =
  (Loc, Loc, Ident)

empty :: LocalVarTree
empty =
  Leaf

fromList :: [Entry] -> LocalVarTree
fromList entries = do
  build $ dropShadowed $ sortOn (\(startLoc, _, _) -> startLoc) entries

dropShadowed :: [Entry] -> [Entry]
dropShadowed entries =
  case entries of
    [] ->
      []
    [entry] ->
      [entry]
    entry@(startLoc, _, _) : rest@((startLoc', _, _) : _)
      | startLoc == startLoc' ->
          dropShadowed rest
      | otherwise ->
          entry : dropShadowed rest

build :: [Entry] -> LocalVarTree
build entries =
  case splitAt (length entries `div` 2) entries of
    (_, []) ->
      Leaf
    (smaller, (startLoc, endLoc, x) : larger) ->
      Node startLoc endLoc x (build smaller) (build larger)

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
