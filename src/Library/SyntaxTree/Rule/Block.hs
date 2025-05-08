module Library.SyntaxTree.Rule.Block (Block, Block', mapBlock, mapBlock') where

import Library.Logger.Rule.Hint
import Library.SyntaxTree.Rule.C

type Block a =
  (C, (a, C))

type Block' a =
  (C, (a, Loc, C))

mapBlock :: (a -> b) -> Block a -> Block b
mapBlock f (c1, (x, c2)) =
  (c1, (f x, c2))

mapBlock' :: (a -> b) -> Block' a -> Block' b
mapBlock' f (c1, (x, loc, c2)) =
  (c1, (f x, loc, c2))
