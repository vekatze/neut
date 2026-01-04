module SyntaxTree.Block (Block, Block') where

import Logger.Hint
import SyntaxTree.C

type Block a =
  (C, (a, C))

type Block' a =
  (C, (a, Loc, C))
