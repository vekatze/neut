module Entity.Macro
  ( Arg (..),
    Args,
    Rules,
    Sub,
  )
where

import Data.HashMap.Strict qualified as Map
import Entity.RawIdent
import Entity.Tree

data Arg
  = Literal RawIdent
  | Var RawIdent
  | ArgNode Args
  deriving (Show)

type Args =
  ([Arg], Maybe RawIdent)

type Rules =
  Map.HashMap RawIdent [(Args, Tree)]

type Sub =
  Map.HashMap RawIdent Tree
