module Entity.Macro
  ( Arg (..),
    Args,
    Rules,
    Sub,
    MacroInfo,
  )
where

import Data.Binary
import Data.HashMap.Strict qualified as Map
import Entity.RawIdent
import Entity.Tree
import GHC.Generics (Generic)

data Arg
  = Literal RawIdent
  | Var RawIdent
  | ArgNode Args
  deriving (Show, Generic)

instance Binary Arg

type Args =
  ([Arg], Maybe RawIdent)

type Rules =
  Map.HashMap RawIdent [(Args, Tree)]

type Sub =
  Map.HashMap RawIdent Tree

type MacroInfo =
  (RawIdent, [(Args, Tree)])
