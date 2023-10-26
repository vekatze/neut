module Entity.StmtKind
  ( StmtKind,
    RawStmtKind,
    BaseStmtKind (..),
    toOpacity,
    toLowOpacity,
  )
where

import Data.Binary
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.Ident
import Entity.IsConstLike
import Entity.Opacity qualified as O
import Entity.RawIdent
import Entity.RawTerm qualified as RT
import GHC.Generics

data BaseStmtKind x t
  = Normal O.Opacity
  | Data
      DD.DefiniteDescription -- the name of the variant type
      [(Hint, x, t)] -- variant args
      [(Hint, DD.DefiniteDescription, IsConstLike, [(Hint, x, t)], D.Discriminant)] -- constructors
  | DataIntro DD.DefiniteDescription [(Hint, x, t)] [(Hint, x, t)] D.Discriminant
  deriving (Generic)

instance (Binary x, Binary t) => Binary (BaseStmtKind x t)

type StmtKind a =
  BaseStmtKind Ident a

type RawStmtKind =
  BaseStmtKind RawIdent RT.RawTerm

toOpacity :: BaseStmtKind x t -> O.Opacity
toOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    _ ->
      O.Clear

toLowOpacity :: BaseStmtKind x t -> O.Opacity
toLowOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    Data {} ->
      O.Opaque -- so as not to reduce recursive terms
    DataIntro {} ->
      O.Clear
