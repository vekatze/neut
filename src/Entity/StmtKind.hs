module Entity.StmtKind
  ( StmtKind,
    RawStmtKind,
    BaseStmtKind (..),
    toOpacity,
    toLowOpacity,
  )
where

import Data.Binary
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.IsConstLike
import Entity.Opacity qualified as O
import Entity.RawBinder (RawBinder)
import Entity.RawTerm qualified as RT
import GHC.Generics

data BaseStmtKind b t
  = Normal O.Opacity
  | Data
      DD.DefiniteDescription -- the name of the variant type
      [b] -- variant args
      [(SavedHint, DD.DefiniteDescription, IsConstLike, [b], D.Discriminant)] -- constructors
  | DataIntro DD.DefiniteDescription [b] [b] D.Discriminant
  deriving (Generic)

instance (Binary x, Binary t) => Binary (BaseStmtKind x t)

type StmtKind a =
  BaseStmtKind (BinderF a) a

type RawStmtKind =
  BaseStmtKind (RawBinder RT.RawTerm) RT.RawTerm

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
