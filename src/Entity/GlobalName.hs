module Entity.GlobalName
  ( GlobalName (..),
    showKind,
  )
where

import Data.Binary
import Data.Text qualified as T
import Entity.ArgNum
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.IsConstLike
import Entity.Macro (Args)
import Entity.PrimOp
import Entity.PrimType qualified as PT
import Entity.Tree (Tree)
import GHC.Generics (Generic)

data GlobalName
  = TopLevelFunc ArgNum IsConstLike
  | PrimType PT.PrimType
  | PrimOp PrimOp
  | Data ArgNum [(DD.DefiniteDescription, (Hint, GlobalName))] IsConstLike
  | DataIntro ArgNum ArgNum D.Discriminant IsConstLike
  | Resource
  | Macro
  deriving (Generic)

instance Binary GlobalName

showKind :: GlobalName -> T.Text
showKind gn =
  case gn of
    TopLevelFunc {} ->
      "top-level function"
    PrimType {} ->
      "primitive type"
    PrimOp {} ->
      "primitive operation"
    Data {} ->
      "data"
    DataIntro {} ->
      "constructor"
    Resource {} ->
      "resource"
    Macro {} ->
      "macro"
