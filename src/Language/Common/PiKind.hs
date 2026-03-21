module Language.Common.PiKind
  ( PiKind (..),
    normal,
    fromStmtKind,
    fromNominalTag,
  )
where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.NominalTag (NominalTag, isDestPassingTag)
import Language.Common.StmtKind qualified as SK

data PiKind
  = Normal IsConstLike
  | DestPass IsConstLike
  | DataIntro IsConstLike
  deriving (Show, Eq, Generic)

instance Binary PiKind

normal :: PiKind
normal =
  Normal False

fromStmtKind :: SK.BaseStmtKindTerm name binder t -> IsConstLike -> PiKind
fromStmtKind stmtKind isConstLike =
  if SK.isDestPassingStmtKind stmtKind
    then DestPass isConstLike
    else do
      case stmtKind of
        SK.DataIntro {} ->
          DataIntro isConstLike
        _ ->
          Normal isConstLike

fromNominalTag :: NominalTag -> IsConstLike -> PiKind
fromNominalTag tag isConstLike =
  if isDestPassingTag tag
    then DestPass isConstLike
    else Normal isConstLike
