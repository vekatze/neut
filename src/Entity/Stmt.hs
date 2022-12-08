module Entity.Stmt where

import Control.Comonad.Cofree
import Data.Binary
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.Arity as A
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.Hint
import qualified Entity.ImpArgNum as I
import qualified Entity.Opacity as O
import qualified Entity.RawTerm as RT
import qualified Entity.Section as Section
import qualified Entity.Source as Source
import qualified Entity.Term as TM
import qualified Entity.WeakTerm as WT
import qualified Entity.WeakTerm.ToText as WT
import GHC.Generics
import Path

type PreProgram =
  (Path Abs File, [RawStmt])

data StmtKindF a
  = Normal O.Opacity
  | Data A.Arity DD.DefiniteDescription [DD.DefiniteDescription]
  | DataIntro DD.DefiniteDescription [BinderF a] [BinderF a] D.Discriminant
  deriving (Generic)

toOpacity :: StmtKindF a -> O.Opacity
toOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    _ ->
      O.Transparent

instance Binary a => Binary (StmtKindF a)

data RawStmt
  = RawStmtDefine
      (StmtKindF RT.RawTerm)
      Hint
      DD.DefiniteDescription
      I.ImpArgNum
      [BinderF RT.RawTerm]
      RT.RawTerm
      RT.RawTerm
  | RawStmtSection Section.Section [RawStmt]

data WeakStmt
  = WeakStmtDefine
      (StmtKindF WT.WeakTerm)
      Hint
      DD.DefiniteDescription
      I.ImpArgNum
      [BinderF WT.WeakTerm]
      WT.WeakTerm
      WT.WeakTerm

type Program =
  (Source.Source, [Stmt])

data Stmt
  = StmtDefine
      (StmtKindF TM.Term)
      Hint
      DD.DefiniteDescription
      I.ImpArgNum
      [BinderF TM.Term]
      TM.Term
      TM.Term
  deriving (Generic)

instance Binary Stmt

type PathSet = S.Set (Path Abs File)

newtype Cache = Cache
  { cacheStmtList :: [Stmt]
  }
  deriving (Generic)

instance Binary Cache

compress :: Stmt -> Stmt
compress stmt =
  case stmt of
    StmtDefine stmtKind m functionName impArgNum args codType _ ->
      case stmtKind of
        Normal O.Opaque ->
          StmtDefine stmtKind m functionName impArgNum args codType (m :< TM.Tau)
        _ ->
          stmt

showStmt :: WeakStmt -> T.Text
showStmt (WeakStmtDefine _ m x _ xts codType e) = do
  DD.reify x <> "\n" <> WT.toText (m :< WT.Pi xts codType) <> "\n" <> WT.toText (m :< WT.Pi xts e)
