module Entity.Stmt where

import Control.Comonad.Cofree
import Data.Binary
import qualified Data.Set as S
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.EnumInfo
import Entity.Hint
import qualified Entity.ImpArgNum as I
import Entity.Opacity
import qualified Entity.PreTerm as PT
import qualified Entity.Section as Section
import qualified Entity.Source as Source
import qualified Entity.Term as TM
import qualified Entity.WeakTerm as WT
import GHC.Generics
import Path

type PreProgram =
  (Path Abs File, [PreStmt])

data PreStmt
  = PreStmtDefine Opacity Hint DD.DefiniteDescription I.ImpArgNum [BinderF PT.PreTerm] PT.PreTerm PT.PreTerm
  | PreStmtSection Section.Section [PreStmt]

data WeakStmt
  = WeakStmtDefine Opacity Hint DD.DefiniteDescription I.ImpArgNum [BinderF WT.WeakTerm] WT.WeakTerm WT.WeakTerm

type Program =
  (Source.Source, [Stmt])

data Stmt
  = StmtDefine Opacity Hint DD.DefiniteDescription I.ImpArgNum [BinderF TM.Term] TM.Term TM.Term
  deriving (Generic)

instance Binary Stmt

type PathSet = S.Set (Path Abs File)

data Cache = Cache
  { cacheStmtList :: [Stmt],
    cacheEnumInfo :: [EnumInfo]
  }
  deriving (Generic)

instance Binary Cache

compress :: Stmt -> Stmt
compress stmt =
  case stmt of
    StmtDefine opacity m functionName impArgNum args codType _ ->
      case opacity of
        OpacityOpaque ->
          StmtDefine opacity m functionName impArgNum args codType (m :< TM.Tau)
        _ ->
          stmt
