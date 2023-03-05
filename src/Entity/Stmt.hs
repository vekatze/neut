module Entity.Stmt where

import Control.Comonad.Cofree
import Data.Binary
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.Opacity qualified as O
import Entity.RawTerm qualified as RT
import Entity.Section qualified as Section
import Entity.Source qualified as Source
import Entity.Term qualified as TM
import Entity.WeakTerm qualified as WT
import Entity.WeakTerm.ToText qualified as WT
import GHC.Generics
import Path

type ConsInfo = (DD.DefiniteDescription, [BinderF TM.Term], D.Discriminant)

type PreProgram =
  (Path Abs File, [RawStmt])

data StmtKindF a
  = Normal O.Opacity
  | Data DD.DefiniteDescription [BinderF a] [(DD.DefiniteDescription, [BinderF a], D.Discriminant)]
  | DataIntro DD.DefiniteDescription [BinderF a] [BinderF a] D.Discriminant
  deriving (Generic)

-- opacity for elaboration
toOpacity :: StmtKindF a -> O.Opacity
toOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    _ ->
      O.Transparent

-- opacity for clarification
toLowOpacity :: StmtKindF a -> O.Opacity
toLowOpacity stmtKind =
  case stmtKind of
    Normal opacity ->
      opacity
    Data {} ->
      O.Opaque -- so as not to reduce recursive terms
    _ ->
      O.Transparent

instance Binary a => Binary (StmtKindF a)

data RawStmt
  = RawStmtDefine
      (StmtKindF RT.RawTerm)
      Hint
      DD.DefiniteDescription
      AN.ArgNum
      [BinderF RT.RawTerm]
      RT.RawTerm
      RT.RawTerm
  | RawStmtSection Section.Section [RawStmt]
  | RawStmtDefineResource Hint DD.DefiniteDescription RT.RawTerm RT.RawTerm

data WeakStmt
  = WeakStmtDefine
      (StmtKindF WT.WeakTerm)
      Hint
      DD.DefiniteDescription
      AN.ArgNum
      [BinderF WT.WeakTerm]
      WT.WeakTerm
      WT.WeakTerm
  | WeakStmtDefineResource Hint DD.DefiniteDescription WT.WeakTerm WT.WeakTerm

type Program =
  (Source.Source, [Stmt])

data Stmt
  = StmtDefine
      (StmtKindF TM.Term)
      Hint
      DD.DefiniteDescription
      AN.ArgNum
      [BinderF TM.Term]
      TM.Term
      TM.Term
  | StmtDefineResource Hint DD.DefiniteDescription TM.Term TM.Term
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
    StmtDefineResource {} ->
      stmt

showStmt :: WeakStmt -> T.Text
showStmt stmt =
  case stmt of
    WeakStmtDefine _ m x _ xts codType e ->
      DD.reify x <> "\n" <> WT.toText (m :< WT.Pi xts codType) <> "\n" <> WT.toText (m :< WT.Pi xts e)
    _ ->
      "<define-resource>"

-- defineEnum :: DD.DefiniteDescription -> [BinderF TM.Term] -> [ConsInfo] -> Stmt
-- defineEnum dataName dataArgs consInfoList = do
--   StmtDefine
--     (Data dataName dataArgs consInfoList)
--     internalHint
--     dataName
--     dataArgs
--     (internalHint :< TM.Tau)
--     (internalHint :< TM.Data dataName (map argToTerm dataArgs))

argToTerm :: BinderF TM.Term -> TM.Term
argToTerm (m, x, _) =
  m :< TM.Var x

-- defineEnumIntro ::
--   DD.DefiniteDescription ->
--   [BinderF TM.Term] ->
--   ConsInfo ->
--   Stmt
-- defineEnumIntro dataName dataArgs (consName, consArgs, discriminant) =
--   StmtDefine
--     (DataIntro dataName dataArgs consArgs discriminant)
--     internalHint
--     consName
--     (dataArgs ++ consArgs)
--     (internalHint :< TM.Data dataName (map argToTerm dataArgs))
--     (internalHint :< TM.DataIntro dataName consName discriminant (map argToTerm dataArgs) (map argToTerm consArgs))

addDiscriminants :: [(a, [(b, c)])] -> [(a, [(b, c, D.Discriminant)])]
addDiscriminants info = do
  let (formInfo, introInfo) = unzip info
  zip formInfo $ map (addDiscriminants' D.zero) introInfo

addDiscriminants' :: D.Discriminant -> [(b, c)] -> [(b, c, D.Discriminant)]
addDiscriminants' d xs =
  case xs of
    [] ->
      []
    (x, y) : rest ->
      (x, y, d) : addDiscriminants' (D.increment d) rest
