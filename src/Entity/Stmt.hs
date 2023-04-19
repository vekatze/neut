module Entity.Stmt where

import Control.Comonad.Cofree
import Data.Binary
import Data.Set qualified as S
import Data.Text qualified as T
import Entity.ArgNum qualified as AN
import Entity.BaseName qualified as BN
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Error
import Entity.GlobalLocator qualified as GL
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.IsConstLike
import Entity.LocalLocator qualified as LL
import Entity.Opacity qualified as O
import Entity.RawTerm qualified as RT
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
  | Data DD.DefiniteDescription [BinderF a] [(DD.DefiniteDescription, IsConstLike, [BinderF a], D.Discriminant)]
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
      IsConstLike
      (StmtKindF RT.RawTerm)
      Hint
      DD.DefiniteDescription
      AN.ArgNum
      [BinderF RT.RawTerm]
      RT.RawTerm
      RT.RawTerm
  | RawStmtDefineResource Hint DD.DefiniteDescription RT.RawTerm RT.RawTerm
  | RawStmtExport
      Hint
      DD.DefiniteDescription -- alias
      (Either T.Text (GL.GlobalLocator, LL.LocalLocator)) -- original

getBaseName :: Hint -> Either T.Text (GL.GlobalLocator, LL.LocalLocator) -> Either Error BN.BaseName
getBaseName m varOrDefiniteDescription =
  case varOrDefiniteDescription of
    Left var ->
      BN.reflect m var
    Right (_, ll) ->
      Right $ LL.baseName ll

data WeakStmt
  = WeakStmtDefine
      IsConstLike
      (StmtKindF WT.WeakTerm)
      Hint
      DD.DefiniteDescription
      AN.ArgNum
      [BinderF WT.WeakTerm]
      WT.WeakTerm
      WT.WeakTerm
  | WeakStmtDefineResource Hint DD.DefiniteDescription WT.WeakTerm WT.WeakTerm
  | WeakStmtExport Hint DD.DefiniteDescription DD.DefiniteDescription GN.GlobalName

type Program =
  (Source.Source, [Stmt])

data Stmt
  = StmtDefine
      IsConstLike
      (StmtKindF TM.Term)
      Hint
      DD.DefiniteDescription
      AN.ArgNum
      [BinderF TM.Term]
      TM.Term
      TM.Term
  | StmtDefineResource Hint DD.DefiniteDescription TM.Term TM.Term
  | StmtExport Hint DD.DefiniteDescription DD.DefiniteDescription GN.GlobalName
  deriving (Generic)

instance Binary Stmt

type PathSet = S.Set (Path Abs File)

compress :: Stmt -> Stmt
compress stmt =
  case stmt of
    StmtDefine isConstLike stmtKind m functionName impArgNum args codType _ ->
      case stmtKind of
        Normal O.Opaque ->
          StmtDefine isConstLike stmtKind m functionName impArgNum args codType (m :< TM.Tau)
        _ ->
          stmt
    StmtDefineResource {} ->
      stmt
    StmtExport {} ->
      stmt

-- getNameFromStmt :: Stmt -> (DD.DefiniteDescription, Maybe GN.GlobalName)
-- getNameFromStmt stmt =
--   case stmt of
--     StmtDefine _ _ _ functionName _ _ _ _ ->
--       (functionName, Nothing)
--     StmtDefineResource _ resourceName _ _ ->
--       (resourceName, Nothing)
--     StmtExport _ name globalName ->
--       (name, Just globalName)

getNameFromWeakStmt :: WeakStmt -> DD.DefiniteDescription
getNameFromWeakStmt stmt =
  case stmt of
    WeakStmtDefine _ _ _ functionName _ _ _ _ ->
      functionName
    WeakStmtDefineResource _ resourceName _ _ ->
      resourceName
    WeakStmtExport _ aliasName _ _ ->
      aliasName

showStmt :: WeakStmt -> T.Text
showStmt stmt =
  case stmt of
    WeakStmtDefine _ _ m x _ xts codType e ->
      DD.reify x <> "\n" <> WT.toText (m :< WT.Pi xts codType) <> "\n" <> WT.toText (m :< WT.Pi xts e)
    _ ->
      "<define-resource>"

argToTerm :: BinderF TM.Term -> TM.Term
argToTerm (m, x, _) =
  m :< TM.Var x

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
