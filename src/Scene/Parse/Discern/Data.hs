module Scene.Parse.Discern.Data (defineData) where

import Context.App
import Context.Locator qualified as Locator
import Control.Comonad.Cofree hiding (section)
import Data.Maybe
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.IsConstLike
import Entity.Name
import Entity.RawBinder
import Entity.RawDecl qualified as RD
import Entity.RawIdent
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.StmtKind qualified as SK
import Entity.Syntax.Series qualified as SE

defineData ::
  Hint ->
  DD.DefiniteDescription ->
  Maybe (RT.Args RT.RawTerm) ->
  [RawConsInfo BN.BaseName] ->
  App [RawStmt]
defineData m dataName dataArgsOrNone consInfoList = do
  let dataArgs = modifyDataArgs dataArgsOrNone
  let dataArgs' = fromMaybe RT.emptyArgs dataArgsOrNone
  consInfoList' <- mapM modifyConstructorName consInfoList
  let consInfoList'' = modifyConsInfo D.zero consInfoList'
  let stmtKind = SK.Data dataName dataArgs consInfoList''
  let consNameList = map (\(_, consName, isConstLike, _, _) -> (consName, isConstLike)) consInfoList''
  let isConstLike = isNothing dataArgsOrNone
  let dataType = constructDataType m dataName isConstLike consNameList dataArgs
  let decl =
        RD.RawDecl
          { loc = m,
            name = (dataName, []),
            isConstLike = isConstLike,
            impArgs = RT.emptyArgs,
            expArgs = dataArgs',
            cod = ([], (m :< RT.Tau, []))
          }
  let formRule = RawStmtDefine [] stmtKind decl ([], (dataType, []))
  introRuleList <- parseDefineDataConstructor dataType dataName dataArgs' consInfoList' D.zero
  return $ formRule : introRuleList

modifyDataArgs :: Maybe (RT.Args RT.RawTerm) -> [RawBinder RT.RawTerm]
modifyDataArgs =
  maybe [] RT.extractArgs

modifyConsInfo ::
  D.Discriminant ->
  [RawConsInfo DD.DefiniteDescription] ->
  [(SavedHint, DD.DefiniteDescription, IsConstLike, [RawBinder RT.RawTerm], D.Discriminant)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    (m, (consName, _), isConstLike, consArgs) : rest ->
      (SavedHint m, consName, isConstLike, SE.extract consArgs, d) : modifyConsInfo (D.increment d) rest

modifyConstructorName ::
  RawConsInfo BN.BaseName ->
  App (RawConsInfo DD.DefiniteDescription)
modifyConstructorName (mb, (consName, c), isConstLike, yts) = do
  consName' <- Locator.attachCurrentLocator consName
  return (mb, (consName', c), isConstLike, yts)

parseDefineDataConstructor ::
  RT.RawTerm ->
  DD.DefiniteDescription ->
  RT.Args RT.RawTerm ->
  [RawConsInfo DD.DefiniteDescription] ->
  D.Discriminant ->
  App [RawStmt]
parseDefineDataConstructor dataType dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      return []
    (m, (consName, cCons), isConstLike, consArgs) : rest -> do
      let dataArgs' = RT.extractArgs dataArgs
      let consArgs' = SE.extract consArgs
      let dataArgs'' = map identPlusToVar dataArgs'
      let consArgs'' = map adjustConsArg consArgs'
      let consNameList = map (\(_, (dd, _), isConstLike', _) -> (dd, isConstLike')) consInfoList
      let decl =
            RD.RawDecl
              { loc = m,
                name = (consName, cCons),
                isConstLike = isConstLike,
                impArgs = dataArgs,
                expArgs = (consArgs, []),
                cod = ([], (dataType, []))
              }
      let introRule =
            RawStmtDefine
              []
              (SK.DataIntro consName dataArgs' consArgs' discriminant)
              decl
              ([], (m :< RT.DataIntro (AttrDI.Attr {..}) consName dataArgs'' (map fst consArgs''), []))
      introRuleList <- parseDefineDataConstructor dataType dataName dataArgs rest (D.increment discriminant)
      return $ introRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  IsConstLike ->
  [(DD.DefiniteDescription, IsConstLike)] ->
  [RawBinder RT.RawTerm] ->
  RT.RawTerm
constructDataType m dataName isConstLike consNameList dataArgs = do
  m :< RT.Data (AttrD.Attr {..}) dataName (map identPlusToVar dataArgs)

identPlusToVar :: RawBinder a -> RT.RawTerm
identPlusToVar (m, x, _, _, _) =
  m :< RT.Var (Var x)

adjustConsArg :: RawBinder a -> (RT.RawTerm, RawIdent)
adjustConsArg (m, x, _, _, _) =
  (m :< RT.Var (Var x), x)
