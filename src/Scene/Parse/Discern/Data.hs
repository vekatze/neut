module Scene.Parse.Discern.Data (defineData) where

import Context.App
import Context.Locator qualified as Locator
import Control.Comonad.Cofree hiding (section)
import Data.Maybe
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.IsConstLike
import Entity.Name
import Entity.RawBinder
import Entity.RawDecl qualified as RD
import Entity.RawIdent
import Entity.RawTerm qualified as RT
import Entity.Stmt
import Entity.StmtKind qualified as SK

defineData ::
  Hint ->
  DD.DefiniteDescription ->
  Maybe RD.ExpArgs ->
  [RawConsInfo] ->
  App [RawStmt]
defineData m dataName dataArgsOrNone consInfoList = do
  let dataArgs = modifyDataArgs dataArgsOrNone
  let dataArgs' = fromMaybe (Nothing, ([], [])) dataArgsOrNone
  consInfoList' <- mapM modifyConstructorName consInfoList
  let consInfoList'' = modifyConsInfo D.zero consInfoList'
  let stmtKind = SK.Data dataName dataArgs consInfoList''
  let consNameList = map (\(_, consName, isConstLike, _, _) -> (consName, isConstLike)) consInfoList''
  let isConstLike = isNothing dataArgsOrNone
  let dataType = constructDataType m dataName isConstLike consNameList dataArgs
  let formRule =
        RawStmtDefine
          []
          isConstLike
          stmtKind
          m
          (dataName, [])
          ([], [])
          dataArgs'
          ([], (m :< RT.Tau, []))
          ([], (dataType, []))
  introRuleList <- parseDefineDataConstructor dataType dataName dataArgs' consInfoList' D.zero
  return $ formRule : introRuleList

modifyDataArgs :: Maybe RD.ExpArgs -> [RawBinder RT.RawTerm]
modifyDataArgs =
  maybe [] stripComments

modifyConsInfo ::
  D.Discriminant ->
  [(Hint, DD.DefiniteDescription, b, RD.ExpArgs)] ->
  [(SavedHint, DD.DefiniteDescription, b, [RawBinder RT.RawTerm], D.Discriminant)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    (m, consName, isConstLike, consArgs) : rest ->
      (SavedHint m, consName, isConstLike, stripComments consArgs, d) : modifyConsInfo (D.increment d) rest

modifyConstructorName ::
  RawConsInfo ->
  App (Hint, DD.DefiniteDescription, IsConstLike, RD.ExpArgs)
modifyConstructorName (mb, consName, isConstLike, yts) = do
  consName' <- Locator.attachCurrentLocator consName
  return (mb, consName', isConstLike, yts)

parseDefineDataConstructor ::
  RT.RawTerm ->
  DD.DefiniteDescription ->
  RD.ExpArgs ->
  [(Hint, DD.DefiniteDescription, IsConstLike, RD.ExpArgs)] ->
  D.Discriminant ->
  App [RawStmt]
parseDefineDataConstructor dataType dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      return []
    (m, consName, isConstLike, consArgs) : rest -> do
      let dataArgs' = stripComments dataArgs
      let consArgs' = stripComments consArgs
      let dataArgs'' = map identPlusToVar dataArgs'
      let consArgs'' = map adjustConsArg consArgs'
      let consNameList = map (\(_, c, isConstLike', _) -> (c, isConstLike')) consInfoList
      let introRule =
            RawStmtDefine
              []
              isConstLike
              (SK.DataIntro consName dataArgs' consArgs' discriminant)
              m
              (consName, [])
              (snd dataArgs)
              consArgs
              ([], (dataType, []))
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

stripComments :: RD.ExpArgs -> [RawBinder RT.RawTerm]
stripComments (_, (args, _)) =
  map ((\(m, x, c1, c2, (t, _)) -> (m, x, c1, c2, t)) . snd) args
