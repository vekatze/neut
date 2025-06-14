module Kernel.Parse.Move.Internal.Discern.Data (defineData) where

import Control.Comonad.Cofree hiding (section)
import Data.Maybe
import Language.Common.Rule.Attr.Data qualified as AttrD
import Language.Common.Rule.Attr.DataIntro qualified as AttrDI
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.IsConstLike
import Language.Common.Rule.StmtKind qualified as SK
import Language.RawTerm.Rule.Name
import Language.RawTerm.Rule.RawBinder
import Language.RawTerm.Rule.RawIdent
import Language.RawTerm.Rule.RawStmt
import Language.RawTerm.Rule.RawTerm qualified as RT
import Logger.Rule.Hint
import SyntaxTree.Rule.Series qualified as SE

defineData ::
  Hint ->
  DD.DefiniteDescription ->
  Maybe (RT.Args RT.RawTerm) ->
  [RawConsInfo DD.DefiniteDescription] ->
  Loc ->
  [PostRawStmt]
defineData m dataName dataArgsOrNone consInfoList loc = do
  let dataArgs = modifyDataArgs dataArgsOrNone
  let dataArgs' = fromMaybe RT.emptyArgs dataArgsOrNone
  let consInfoList' = modifyConsInfo D.zero consInfoList
  let stmtKind = SK.Data dataName dataArgs consInfoList'
  let consNameList = map (\(_, consName, isConstLike, _, _) -> (consName, isConstLike)) consInfoList'
  let isConstLike = isNothing dataArgsOrNone
  let dataType = constructDataType m dataName isConstLike consNameList dataArgs
  let geist =
        RT.RawGeist
          { loc = m,
            name = (dataName, []),
            isConstLike = isConstLike,
            impArgs = RT.emptyImpArgs,
            expArgs = dataArgs',
            cod = ([], m :< RT.Tau)
          }
  let formRule =
        PostRawStmtDefine
          []
          stmtKind
          ( RT.RawDef
              { geist,
                leadingComment = [],
                body = dataType,
                trailingComment = [],
                endLoc = loc
              }
          )
  let introRuleList = parseDefineDataConstructor dataType dataName dataArgs' consInfoList D.zero
  formRule : introRuleList

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
    (m, consName, isConstLike, consArgs, _) : rest ->
      (SavedHint m, consName, isConstLike, SE.extract consArgs, d) : modifyConsInfo (D.increment d) rest

parseDefineDataConstructor ::
  RT.RawTerm ->
  DD.DefiniteDescription ->
  RT.Args RT.RawTerm ->
  [RawConsInfo DD.DefiniteDescription] ->
  D.Discriminant ->
  [PostRawStmt]
parseDefineDataConstructor dataType dataName dataArgs consInfoList discriminant = do
  case consInfoList of
    [] ->
      []
    (m, consName, isConstLike, consArgs, loc) : rest -> do
      let dataArgs' = RT.extractArgs dataArgs
      let consArgs' = SE.extract consArgs
      let dataArgs'' = map identPlusToVar dataArgs'
      let consArgs'' = map adjustConsArg consArgs'
      let consNameList = map (\(_, dd, isConstLike', _, _) -> (dd, isConstLike')) consInfoList
      let geist =
            RT.RawGeist
              { loc = m,
                name = (consName, []),
                isConstLike = isConstLike,
                impArgs = let (series, c) = dataArgs in (fmap (,Nothing) series, c),
                expArgs = (consArgs, []),
                cod = ([], dataType)
              }
      let introRule =
            PostRawStmtDefine
              []
              (SK.DataIntro consName dataArgs' consArgs' discriminant)
              ( RT.RawDef
                  { geist,
                    leadingComment = [],
                    body = m :< RT.DataIntro (AttrDI.Attr {..}) consName dataArgs'' (map fst consArgs''),
                    trailingComment = [],
                    endLoc = loc
                  }
              )
      let introRuleList = parseDefineDataConstructor dataType dataName dataArgs rest (D.increment discriminant)
      introRule : introRuleList

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
