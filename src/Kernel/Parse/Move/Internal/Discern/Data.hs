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
  let consNameList = map (\consInfo -> (name consInfo, isConstLikeConsInfo consInfo)) consInfoList
  let consInfoList' = modifyConsInfo D.zero consInfoList
  let stmtKind = SK.Data dataName dataArgs consInfoList'
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
    consInfo : rest -> do
      let expArgsExtracted = maybe [] SE.extract (expArgs consInfo)
      (SavedHint (loc consInfo), name consInfo, isConstLikeConsInfo consInfo, expArgsExtracted, d) : modifyConsInfo (D.increment d) rest

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
    consInfo : rest -> do
      let dataArgs' = RT.extractArgs dataArgs
      let dataArgs'' = map identPlusToVar dataArgs'
      let expConsArgs = maybe [] SE.extract (expArgs consInfo)
      let expConsArgs' = map adjustExpConsArg expConsArgs
      let consNameList = map (\ci -> (name ci, isConstLikeConsInfo ci)) consInfoList
      let m = loc consInfo
      let attr = AttrDI.Attr {dataName, consNameList, discriminant, isConstLike = isConstLikeConsInfo consInfo}
      let dataImpArgs = do
            let (series, c) = dataArgs
            (fmap (,Nothing) series, c)
      let consType =
            if isConstLikeConsInfo consInfo
              then dataType
              else do
                let impArgsWithEmpty = (SE.emptySeriesAC, [])
                let expArgsWithEmpty = (fromMaybe SE.emptySeriesPC (expArgs consInfo), [])
                m :< RT.Pi impArgsWithEmpty expArgsWithEmpty [] dataType (endLoc consInfo)
      let body =
            if isConstLikeConsInfo consInfo
              then m :< RT.DataIntro attr (name consInfo) dataArgs'' (map fst expConsArgs')
              else
                RT.genLam (endLoc consInfo) m [] (map (,[]) expConsArgs) dataType $
                  m :< RT.DataIntro attr (name consInfo) dataArgs'' (map fst expConsArgs')
      let introRule =
            PostRawStmtDefine
              []
              (SK.DataIntro (name consInfo) dataArgs' expConsArgs discriminant)
              ( RT.RawDef
                  { geist =
                      RT.RawGeist
                        { loc = loc consInfo,
                          name = (name consInfo, []),
                          isConstLike = isConstLikeConsInfo consInfo,
                          impArgs = dataImpArgs,
                          expArgs = (SE.emptySeriesPC, []),
                          cod = ([], consType)
                        },
                    leadingComment = [],
                    endLoc = endLoc consInfo,
                    trailingComment = [],
                    body = body
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

adjustExpConsArg :: RawBinder a -> (RT.RawTerm, RawIdent)
adjustExpConsArg (m, x, _, _, _) =
  (m :< RT.Var (Var x), x)

isConstLikeConsInfo :: RawConsInfo a -> IsConstLike
isConstLikeConsInfo consInfo =
  isNothing (expArgs consInfo)
