module Kernel.Parse.Internal.Discern.Data (defineData) where

import Control.Comonad.Cofree hiding (section)
import Data.Maybe
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike
import Language.Common.StmtKind qualified as SK
import Language.RawTerm.Name
import Language.RawTerm.RawBinder
import Language.RawTerm.RawIdent
import Language.RawTerm.RawStmt
import Language.RawTerm.RawTerm qualified as RT
import Logger.Hint
import SyntaxTree.Series qualified as SE

defineData ::
  Hint ->
  DD.DefiniteDescription ->
  Maybe (RT.Args RT.RawType) ->
  [RawConsInfo DD.DefiniteDescription] ->
  Loc ->
  [PostRawStmt]
defineData m dataName dataArgsOrNone consInfoList loc = do
  let dataArgs = modifyDataArgs dataArgsOrNone
  let dataArgs' = fromMaybe RT.emptyArgs dataArgsOrNone
  let consInfoList' = modifyConsInfo D.zero consInfoList
  let stmtKind = SK.Data dataName dataArgs consInfoList'
  let isConstLike = isNothing dataArgsOrNone
  let dataType = constructDataType m dataName isConstLike dataArgs
  let geist =
        RT.RawGeist
          { loc = m,
            name = (dataName, []),
            isConstLike = isConstLike,
            isDestPassing = False,
            impArgs = RT.emptyImpArgs,
            defaultArgs = RT.emptyDefaultArgs,
            expArgs = dataArgs',
            cod = ([], m :< RT.Tau)
          }
  let formRule =
        PostRawStmtDefineType
          []
          stmtKind
          ( RT.RawTypeDef
              { typeGeist = geist,
                typeLeadingComment = [],
                typeBody = dataType,
                typeTrailingComment = [],
                typeEndLoc = loc
              }
          )
  let introRuleList = parseDefineDataConstructor dataType dataName dataArgs' consInfoList D.zero
  formRule : introRuleList

modifyDataArgs :: Maybe (RT.Args RT.RawType) -> [RawBinder RT.RawType]
modifyDataArgs =
  maybe [] RT.extractArgs

modifyConsInfo ::
  D.Discriminant ->
  [RawConsInfo DD.DefiniteDescription] ->
  [DI.StmtConsInfo (RawBinder RT.RawType)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    consInfo : rest -> do
      let expArgsExtracted = maybe [] SE.extract (expArgs consInfo)
      ( SavedHint (loc consInfo),
        DI.ConsInfo
          { DI.consName = name consInfo,
            DI.isConstLike = isConstLikeConsInfo consInfo,
            DI.consArgs = expArgsExtracted,
            DI.discriminant = d
          }
        )
        : modifyConsInfo (D.increment d) rest

parseDefineDataConstructor ::
  RT.RawType ->
  DD.DefiniteDescription ->
  RT.Args RT.RawType ->
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
      let m = loc consInfo
      let attr = AttrDI.Attr {dataName, discriminant, isConstLike = isConstLikeConsInfo consInfo}
      let consType =
            if isConstLikeConsInfo consInfo
              then dataType
              else do
                let emptyImpArgs = (SE.emptySeriesAC, [])
                let expArgsWithEmpty = (fromMaybe SE.emptySeriesPC (expArgs consInfo), [])
                let emptyDefArgs = RT.emptyDefaultBinders
                m :< RT.Pi emptyImpArgs expArgsWithEmpty emptyDefArgs RT.PiDataIntro [] dataType (endLoc consInfo)
      let consBody =
            if isConstLikeConsInfo consInfo
              then m :< RT.DataIntro attr (name consInfo) dataArgs'' (map fst expConsArgs')
              else
                RT.lam (endLoc consInfo) m (map (,[]) expConsArgs) dataType $
                  m :< RT.DataIntro attr (name consInfo) dataArgs'' (map fst expConsArgs')
      let introRule =
            PostRawStmtDefineTerm
              []
              (SK.DataIntro (name consInfo) dataArgs' expConsArgs discriminant)
              ( RT.RawDef
                  { geist =
                      RT.RawGeist
                        { loc = loc consInfo,
                          name = (name consInfo, []),
                          isConstLike = isConstLikeConsInfo consInfo,
                          isDestPassing = False,
                          impArgs = dataArgs,
                          defaultArgs = RT.emptyDefaultArgs,
                          expArgs = (SE.emptySeriesPC, []),
                          cod = ([], consType)
                        },
                    leadingComment = [],
                    endLoc = endLoc consInfo,
                    trailingComment = [],
                    body = consBody
                  }
              )
      let introRuleList = parseDefineDataConstructor dataType dataName dataArgs rest (D.increment discriminant)
      introRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  IsConstLike ->
  [RawBinder RT.RawType] ->
  RT.RawType
constructDataType m dataName isConstLike dataArgs =
  m :< RT.Data (AttrD.Attr {isConstLike}) dataName (map identPlusToVar dataArgs)

identPlusToVar :: RawBinder a -> RT.RawType
identPlusToVar (m, _, x, _, _, _) =
  m :< RT.TyVar (Var x)

adjustExpConsArg :: RawBinder a -> (RT.RawTerm, RawIdent)
adjustExpConsArg (m, _, x, _, _, _) =
  (m :< RT.Var (Var x), x)

isConstLikeConsInfo :: RawConsInfo a -> IsConstLike
isConstLikeConsInfo consInfo =
  isNothing (expArgs consInfo)
