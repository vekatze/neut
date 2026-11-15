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
  Maybe (RT.Params RT.RawType) ->
  [RawConsInfo DD.DefiniteDescription] ->
  Loc ->
  [PostRawStmt]
defineData m dataName dataParamsOrNone consInfoList loc = do
  let dataParams = modifyDataParams dataParamsOrNone
  let dataParams' = fromMaybe RT.emptyParams dataParamsOrNone
  let consInfoList' = modifyConsInfo D.zero consInfoList
  let stmtKind = SK.Data dataName dataParams consInfoList' False
  let isConstLike = isNothing dataParamsOrNone
  let dataType = constructDataType m dataName isConstLike dataParams
  let geist =
        RT.RawGeist
          { loc = m,
            name = (dataName, []),
            isConstLike = isConstLike,
            isDestPassing = False,
            impParams = RT.emptyImpParams,
            defaultParams = RT.emptyDefaultParams,
            expParams = dataParams',
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
  let introRuleList = parseDefineDataConstructor dataType dataName dataParams' consInfoList D.zero
  formRule : introRuleList

modifyDataParams :: Maybe (RT.Params RT.RawType) -> [RawBinder RT.RawType]
modifyDataParams =
  maybe [] RT.extractParams

modifyConsInfo ::
  D.Discriminant ->
  [RawConsInfo DD.DefiniteDescription] ->
  [DI.StmtConsInfo (RawBinder RT.RawType)]
modifyConsInfo d consInfoList =
  case consInfoList of
    [] ->
      []
    consInfo : rest -> do
      let expParamsExtracted = maybe [] SE.extract (expParams consInfo)
      ( SavedHint (loc consInfo),
        DI.ConsInfo
          { DI.consName = name consInfo,
            DI.isConstLike = isConstLikeConsInfo consInfo,
            DI.consImpParams = SE.extract (impParams consInfo),
            DI.consParams = map snd expParamsExtracted,
            DI.consParamHints = map fst expParamsExtracted,
            DI.consParamLayouts = [], -- filled by Elaborate
            DI.discriminant = d
          }
        )
        : modifyConsInfo (D.increment d) rest

parseDefineDataConstructor ::
  RT.RawType ->
  DD.DefiniteDescription ->
  RT.Params RT.RawType ->
  [RawConsInfo DD.DefiniteDescription] ->
  D.Discriminant ->
  [PostRawStmt]
parseDefineDataConstructor dataType dataName dataParams consInfoList discriminant = do
  case consInfoList of
    [] ->
      []
    consInfo : rest -> do
      let dataParams' = RT.extractParams dataParams
      let dataArgs'' = map identPlusToVar dataParams'
      let consImpParams = SE.extract (impParams consInfo)
      let consImpArgs' = map identPlusToVar consImpParams
      let expConsParams = map snd $ maybe [] SE.extract (expParams consInfo)
      let expConsArgs' = map adjustExpConsParam expConsParams
      let m = loc consInfo
      let attr = AttrDI.Attr {dataName, discriminant, isConstLike = isConstLikeConsInfo consInfo}
      let consType =
            if isConstLikeConsInfo consInfo
              then dataType
              else do
                let expParamsWithEmpty = (maybe SE.emptySeriesPC (fmap snd) (expParams consInfo), [])
                m :< RT.Pi RT.emptyImpParams expParamsWithEmpty RT.emptyDefaultParams RT.PiDataIntro [] dataType (endLoc consInfo)
      let consBody =
            if isConstLikeConsInfo consInfo
              then m :< RT.DataIntro attr (name consInfo) dataArgs'' consImpArgs' (map fst expConsArgs')
              else
                RT.lam (endLoc consInfo) m (map (,[]) expConsParams) dataType $
                  m :< RT.DataIntro attr (name consInfo) dataArgs'' consImpArgs' (map fst expConsArgs')
      let introRule =
            PostRawStmtDefineTerm
              []
              (SK.DataIntro (name consInfo) dataParams' consImpParams expConsParams discriminant)
              ( RT.RawDef
                  { geist =
                      RT.RawGeist
                        { loc = loc consInfo,
                          name = (name consInfo, []),
                          isConstLike = isConstLikeConsInfo consInfo,
                          isDestPassing = False,
                          impParams = (SE.appendLeftBiased (fst dataParams) (impParams consInfo), snd dataParams),
                          defaultParams = RT.emptyDefaultParams,
                          expParams = RT.emptyParams,
                          cod = ([], consType)
                        },
                    leadingComment = [],
                    endLoc = endLoc consInfo,
                    trailingComment = [],
                    body = consBody
                  }
              )
      let introRuleList = parseDefineDataConstructor dataType dataName dataParams rest (D.increment discriminant)
      introRule : introRuleList

constructDataType ::
  Hint ->
  DD.DefiniteDescription ->
  IsConstLike ->
  [RawBinder RT.RawType] ->
  RT.RawType
constructDataType m dataName isConstLike dataParams =
  m :< RT.Data (AttrD.Attr {isConstLike}) dataName (map identPlusToVar dataParams)

identPlusToVar :: RawBinder a -> RT.RawType
identPlusToVar (m, _, x, _, _, _) =
  m :< RT.TyVar (Bare x)

adjustExpConsParam :: RawBinder a -> (RT.RawTerm, RawIdent)
adjustExpConsParam (m, _, x, _, _, _) =
  (m :< RT.Var (Bare x), x)

isConstLikeConsInfo :: RawConsInfo a -> IsConstLike
isConstLikeConsInfo consInfo =
  isNothing (expParams consInfo)
