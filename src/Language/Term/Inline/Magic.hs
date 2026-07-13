module Language.Term.Inline.Magic
  ( evaluateInspectType,
    evaluateEqType,
    evaluateShowType,
    evaluateTextCons,
    evaluateTextUncons,
    evaluateMakeSwitch,
    evaluateCompileError,
    evaluateGetOriginFileName,
    evaluateGetOriginLine,
    evaluateGetOriginColumn,
    constructUnitTerm,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.IORef (readIORef)
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Module qualified as Module
import Kernel.Common.TypeTag qualified as TypeTag
import Kernel.Common.TypeValue qualified as TypeValue
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseName qualified as BN
import Language.Common.Binder (BinderF)
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DataInfo qualified as DI
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.Literal qualified as L
import Language.Common.ModuleID qualified as MID
import Language.Common.PiElimKind qualified as PEK
import Language.Common.PrimNumSize (dataSizeToIntSize)
import Language.Common.PrimType qualified as PT
import Language.Common.Rune qualified as Rune
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
import Language.Term.Eq qualified as TermEq
import Language.Term.Inline.Handle
import Language.Term.PrimValue qualified as PV
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Language.Term.Weaken (weakenType)
import Language.WeakTerm.ToText (toTextType)
import Logger.Hint (Hint (..), showFilePosRelative)
import Path (toFilePath)

evaluateInspectType :: Handle -> Hint -> MID.ModuleID -> TM.Type -> App TM.Term
evaluateInspectType h m moduleID typeExpr = do
  case typeExpr of
    _ :< TM.Tau ->
      returnTypeValueIntValue h m moduleID TypeValue.Type
    _ :< TM.Pi {} ->
      returnTypeValueIntValue h m moduleID TypeValue.Function
    _ :< TM.Data _ dataName dataArgs -> do
      DI.DataInfo {DI.dataArgs = dataArgBinders, DI.consInfoList = consInfoListRaw} <- lookupDataInfo h m dataName
      consInfoList <- specializeConsInfoList h m dataName dataArgBinders dataArgs consInfoListRaw
      case consInfoList of
        [DI.ConsInfo {DI.consArgs = [(_, _, _, arg)]}] -> do
          if dataName == makeVectorDD moduleID
            then do
              case dataArgs of
                [dataArg] ->
                  returnTypeValueIntValue h m moduleID $ TypeValue.Vector dataArg
                _ -> do
                  let len = length dataArgs
                  reportMacroError h m $
                    "inspect-type: `vector` expects 1 argument, but got " <> T.pack (show len) <> " arguments."
            else do
              if dataName == makeArrayDD moduleID
                then do
                  case dataArgs of
                    [dataArg] ->
                      returnTypeValueIntValue h m moduleID $ TypeValue.Array dataArg
                    _ -> do
                      let len = length dataArgs
                      reportMacroError h m $
                        "inspect-type: `array` expects 1 argument, but got " <> T.pack (show len) <> " arguments."
                else returnTypeValueIntValue h m moduleID $ TypeValue.Wrapper arg
        _ -> do
          let consInfoList' = map consToTypeValue consInfoList
          let isEnum = all (\consInfo -> DI.isConstLike consInfo && null (DI.consArgs consInfo)) consInfoList
          if isEnum && not (null consInfoList)
            then do
              let enumConsNames = map (DD.localLocator . DI.consName) consInfoList
              returnTypeValueIntValue h m moduleID $ TypeValue.Enum enumConsNames
            else do
              let dataNameText = DD.localLocator dataName
              returnTypeValueIntValue h m moduleID $ TypeValue.Algebraic dataNameText dataArgs consInfoList'
    _ :< TM.BoxNoema t ->
      returnTypeValueIntValue h m moduleID $ TypeValue.Noema t
    _ :< TM.Box t ->
      returnTypeValueIntValue h m moduleID $ TypeValue.BoxT t
    _ :< TM.Code _ ->
      returnTypeValueIntValue h m moduleID TypeValue.Opaque
    _ :< TM.PrimType (PT.Int size) ->
      returnTypeValueIntValue h m moduleID (TypeValue.fromIntSize size)
    _ :< TM.PrimType (PT.Float size) ->
      returnTypeValueIntValue h m moduleID (TypeValue.fromFloatSize size)
    _ :< TM.PrimType PT.Text ->
      returnTypeValueIntValue h m moduleID TypeValue.Opaque
    _ :< TM.PrimType PT.Blob ->
      returnTypeValueIntValue h m moduleID TypeValue.Opaque
    _ :< TM.PrimType PT.Pointer ->
      returnTypeValueIntValue h m moduleID TypeValue.Pointer
    _ :< TM.PrimType PT.Rune ->
      returnTypeValueIntValue h m moduleID TypeValue.Rune
    _ :< TM.Resource name _ -> do
      let binarySGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.binaryLocator}
      let binaryDD = DD.newByGlobalLocator binarySGL BN.binary
      if name == binaryDD
        then returnTypeValueIntValue h m moduleID TypeValue.Binary
        else returnTypeValueIntValue h m moduleID TypeValue.Opaque
    _ :< TM.TVarGlobal _ _ -> do
      returnTypeValueIntValue h m moduleID TypeValue.Opaque
    _ :< TM.TyApp (_ :< TM.TVarGlobal _ _) _ -> do
      returnTypeValueIntValue h m moduleID TypeValue.Opaque
    _ -> do
      reportMacroError h m $
        "inspect-type: unable to determine type value for this type expression. Got: "
          <> toTextType (weakenType typeExpr)

evaluateEqType :: Hint -> MID.ModuleID -> TM.Type -> TM.Type -> App TM.Term
evaluateEqType m moduleID typeExpr1 typeExpr2 = do
  let isEqual = TermEq.eqType typeExpr1 typeExpr2
  return $ constructBoolTerm m moduleID isEqual

constructUnitTerm :: Hint -> MID.ModuleID -> TM.Term
constructUnitTerm m moduleID = do
  let unitSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.unitLocator}
  let unitTypeDD = DD.newByGlobalLocator unitSGL BN.unitType
  let unitDD = DD.newByGlobalLocator unitSGL BN.unit
  let attr = AttrDI.Attr {dataName = unitTypeDD, discriminant = D.zero, isConstLike = True}
  m :< TM.DataIntro attr unitDD [] []

makeVectorDD :: MID.ModuleID -> DD.DefiniteDescription
makeVectorDD moduleID = do
  let vectorSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.vectorLocator}
  DD.newByGlobalLocator vectorSGL BN.vector

makeArrayDD :: MID.ModuleID -> DD.DefiniteDescription
makeArrayDD moduleID = do
  let arraySGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.arrayLocator}
  DD.newByGlobalLocator arraySGL BN.array

consToTypeValue ::
  DI.ConsInfo (BinderF TM.Type) ->
  TypeValue.Constructor
consToTypeValue consInfo = do
  let params = zipWith (\(_, _, x, t) layout -> (Ident.toText x, t, layout)) (DI.consArgs consInfo) (DI.consArgLayouts consInfo)
  (DD.localLocator (DI.consName consInfo), DI.isConstLike consInfo, params)

isConstTypeTag :: TypeTag.TypeTag -> Bool
isConstTypeTag tt =
  case tt of
    TypeTag.Vector ->
      False
    TypeTag.Array ->
      False
    TypeTag.Algebraic ->
      False
    TypeTag.Wrapper ->
      False
    TypeTag.Noema ->
      False
    TypeTag.Enum ->
      False
    TypeTag.BoxT ->
      False
    _ ->
      True

makeAttrDI ::
  SGL.StrictGlobalLocator ->
  TypeTag.TypeTag ->
  App AttrDI.Attr
makeAttrDI typeValueSGL typeTag = do
  let dataName = DD.newByGlobalLocator typeValueSGL BN.typeValue
  let discriminant = D.MakeDiscriminant $ TypeTag.typeTagToInteger typeTag
  return $ AttrDI.Attr {dataName, discriminant, isConstLike = isConstTypeTag typeTag}

returnTypeValueIntValue :: Handle -> Hint -> MID.ModuleID -> TypeValue.TypeValue -> App TM.Term
returnTypeValueIntValue h m moduleID typeValue = do
  let typeValueSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.typeValueLocator}
  attr <- makeAttrDI typeValueSGL $ TypeValue.toTypeTag typeValue
  let tag = TypeValue.toTypeTag typeValue
  let consName = DD.newByGlobalLocator typeValueSGL (BN.fromTypeTag tag)
  case typeValue of
    TypeValue.Algebraic dataName dataArgs consInfoList -> do
      let listSgl = makeListSGL moduleID
      let dataNameTerm = m :< TM.Prim (PV.Text dataName)
      dataArgsTerm <- constructListTerm m listSgl dataArgs
      consInfoTerm <- constructConstructorInfoListTerm h m moduleID consInfoList
      return $ m :< TM.DataIntro attr consName [] [dataNameTerm, dataArgsTerm, consInfoTerm]
    TypeValue.Enum consNames -> do
      let listSgl = makeListSGL moduleID
      let elemType = m :< TM.PrimType PT.Text
      let nameTerms = map (\name -> m :< TM.Prim (PV.Text name)) consNames
      namesListTerm <- constructListTermFromTerms m listSgl elemType nameTerms
      return $ m :< TM.DataIntro attr consName [] [namesListTerm]
    TypeValue.Vector t -> do
      return $ m :< TM.DataIntro attr consName [] [m :< TM.TauIntro t]
    TypeValue.Array t -> do
      return $ m :< TM.DataIntro attr consName [] [m :< TM.TauIntro t]
    TypeValue.Wrapper t -> do
      return $ m :< TM.DataIntro attr consName [] [m :< TM.TauIntro t]
    TypeValue.Noema t -> do
      return $ m :< TM.DataIntro attr consName [] [m :< TM.TauIntro t]
    TypeValue.BoxT t -> do
      return $ m :< TM.DataIntro attr consName [] [m :< TM.TauIntro t]
    _ ->
      return $ m :< TM.DataIntro attr consName [] []

constructListTerm :: Hint -> SGL.StrictGlobalLocator -> [TM.Type] -> App TM.Term
constructListTerm m listSgl types = do
  let wrappedTypes = map (\ty -> m :< TM.TauIntro ty) types
  constructListTermFromTerms m listSgl (m :< TM.Tau) wrappedTypes

constructListTermFromTerms :: Hint -> SGL.StrictGlobalLocator -> TM.Type -> [TM.Term] -> App TM.Term
constructListTermFromTerms hint listSgl elemType terms =
  case terms of
    [] ->
      constructListNilTerm hint listSgl elemType
    headTerm : rest -> do
      tailList <- constructListTermFromTerms hint listSgl elemType rest
      constructListConsTerm hint listSgl elemType headTerm tailList

constructListNilTerm :: Hint -> SGL.StrictGlobalLocator -> TM.Type -> App TM.Term
constructListNilTerm hint listSgl elemType = do
  let dataName = coreListType listSgl
  let attr = AttrDI.Attr {dataName, discriminant = D.zero, isConstLike = True}
  return $ hint :< TM.DataIntro attr (coreListNil listSgl) [elemType] []

constructListConsTerm :: Hint -> SGL.StrictGlobalLocator -> TM.Type -> TM.Term -> TM.Term -> App TM.Term
constructListConsTerm hint listSgl elemType headTerm tailList = do
  let dataName = coreListType listSgl
  let attr = AttrDI.Attr {dataName, discriminant = D.increment D.zero, isConstLike = False}
  return $ hint :< TM.DataIntro attr (coreListCons listSgl) [elemType] [headTerm, tailList]

coreListType :: SGL.StrictGlobalLocator -> DD.DefiniteDescription
coreListType sglList =
  DD.newByGlobalLocator sglList BN.list

coreListNil :: SGL.StrictGlobalLocator -> DD.DefiniteDescription
coreListNil sglList =
  DD.newByGlobalLocator sglList BN.nil

coreListCons :: SGL.StrictGlobalLocator -> DD.DefiniteDescription
coreListCons sglList =
  DD.newByGlobalLocator sglList BN.consName

makeListSGL :: MID.ModuleID -> SGL.StrictGlobalLocator
makeListSGL moduleID =
  SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.listLocator}

makeConstructorSGL :: MID.ModuleID -> SGL.StrictGlobalLocator
makeConstructorSGL moduleID =
  SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.typeValueLocator}

makeConstructorTypeExpr :: Hint -> MID.ModuleID -> TM.Type
makeConstructorTypeExpr m moduleID = do
  let constructorSgl = makeConstructorSGL moduleID
  let constructorTypeDD = DD.newByGlobalLocator constructorSgl BN.constructorType
  m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True, isDestPassing = False}) constructorTypeDD

makeFieldTypeExpr :: Hint -> MID.ModuleID -> TM.Type
makeFieldTypeExpr m moduleID = do
  let constructorSgl = makeConstructorSGL moduleID
  let fieldTypeDD = DD.newByGlobalLocator constructorSgl BN.fieldType
  m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True, isDestPassing = False}) fieldTypeDD

constructBoolTerm :: Hint -> MID.ModuleID -> IsConstLike -> TM.Term
constructBoolTerm hint moduleID value = do
  let boolSgl = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.boolLocator}
  let boolTypeDD = DD.newByGlobalLocator boolSgl BN.boolType
  let trueDD = DD.newByGlobalLocator boolSgl BN.trueConstructor
  let falseDD = DD.newByGlobalLocator boolSgl BN.falseConstructor
  let (consName, discriminant) =
        if value
          then (trueDD, D.increment D.zero)
          else (falseDD, D.zero)
  let attr = AttrDI.Attr {dataName = boolTypeDD, discriminant, isConstLike = True}
  hint :< TM.DataIntro attr consName [] []

constructConstructorInfoListTerm ::
  Handle ->
  Hint ->
  MID.ModuleID ->
  [TypeValue.Constructor] ->
  App TM.Term
constructConstructorInfoListTerm h hint moduleID consInfoList = do
  let listSgl = makeListSGL moduleID
  let constructorType = makeConstructorTypeExpr hint moduleID
  consInfoTerms <- mapM (constructConstructorTerm h hint moduleID) consInfoList
  constructListTermFromTerms hint listSgl constructorType consInfoTerms

constructConstructorTerm ::
  Handle ->
  Hint ->
  MID.ModuleID ->
  TypeValue.Constructor ->
  App TM.Term
constructConstructorTerm h m moduleID (consName, isConstLike, params) = do
  let constructorSgl = makeConstructorSGL moduleID
  let listSgl = makeListSGL moduleID
  let constructorTypeDD = DD.newByGlobalLocator constructorSgl BN.constructorType
  let consDD = DD.newByGlobalLocator constructorSgl BN.constructor
  let fieldType = makeFieldTypeExpr m moduleID
  let attr = AttrDI.Attr {dataName = constructorTypeDD, discriminant = D.zero, isConstLike = False}
  let consNameText = m :< TM.Prim (PV.Text consName)
  fieldTerms <- mapM (constructFieldTerm h m moduleID) params
  paramListTerm <- constructListTermFromTerms m listSgl fieldType fieldTerms
  let boolTerm = constructBoolTerm m moduleID isConstLike
  return $ m :< TM.DataIntro attr consDD [] [consNameText, boolTerm, paramListTerm]

lookupDataInfo :: Handle -> Hint -> DD.DefiniteDescription -> App (DI.DataInfo (BinderF TM.Type))
lookupDataInfo h m dataName = do
  dataInfoOrNone <- liftIO $ Data.lookup (dataHandle h) dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return dataInfo
    Nothing ->
      reportMacroError h m $ "inspect-type: constructor metadata for `" <> ModulePath.renderDD (modulePathMap h) dataName <> "` is unavailable."

specializeConsInfoList ::
  Handle ->
  Hint ->
  DD.DefiniteDescription ->
  [BinderF TM.Type] ->
  [TM.Type] ->
  [DI.ConsInfo (BinderF TM.Type)] ->
  App [DI.ConsInfo (BinderF TM.Type)]
specializeConsInfoList h m dataName dataBinders dataArgs consInfoList
  | length dataBinders == length dataArgs = do
      let binderIds = map (\(_, _, x, _) -> x) dataBinders
      let sub = IntMap.fromList $ zip (map Ident.toInt binderIds) (map Subst.Type dataArgs)
      mapM (specializeConsInfo h sub) consInfoList
  | otherwise =
      reportMacroError h m $
        "inspect-type: arity mismatch while specializing constructor metadata for `"
          <> ModulePath.renderDD (modulePathMap h) dataName
          <> "`."

specializeConsInfo :: Handle -> Subst.Subst -> DI.ConsInfo (BinderF TM.Type) -> App (DI.ConsInfo (BinderF TM.Type))
specializeConsInfo h sub consInfo = do
  let specializeConsArg (mx, k, x, t) = do
        t' <- liftIO $ Subst.substType (substHandle h) sub t
        return (mx, k, x, t')
  consArgs' <- mapM specializeConsArg (DI.consArgs consInfo)
  return $ consInfo {DI.consArgs = consArgs'}

constructFieldTerm :: Handle -> Hint -> MID.ModuleID -> TypeValue.Field -> App TM.Term
constructFieldTerm h m moduleID (paramName, paramType, layout) = do
  let constructorSgl = makeConstructorSGL moduleID
  let fieldTypeDD = DD.newByGlobalLocator constructorSgl BN.fieldType
  let fieldDD = DD.newByGlobalLocator constructorSgl BN.field
  let nameTerm = m :< TM.Prim (PV.Text paramName)
  let typeTerm = m :< TM.TauIntro paramType
  let attr = AttrDI.Attr {dataName = fieldTypeDD, discriminant = D.zero, isConstLike = False}
  layoutTerm <- constructFieldLayoutTerm h m moduleID layout
  return $ m :< TM.DataIntro attr fieldDD [] [nameTerm, typeTerm, layoutTerm]

constructFieldLayoutTerm :: Handle -> Hint -> MID.ModuleID -> DI.FieldLayout -> App TM.Term
constructFieldLayoutTerm h m moduleID layout = do
  let constructorSgl = makeConstructorSGL moduleID
  let fieldLayoutTypeDD = DD.newByGlobalLocator constructorSgl BN.fieldLayout
  case layout of
    DI.LayoutDirect -> do
      let directDD = DD.newByGlobalLocator constructorSgl BN.direct
      let attr = AttrDI.Attr {dataName = fieldLayoutTypeDD, discriminant = D.zero, isConstLike = True}
      return $ m :< TM.DataIntro attr directDD [] []
    DI.LayoutFlattened slotCount -> do
      let mixedDD = DD.newByGlobalLocator constructorSgl BN.mixed
      let attr = AttrDI.Attr {dataName = fieldLayoutTypeDD, discriminant = D.increment D.zero, isConstLike = False}
      let slotCountTerm = constructIntTerm h m slotCount
      return $ m :< TM.DataIntro attr mixedDD [] [slotCountTerm]

constructIntTerm :: Handle -> Hint -> Int -> TM.Term
constructIntTerm h m value = do
  let intSize = dataSizeToIntSize (baseSize h)
  let intType = m :< TM.PrimType (PT.Int intSize)
  m :< TM.Prim (PV.Int intType intSize (toInteger value))

evaluateShowType :: Hint -> TM.Type -> App TM.Term
evaluateShowType m typeExpr = do
  let typeText = toTextType $ weakenType typeExpr
  return $ m :< TM.Prim (PV.Text typeText)

evaluateTextCons :: Handle -> Hint -> TM.Term -> TM.Term -> App TM.Term
evaluateTextCons h m rune text = do
  case (rune, text) of
    (_ :< TM.Prim (PV.Rune r), _ :< TM.Prim (PV.Text textValue)) -> do
      let newText = T.cons (Rune.asChar r) textValue
      return $ m :< TM.Prim (PV.Text newText)
    _ ->
      reportMacroError h m "text-cons requires a rune literal and a static text literal"

evaluateTextUncons :: Handle -> Hint -> MID.ModuleID -> TM.Term -> App TM.Term
evaluateTextUncons h m moduleID text = do
  case text of
    _ :< TM.Prim (PV.Text textValue) -> do
      let eitherSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.eitherLocator}
      let unitSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.unitLocator}
      let pairSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.pairLocator}
      let unitTypeDD = DD.newByGlobalLocator unitSGL BN.unitType
      let pairTypeDD = DD.newByGlobalLocator pairSGL BN.pairType
      let unitTypeVar = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True, isDestPassing = False}) unitTypeDD
      let pairTypeVar = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.fromInt 2, isConstLike = False, isDestPassing = False}) pairTypeDD
      let runeType = m :< TM.PrimType PT.Rune
      let textType = m :< TM.PrimType PT.Text
      let pairType = m :< TM.TyApp pairTypeVar [runeType, textType]
      case T.uncons textValue of
        Nothing -> do
          let leftDD = DD.newByGlobalLocator eitherSGL BN.left
          let unitDD = DD.newByGlobalLocator unitSGL BN.unit
          let leftVar = m :< TM.VarGlobal (AttrVG.Attr {argNum = AN.fromInt 3, isConstLike = False, isDestPassing = False}) leftDD
          let unitVar = m :< TM.VarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True, isDestPassing = False}) unitDD
          return $ m :< TM.PiElim PEK.Normal (m :< TM.PiElim PEK.Normal leftVar [unitTypeVar, pairType] [] []) [] [unitVar] []
        Just (c, rest) -> do
          let rightDD = DD.newByGlobalLocator eitherSGL BN.right
          let pairDD = DD.newByGlobalLocator pairSGL BN.pair
          let rightVar = m :< TM.VarGlobal (AttrVG.Attr {argNum = AN.fromInt 3, isConstLike = False, isDestPassing = False}) rightDD
          let pairVar = m :< TM.VarGlobal (AttrVG.Attr {argNum = AN.fromInt 4, isConstLike = False, isDestPassing = False}) pairDD
          let runeValue = m :< TM.Prim (PV.Rune (Rune.fromChar c))
          let restText = m :< TM.Prim (PV.Text rest)
          let pair = m :< TM.PiElim PEK.Normal (m :< TM.PiElim PEK.Normal pairVar [runeType, textType] [] []) [] [runeValue, restText] []
          return $ m :< TM.PiElim PEK.Normal (m :< TM.PiElim PEK.Normal rightVar [unitTypeVar, pairType] [] []) [] [pair] []
    _ ->
      reportMacroError h m "text-uncons requires a static text literal"

data SwitchClause = SwitchClause Integer TM.Term

evaluateMakeSwitch :: Handle -> Hint -> MID.ModuleID -> TM.Term -> TM.Term -> TM.Term -> App TM.Term
evaluateMakeSwitch h m moduleID key fallback clausesTerm = do
  clauses <- collectSwitchClauses h m moduleID clausesTerm
  case clauses of
    [] ->
      return fallback
    _ -> do
      case asCodeIntTerm key of
        Just keyValue -> do
          return $ selectSwitchClause keyValue fallback clauses
        Nothing -> do
          cursor <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "switch-key"
          let intType = m :< TM.PrimType (PT.Int (dataSizeToIntSize (baseSize h)))
          let fallbackTree = makeSwitchLeaf m fallback
          let caseList = map (makeSwitchClause m) clauses
          let tree = DT.Switch (cursor, intType) (fallbackTree, caseList)
          let keyElim = m :< TM.CodeElim key
          return $ m :< TM.CodeIntro (m :< TM.DataElim False [(cursor, keyElim, intType)] tree)

collectSwitchClauses :: Handle -> Hint -> MID.ModuleID -> TM.Term -> App [SwitchClause]
collectSwitchClauses h m moduleID term = do
  case term of
    _ :< TM.DataIntro _ consName _ []
      | consName == coreListNilByModule moduleID ->
          return []
    _ :< TM.DataIntro _ consName _ [clauseTerm, rest]
      | consName == coreListConsByModule moduleID -> do
          clause <- destructSwitchClause h m moduleID clauseTerm
          restClauses <- collectSwitchClauses h m moduleID rest
          return $ clause : restClauses
    _ ->
      reportMacroError h m "make-switch requires clauses to reduce to a static list of `Pair(int, code)` values"

destructSwitchClause :: Handle -> Hint -> MID.ModuleID -> TM.Term -> App SwitchClause
destructSwitchClause h m moduleID term = do
  case term of
    _ :< TM.DataIntro _ consName _ [key, body]
      | consName == corePairByModule moduleID -> do
          case asIntTerm key of
            Just keyValue ->
              return $ SwitchClause keyValue body
            Nothing ->
              reportMacroError h m "make-switch requires every clause key to reduce to a static integer literal"
    _ ->
      reportMacroError h m "make-switch requires every clause to reduce to `Pair(int, code)`"

selectSwitchClause :: Integer -> TM.Term -> [SwitchClause] -> TM.Term
selectSwitchClause keyValue fallback clauses =
  case clauses of
    [] ->
      fallback
    SwitchClause key body : rest -> do
      if key == keyValue
        then body
        else selectSwitchClause keyValue fallback rest

makeSwitchLeaf :: Hint -> TM.Term -> DT.DecisionTree TM.Type TM.Term
makeSwitchLeaf m body =
  DT.Leaf [] [] (m :< TM.CodeElim body)

makeSwitchClause :: Hint -> SwitchClause -> DT.Case TM.Type TM.Term
makeSwitchClause m (SwitchClause key body) =
  DT.LiteralCase m (L.Int key) (makeSwitchLeaf m body)

asCodeIntTerm :: TM.Term -> Maybe Integer
asCodeIntTerm term =
  case term of
    _ :< TM.CodeIntro body ->
      asIntTerm body
    _ ->
      Nothing

asIntTerm :: TM.Term -> Maybe Integer
asIntTerm term =
  case term of
    _ :< TM.Prim (PV.Int _ _ value) ->
      Just value
    _ ->
      Nothing

coreListNilByModule :: MID.ModuleID -> DD.DefiniteDescription
coreListNilByModule moduleID =
  DD.newByGlobalLocator (coreSGL moduleID SL.listLocator) BN.nil

coreListConsByModule :: MID.ModuleID -> DD.DefiniteDescription
coreListConsByModule moduleID =
  DD.newByGlobalLocator (coreSGL moduleID SL.listLocator) BN.consName

corePairByModule :: MID.ModuleID -> DD.DefiniteDescription
corePairByModule moduleID =
  DD.newByGlobalLocator (coreSGL moduleID SL.pairLocator) BN.pair

coreSGL :: MID.ModuleID -> SL.SourceLocator -> SGL.StrictGlobalLocator
coreSGL moduleID sourceLocator =
  SGL.StrictGlobalLocator {moduleID, sourceLocator}

evaluateCompileError :: Handle -> Hint -> TM.Term -> App a
evaluateCompileError h m msg = do
  case msg of
    _ :< TM.Prim (PV.Text messageText) -> do
      reportMacroError h m messageText
    _ ->
      raiseError m "compile-error requires a static text message"

evaluateGetOriginFileName :: Handle -> Hint -> App TM.Term
evaluateGetOriginFileName h m = do
  origin <- getOriginHint h m "get-origin-file-name"
  return $ m :< TM.Prim (PV.Text (T.pack $ metaFileName origin))

evaluateGetOriginLine :: Handle -> Hint -> App TM.Term
evaluateGetOriginLine h m = do
  origin <- getOriginHint h m "get-origin-line"
  let (line, _) = metaLocation origin
  returnOriginInt h m line

evaluateGetOriginColumn :: Handle -> Hint -> App TM.Term
evaluateGetOriginColumn h m = do
  origin <- getOriginHint h m "get-origin-column"
  let (_, column) = metaLocation origin
  returnOriginInt h m column

getOriginHint :: Handle -> Hint -> T.Text -> App Hint
getOriginHint h m magicName = do
  when (insideDefineMeta h) $ do
    reportDefineMetaError h m $ magicName <> " cannot be used while evaluating define-meta"
  stack <- liftIO $ getMacroCallStack h
  case reverse stack of
    (_, _, origin) : _ ->
      return origin
    [] ->
      return m

returnOriginInt :: Handle -> Hint -> Int -> App TM.Term
returnOriginInt h m value = do
  let intSize = dataSizeToIntSize (baseSize h)
  let intType = m :< TM.PrimType (PT.Int intSize)
  return $ m :< TM.Prim (PV.Int intType intSize (toInteger value))

reportMacroError :: Handle -> Hint -> T.Text -> App a
reportMacroError =
  reportMacroError' False

reportDefineMetaError :: Handle -> Hint -> T.Text -> App a
reportDefineMetaError =
  reportMacroError' True

reportMacroError' :: Bool -> Handle -> Hint -> T.Text -> App a
reportMacroError' highlightDefineMeta h m message = do
  ms <- liftIO $ getMacroCallStack h
  let moduleDir = T.pack $ toFilePath $ Module.getModuleRootDir $ Module.extractModule (mainModule h)
  let trace = formatMacroTrace highlightDefineMeta moduleDir ms
  let hints = map (\(_, _, hint) -> hint) ms
  let hintStack = NE.reverse $ m :| hints
  raiseError (NE.head hintStack) $ message <> "\n\n" <> trace

getMacroCallStack :: Handle -> IO [(DD.DefiniteDescription, DefKind, Hint)]
getMacroCallStack h = do
  let Handle {macroCallStack} = h
  readIORef macroCallStack

formatMacroTrace :: Bool -> T.Text -> [(DD.DefiniteDescription, DefKind, Hint)] -> T.Text
formatMacroTrace highlightDefineMeta moduleDir stack =
  case reverse stack of
    [] ->
      ""
    frame : rest -> do
      let firstLine = "    " <> formatMacroFrame highlightDefineMeta moduleDir frame
      let restLines = map (\frame' -> "  → " <> formatMacroFrame highlightDefineMeta moduleDir frame') rest
      let allLines = firstLine : restLines
      "Trace:\n" <> T.intercalate "\n" allLines

formatMacroFrame :: Bool -> T.Text -> (DD.DefiniteDescription, DefKind, Hint) -> T.Text
formatMacroFrame highlightDefineMeta moduleDir (dd, defKind, m) = do
  let base = DD.localLocator dd <> " (" <> showFilePosRelative moduleDir m <> ")"
  case defKind of
    Macro
      | highlightDefineMeta ->
          base <> " [define-meta]"
    _ ->
      base
