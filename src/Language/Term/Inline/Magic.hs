module Language.Term.Inline.Magic
  ( evaluateInspectType,
    evaluateEqType,
    evaluateShowType,
    evaluateStringCons,
    evaluateStringUncons,
    evaluateCompileError,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Data.IntMap qualified as IntMap
import Data.IORef (readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.TypeTag qualified as TypeTag
import Kernel.Common.TypeValue qualified as TypeValue
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseName qualified as BN
import Language.Common.Binder (BinderF)
import Language.Common.DataInfo qualified as DI
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.ModuleID qualified as MID
import Language.Common.PiElimKind qualified as PEK
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
import Logger.Hint (Hint, showFilePos)

evaluateInspectType :: Handle -> Hint -> MID.ModuleID -> TM.Type -> App TM.Term
evaluateInspectType h m moduleID typeExpr = do
  case typeExpr of
    _ :< TM.Tau ->
      returnTypeValueIntValue m moduleID TypeValue.Type
    _ :< TM.Pi {} ->
      returnTypeValueIntValue m moduleID TypeValue.Function
    _ :< TM.Data _ dataName dataArgs -> do
      DI.DataInfo {DI.dataArgs = dataArgBinders, DI.consInfoList = consInfoListRaw} <- lookupDataInfo h m dataName
      consInfoList <- specializeConsInfoList h m dataName dataArgBinders dataArgs consInfoListRaw
      case consInfoList of
        [DI.ConsInfo {DI.consArgs = [(_, _, _, arg)]}] -> do
          if dataName == makeVectorDD moduleID
            then do
              case dataArgs of
                [dataArg] ->
                  returnTypeValueIntValue m moduleID $ TypeValue.Vector dataArg
                _ -> do
                  let len = length dataArgs
                  reportMacroError h m $
                    "inspect-type: `vector` expects 1 argument, but got " <> T.pack (show len) <> " arguments."
            else returnTypeValueIntValue m moduleID $ TypeValue.Wrapper arg
        _ -> do
          let consInfoList' = map consToTypeValue consInfoList
          let isEnum = all (\consInfo -> DI.isConstLike consInfo && null (DI.consArgs consInfo)) consInfoList
          if isEnum && not (null consInfoList)
            then do
              let enumConsNames = map (DD.localLocator . DI.consName) consInfoList
              returnTypeValueIntValue m moduleID $ TypeValue.Enum enumConsNames
            else do
              let dataNameText = DD.localLocator dataName
              returnTypeValueIntValue m moduleID $ TypeValue.Algebraic dataNameText dataArgs consInfoList'
    _ :< TM.BoxNoema t ->
      returnTypeValueIntValue m moduleID $ TypeValue.Noema t
    _ :< TM.Box t ->
      returnTypeValueIntValue m moduleID $ TypeValue.BoxT t
    _ :< TM.Code _ ->
      returnTypeValueIntValue m moduleID TypeValue.Opaque
    _ :< TM.PrimType (PT.Int size) ->
      returnTypeValueIntValue m moduleID (TypeValue.fromIntSize size)
    _ :< TM.PrimType (PT.Float size) ->
      returnTypeValueIntValue m moduleID (TypeValue.fromFloatSize size)
    _ :< TM.PrimType PT.Pointer ->
      returnTypeValueIntValue m moduleID TypeValue.Pointer
    _ :< TM.PrimType PT.Rune ->
      returnTypeValueIntValue m moduleID TypeValue.Rune
    _ :< TM.Resource name _ -> do
      let binarySGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.binaryLocator}
      let binaryDD = DD.newByGlobalLocator binarySGL BN.binary
      if name == binaryDD
        then returnTypeValueIntValue m moduleID TypeValue.Binary
        else returnTypeValueIntValue m moduleID TypeValue.Opaque
    _ :< TM.TVarGlobal _ _ -> do
      returnTypeValueIntValue m moduleID TypeValue.Opaque
    _ :< TM.TyApp (_ :< TM.TVarGlobal _ _) _ -> do
      returnTypeValueIntValue m moduleID TypeValue.Opaque
    _ -> do
      reportMacroError h m $
        "inspect-type: unable to determine type value for this type expression. Got: "
          <> toTextType (weakenType typeExpr)

evaluateEqType :: Hint -> MID.ModuleID -> TM.Type -> TM.Type -> App TM.Term
evaluateEqType m moduleID typeExpr1 typeExpr2 = do
  let isEqual = TermEq.eqType typeExpr1 typeExpr2
  return $ constructBoolTerm m moduleID isEqual

makeVectorDD :: MID.ModuleID -> DD.DefiniteDescription
makeVectorDD moduleID = do
  let vectorSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.vectorLocator}
  DD.newByGlobalLocator vectorSGL BN.vector

consToTypeValue ::
  DI.ConsInfo (BinderF TM.Type) ->
  (T.Text, IsConstLike, [(T.Text, TM.Type)])
consToTypeValue consInfo = do
  let params = map (\(_, _, x, t) -> (Ident.toText x, t)) (DI.consArgs consInfo)
  (DD.localLocator (DI.consName consInfo), DI.isConstLike consInfo, params)

isConstTypeTag :: TypeTag.TypeTag -> Bool
isConstTypeTag tt =
  case tt of
    TypeTag.Vector ->
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

returnTypeValueIntValue :: Hint -> MID.ModuleID -> TypeValue.TypeValue -> App TM.Term
returnTypeValueIntValue m moduleID typeValue = do
  let typeValueSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.typeValueLocator}
  attr <- makeAttrDI typeValueSGL $ TypeValue.toTypeTag typeValue
  let tag = TypeValue.toTypeTag typeValue
  let consName = DD.newByGlobalLocator typeValueSGL (BN.fromTypeTag tag)
  case typeValue of
    TypeValue.Algebraic dataName dataArgs consInfoList -> do
      let listSgl = makeListSGL moduleID
      let stringType = makeTextTypeExpr m moduleID
      let dataNameTerm = m :< TM.Prim (PV.NoeticString stringType dataName)
      dataArgsTerm <- constructListTerm m listSgl dataArgs
      consInfoTerm <- constructConstructorInfoListTerm m moduleID consInfoList
      return $ m :< TM.DataIntro attr consName [] [dataNameTerm, dataArgsTerm, consInfoTerm]
    TypeValue.Enum consNames -> do
      let listSgl = makeListSGL moduleID
      let stringType = makeTextTypeExpr m moduleID
      let elemType = m :< TM.BoxNoema stringType
      let nameTerms = map (\name -> m :< TM.Prim (PV.NoeticString stringType name)) consNames
      namesListTerm <- constructListTermFromTerms m listSgl elemType nameTerms
      return $ m :< TM.DataIntro attr consName [] [namesListTerm]
    TypeValue.Vector t -> do
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

makePairTypeExpr :: Hint -> MID.ModuleID -> TM.Type -> TM.Type -> TM.Type
makePairTypeExpr m moduleID leftType rightType = do
  let pairSgl = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.pairLocator}
  let pairTypeDD = DD.newByGlobalLocator pairSgl BN.pairType
  let pairTypeVar = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.fromInt 4, isConstLike = False, isDestPassing = False}) pairTypeDD
  m :< TM.TyApp pairTypeVar [leftType, rightType]

makeTextTypeExpr :: Hint -> MID.ModuleID -> TM.Type
makeTextTypeExpr m moduleID = do
  let textSgl = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.stringLocator}
  let stringTypeDD = DD.newByGlobalLocator textSgl BN.stringType
  m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True, isDestPassing = False}) stringTypeDD

constructPairTerm :: Hint -> MID.ModuleID -> TM.Type -> TM.Type -> TM.Term -> TM.Term -> App TM.Term
constructPairTerm m moduleID leftType rightType leftTerm rightTerm = do
  let pairSgl = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.pairLocator}
  let dataName = DD.newByGlobalLocator pairSgl BN.pairType
  let consName = DD.newByGlobalLocator pairSgl BN.pair
  let attr = AttrDI.Attr {dataName, discriminant = D.zero, isConstLike = False}
  return $ m :< TM.DataIntro attr consName [leftType, rightType] [leftTerm, rightTerm]

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
  Hint ->
  MID.ModuleID ->
  [(T.Text, IsConstLike, [(T.Text, TM.Type)])] ->
  App TM.Term
constructConstructorInfoListTerm hint moduleID consInfoList = do
  let listSgl = makeListSGL moduleID
  let constructorType = makeConstructorTypeExpr hint moduleID
  consInfoTerms <- mapM (constructConstructorTerm hint moduleID) consInfoList
  constructListTermFromTerms hint listSgl constructorType consInfoTerms

constructConstructorTerm ::
  Hint ->
  MID.ModuleID ->
  (T.Text, IsConstLike, [(T.Text, TM.Type)]) ->
  App TM.Term
constructConstructorTerm m moduleID (consName, isConstLike, params) = do
  let constructorSgl = makeConstructorSGL moduleID
  let listSgl = makeListSGL moduleID
  let constructorTypeDD = DD.newByGlobalLocator constructorSgl BN.constructorType
  let consDD = DD.newByGlobalLocator constructorSgl BN.constructor
  let stringType = makeTextTypeExpr m moduleID
  let paramPairType = makePairTypeExpr m moduleID (m :< TM.BoxNoema stringType) (m :< TM.Tau)
  let attr = AttrDI.Attr {dataName = constructorTypeDD, discriminant = D.zero, isConstLike = False}
  let consNameText = m :< TM.Prim (PV.NoeticString stringType consName)
  paramTerms <- mapM (constructParamPairTerm m moduleID stringType) params
  paramListTerm <- constructListTermFromTerms m listSgl paramPairType paramTerms
  let boolTerm = constructBoolTerm m moduleID isConstLike
  return $ m :< TM.DataIntro attr consDD [] [consNameText, boolTerm, paramListTerm]

lookupDataInfo :: Handle -> Hint -> DD.DefiniteDescription -> App (DI.DataInfo (BinderF TM.Type))
lookupDataInfo h m dataName = do
  dataInfoOrNone <- liftIO $ Data.lookup (dataHandle h) dataName
  case dataInfoOrNone of
    Just dataInfo ->
      return dataInfo
    Nothing ->
      reportMacroError h m $ "inspect-type: constructor metadata for `" <> DD.reify dataName <> "` is unavailable."

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
          <> DD.reify dataName
          <> "`."

specializeConsInfo :: Handle -> Subst.Subst -> DI.ConsInfo (BinderF TM.Type) -> App (DI.ConsInfo (BinderF TM.Type))
specializeConsInfo h sub consInfo = do
  consArgs' <- mapM specializeConsArg (DI.consArgs consInfo)
  return $ consInfo {DI.consArgs = consArgs'}
  where
    specializeConsArg (mx, k, x, t) = do
      t' <- liftIO $ Subst.substType (substHandle h) sub t
      return (mx, k, x, t')

constructParamPairTerm :: Hint -> MID.ModuleID -> TM.Type -> (T.Text, TM.Type) -> App TM.Term
constructParamPairTerm m moduleID stringType (paramName, paramType) = do
  let nameTerm = m :< TM.Prim (PV.NoeticString stringType paramName)
  let typeTerm = m :< TM.TauIntro paramType
  constructPairTerm m moduleID (m :< TM.BoxNoema stringType) (m :< TM.Tau) nameTerm typeTerm

evaluateShowType :: Hint -> TM.Type -> TM.Type -> App TM.Term
evaluateShowType m stringTypeExpr typeExpr = do
  let typeText = toTextType $ weakenType typeExpr
  return $ m :< TM.Prim (PV.NoeticString stringTypeExpr typeText)

evaluateStringCons :: Handle -> Hint -> TM.Type -> TM.Term -> TM.Term -> App TM.Term
evaluateStringCons h m stringTypeExpr rune text = do
  case (rune, text) of
    (_ :< TM.Prim (PV.Rune r), _ :< TM.Prim (PV.NoeticString _ textValue)) -> do
      let newText = T.cons (Rune.asChar r) textValue
      return $ m :< TM.Prim (PV.NoeticString stringTypeExpr newText)
    _ ->
      reportMacroError h m "text-cons requires a rune literal and a static text literal"

evaluateStringUncons :: Handle -> Hint -> MID.ModuleID -> TM.Term -> App TM.Term
evaluateStringUncons h m moduleID text = do
  case text of
    _ :< TM.Prim (PV.NoeticString stringTypeExpr textValue) -> do
      let eitherSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.eitherLocator}
      let unitSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.unitLocator}
      let pairSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.pairLocator}
      let unitTypeDD = DD.newByGlobalLocator unitSGL BN.unitType
      let pairTypeDD = DD.newByGlobalLocator pairSGL BN.pairType
      let unitTypeVar = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True, isDestPassing = False}) unitTypeDD
      let pairTypeVar = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.fromInt 4, isConstLike = False, isDestPassing = False}) pairTypeDD
      let runeType = m :< TM.PrimType PT.Rune
      let pairType = m :< TM.TyApp pairTypeVar [runeType, m :< TM.BoxNoema stringTypeExpr]
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
          let restText = m :< TM.Prim (PV.NoeticString stringTypeExpr rest)
          let pair = m :< TM.PiElim PEK.Normal (m :< TM.PiElim PEK.Normal pairVar [runeType, m :< TM.BoxNoema stringTypeExpr] [] []) [] [runeValue, restText] []
          return $ m :< TM.PiElim PEK.Normal (m :< TM.PiElim PEK.Normal rightVar [unitTypeVar, pairType] [] []) [] [pair] []
    _ ->
      reportMacroError h m "text-uncons requires a static string literal"

evaluateCompileError :: Handle -> Hint -> TM.Term -> App a
evaluateCompileError h m msg = do
  case msg of
    _ :< TM.Prim (PV.NoeticString _ messageText) -> do
      reportMacroError h m messageText
    _ ->
      raiseError m "compile-error requires a static string message"

reportMacroError :: Handle -> Hint -> T.Text -> App a
reportMacroError h m message = do
  ms <- liftIO $ getMacroCallStack h
  let trace = formatMacroTrace ms
  let hints = map snd ms
  let hintStack = NE.reverse $ m :| hints
  raiseError (NE.head hintStack) $ message <> "\n\n" <> trace

getMacroCallStack :: Handle -> IO [(DD.DefiniteDescription, Hint)]
getMacroCallStack h = do
  let Handle {macroCallStack} = h
  readIORef macroCallStack

formatMacroTrace :: [(DD.DefiniteDescription, Hint)] -> T.Text
formatMacroTrace stack =
  case reverse stack of
    [] ->
      ""
    (dd, m) : rest -> do
      let firstLine = "    " <> DD.localLocator dd <> " (" <> T.pack (showFilePos m) <> ")"
      let restLines = map (\(dd', m') -> "  → " <> DD.localLocator dd' <> " (" <> T.pack (showFilePos m') <> ")") rest
      let allLines = firstLine : restLines
      "Trace:\n" <> T.intercalate "\n" allLines
