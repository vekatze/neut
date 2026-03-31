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
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef (readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Kernel.Common.TypeTag qualified as TypeTag
import Kernel.Common.TypeValue qualified as TypeValue
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseName qualified as BN
import Language.Common.Binder (BinderF)
import Language.Common.CreateSymbol qualified as Gensym
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
import Language.Common.VarKind qualified as VK
import Language.Term.Eq qualified as TermEq
import Language.Term.Inline.Handle
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Language.Term.Weaken (weakenType)
import Language.WeakTerm.ToText (toTextType)
import Logger.Hint (Hint, showFilePos)

evaluateInspectType :: Handle -> Hint -> MID.ModuleID -> TM.Type -> App TM.Term
evaluateInspectType h m moduleID typeExpr = do
  case typeExpr of
    _ :< TM.Tau ->
      returnTypeValueIntValue h m moduleID TypeValue.Type
    _ :< TM.Pi {} ->
      returnTypeValueIntValue h m moduleID TypeValue.Function
    _ :< TM.Data (AttrD.Attr {AttrD.consNameList}) dataName dataArgs -> do
      case consNameList of
        [(_, [(_, _, _, arg)], _)] -> do
          if dataName == makeVectorDD moduleID
            then do
              case dataArgs of
                [dataArg] ->
                  returnTypeValueIntValue h m moduleID $ TypeValue.Vector dataArg
                _ -> do
                  let len = length dataArgs
                  reportMacroError h m $
                    "inspect-type: `vector` expects 1 argument, but got " <> T.pack (show len) <> " arguments."
            else returnTypeValueIntValue h m moduleID $ TypeValue.Wrapper arg
        _ -> do
          let consInfoList = map consToTypeValue consNameList
          let isEnum = all (\(_, binders, isConstLike) -> isConstLike && null binders) consNameList
          if isEnum && not (null consNameList)
            then do
              let enumConsNames = map (\(dd, _, _) -> DD.localLocator dd) consNameList
              returnTypeValueIntValue h m moduleID $ TypeValue.Enum enumConsNames
            else do
              let dataNameText = DD.localLocator dataName
              returnTypeValueIntValue h m moduleID $ TypeValue.Algebraic dataNameText dataArgs consInfoList
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

makeVectorDD :: MID.ModuleID -> DD.DefiniteDescription
makeVectorDD moduleID = do
  let vectorSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.vectorLocator}
  DD.newByGlobalLocator vectorSGL BN.vector

makeConsNameList ::
  Handle ->
  Hint ->
  SGL.StrictGlobalLocator ->
  App [(DD.DefiniteDescription, [BinderF TM.Type], IsConstLike)]
makeConsNameList h m typeValueSGL = do
  let moduleID = SGL.moduleID typeValueSGL
  forM TypeTag.typeTagList $ \tag -> do
    let dd = DD.newByGlobalLocator typeValueSGL (BN.fromTypeTag tag)
    case tag of
      TypeTag.Vector -> do
        doNotCare <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "tmp"
        return (dd, [(m, VK.Normal, doNotCare, m :< TM.Tau)], False)
      TypeTag.Algebraic -> do
        doNotCare0 <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "tmp"
        doNotCare1 <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "tmp"
        doNotCare2 <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "tmp"
        let stringType = makeTextTypeExpr m moduleID
        let dataNameType = m :< TM.BoxNoema stringType
        let dataArgsType = makeListTypeExpr m moduleID (m :< TM.Tau)
        let consInfoListType = makeListTypeExpr m moduleID (makeConstructorTypeExpr m moduleID)
        return
          ( dd,
            [ (m, VK.Normal, doNotCare0, dataNameType),
              (m, VK.Normal, doNotCare1, dataArgsType),
              (m, VK.Normal, doNotCare2, consInfoListType)
            ],
            False
          )
      TypeTag.Wrapper -> do
        doNotCare <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "tmp"
        return (dd, [(m, VK.Normal, doNotCare, m :< TM.Tau)], False)
      TypeTag.Noema -> do
        doNotCare <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "tmp"
        return (dd, [(m, VK.Normal, doNotCare, m :< TM.Tau)], False)
      TypeTag.Enum -> do
        doNotCare <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "tmp"
        let stringType = makeTextTypeExpr m moduleID
        let nameListType = makeListTypeExpr m moduleID (m :< TM.BoxNoema stringType)
        return (dd, [(m, VK.Normal, doNotCare, nameListType)], False)
      TypeTag.BoxT -> do
        doNotCare <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "tmp"
        return (dd, [(m, VK.Normal, doNotCare, m :< TM.Tau)], False)
      _ ->
        return (dd, [], True)

consToTypeValue ::
  (DD.DefiniteDescription, [BinderF TM.Type], IsConstLike) ->
  (T.Text, IsConstLike, [(T.Text, TM.Type)])
consToTypeValue (dd, binders, isConstLike) = do
  let params = map (\(_, _, x, t) -> (Ident.toText x, t)) binders
  (DD.localLocator dd, isConstLike, params)

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
  Handle ->
  Hint ->
  SGL.StrictGlobalLocator ->
  TypeTag.TypeTag ->
  App (AttrDI.Attr DD.DefiniteDescription (BinderF TM.Type))
makeAttrDI h m typeValueSGL typeTag = do
  let dataName = DD.newByGlobalLocator typeValueSGL BN.typeValue
  let discriminant = D.MakeDiscriminant $ TypeTag.typeTagToInteger typeTag
  consNameList <- makeConsNameList h m typeValueSGL
  return $ AttrDI.Attr {dataName, consNameList, discriminant, isConstLike = isConstTypeTag typeTag}

returnTypeValueIntValue :: Handle -> Hint -> MID.ModuleID -> TypeValue.TypeValue -> App TM.Term
returnTypeValueIntValue h m moduleID typeValue = do
  let typeValueSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.typeValueLocator}
  attr <- makeAttrDI h m typeValueSGL $ TypeValue.toTypeTag typeValue
  let tag = TypeValue.toTypeTag typeValue
  let consName = DD.newByGlobalLocator typeValueSGL (BN.fromTypeTag tag)
  case typeValue of
    TypeValue.Algebraic dataName dataArgs consInfoList -> do
      let listSgl = makeListSGL moduleID
      let stringType = makeTextTypeExpr m moduleID
      let dataNameTerm = m :< TM.Prim (PV.StaticString stringType dataName)
      dataArgsTerm <- constructListTerm h m listSgl dataArgs
      consInfoTerm <- constructConstructorInfoListTerm h m moduleID consInfoList
      return $ m :< TM.DataIntro attr consName [] [dataNameTerm, dataArgsTerm, consInfoTerm]
    TypeValue.Enum consNames -> do
      let listSgl = makeListSGL moduleID
      let stringType = makeTextTypeExpr m moduleID
      let elemType = m :< TM.BoxNoema stringType
      let nameTerms = map (\name -> m :< TM.Prim (PV.StaticString stringType name)) consNames
      namesListTerm <- constructListTermFromTerms h m listSgl elemType nameTerms
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

constructListTerm :: Handle -> Hint -> SGL.StrictGlobalLocator -> [TM.Type] -> App TM.Term
constructListTerm h m listSgl types = do
  let wrappedTypes = map (\ty -> m :< TM.TauIntro ty) types
  constructListTermFromTerms h m listSgl (m :< TM.Tau) wrappedTypes

constructListTermFromTerms :: Handle -> Hint -> SGL.StrictGlobalLocator -> TM.Type -> [TM.Term] -> App TM.Term
constructListTermFromTerms h hint listSgl elemType terms =
  case terms of
    [] ->
      constructListNilTerm h hint listSgl elemType
    headTerm : rest -> do
      tailList <- constructListTermFromTerms h hint listSgl elemType rest
      constructListConsTerm h hint listSgl elemType headTerm tailList

constructListNilTerm :: Handle -> Hint -> SGL.StrictGlobalLocator -> TM.Type -> App TM.Term
constructListNilTerm h hint listSgl elemType = do
  let dataName = coreListType listSgl
  consNameList <- makeListConsNameList h hint listSgl elemType
  let attr = AttrDI.Attr {dataName, consNameList, discriminant = D.zero, isConstLike = True}
  return $ hint :< TM.DataIntro attr (coreListNil listSgl) [elemType] []

constructListConsTerm :: Handle -> Hint -> SGL.StrictGlobalLocator -> TM.Type -> TM.Term -> TM.Term -> App TM.Term
constructListConsTerm h hint listSgl elemType headTerm tailList = do
  let dataName = coreListType listSgl
  consNameList <- makeListConsNameList h hint listSgl elemType
  let attr = AttrDI.Attr {dataName, consNameList, discriminant = D.increment D.zero, isConstLike = False}
  return $ hint :< TM.DataIntro attr (coreListCons listSgl) [elemType] [headTerm, tailList]

makeListConsNameList ::
  Handle ->
  Hint ->
  SGL.StrictGlobalLocator ->
  TM.Type ->
  App [(DD.DefiniteDescription, [BinderF TM.Type], IsConstLike)]
makeListConsNameList h hint listSgl elemType = do
  headBinder <- mkBinder h hint "head" elemType
  tailBinder <- mkBinder h hint "tail" (makeListTypeExpr hint (SGL.moduleID listSgl) elemType)
  return
    [ (coreListNil listSgl, [], True),
      (coreListCons listSgl, [headBinder, tailBinder], False)
    ]

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

makeListTypeExpr :: Hint -> MID.ModuleID -> TM.Type -> TM.Type
makeListTypeExpr m moduleID elemType = do
  let listSgl = makeListSGL moduleID
  let listDD = DD.newByGlobalLocator listSgl BN.list
  let listTypeVar = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.fromInt 1, isConstLike = False, isDestPassing = False}) listDD
  m :< TM.TyApp listTypeVar [elemType]

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

constructPairTerm :: Handle -> Hint -> MID.ModuleID -> TM.Type -> TM.Type -> TM.Term -> TM.Term -> App TM.Term
constructPairTerm h m moduleID leftType rightType leftTerm rightTerm = do
  let pairSgl = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.pairLocator}
  let dataName = DD.newByGlobalLocator pairSgl BN.pairType
  let consName = DD.newByGlobalLocator pairSgl BN.pair
  leftBinder <- mkBinder h m "left" leftType
  rightBinder <- mkBinder h m "right" rightType
  let consNameList = [(consName, [leftBinder, rightBinder], False)]
  let attr = AttrDI.Attr {dataName, consNameList, discriminant = D.zero, isConstLike = False}
  return $ m :< TM.DataIntro attr consName [leftType, rightType] [leftTerm, rightTerm]

constructBoolTerm :: Hint -> MID.ModuleID -> IsConstLike -> TM.Term
constructBoolTerm hint moduleID value = do
  let boolSgl = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.boolLocator}
  let boolTypeDD = DD.newByGlobalLocator boolSgl BN.boolType
  let trueDD = DD.newByGlobalLocator boolSgl BN.trueConstructor
  let falseDD = DD.newByGlobalLocator boolSgl BN.falseConstructor
  let consNameList = [(falseDD, [], True), (trueDD, [], True)]
  let (consName, discriminant) =
        if value
          then (trueDD, D.increment D.zero)
          else (falseDD, D.zero)
  let attr = AttrDI.Attr {dataName = boolTypeDD, consNameList, discriminant, isConstLike = True}
  hint :< TM.DataIntro attr consName [] []

constructConstructorInfoListTerm ::
  Handle ->
  Hint ->
  MID.ModuleID ->
  [(T.Text, IsConstLike, [(T.Text, TM.Type)])] ->
  App TM.Term
constructConstructorInfoListTerm h hint moduleID consInfoList = do
  let listSgl = makeListSGL moduleID
  let constructorType = makeConstructorTypeExpr hint moduleID
  consInfoTerms <- mapM (constructConstructorTerm h hint moduleID) consInfoList
  constructListTermFromTerms h hint listSgl constructorType consInfoTerms

constructConstructorTerm ::
  Handle ->
  Hint ->
  MID.ModuleID ->
  (T.Text, IsConstLike, [(T.Text, TM.Type)]) ->
  App TM.Term
constructConstructorTerm h m moduleID (consName, isConstLike, params) = do
  let constructorSgl = makeConstructorSGL moduleID
  let listSgl = makeListSGL moduleID
  let constructorTypeDD = DD.newByGlobalLocator constructorSgl BN.constructorType
  let consDD = DD.newByGlobalLocator constructorSgl BN.constructor
  let stringType = makeTextTypeExpr m moduleID
  let boolSgl = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.boolLocator}
  let boolTypeDD = DD.newByGlobalLocator boolSgl BN.boolType
  let boolType = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True, isDestPassing = False}) boolTypeDD
  let paramPairType = makePairTypeExpr m moduleID (m :< TM.BoxNoema stringType) (m :< TM.Tau)
  let paramListType = makeListTypeExpr m moduleID paramPairType
  nameBinder <- mkBinder h m "name" (m :< TM.BoxNoema stringType)
  paramsBinder <- mkBinder h m "params" paramListType
  flagBinder <- mkBinder h m "is-const-like" boolType
  let consNameList = [(consDD, [nameBinder, flagBinder, paramsBinder], False)]
  let attr = AttrDI.Attr {dataName = constructorTypeDD, consNameList, discriminant = D.zero, isConstLike = False}
  let consNameText = m :< TM.Prim (PV.StaticString stringType consName)
  paramTerms <- mapM (constructParamPairTerm h m moduleID stringType) params
  paramListTerm <- constructListTermFromTerms h m listSgl paramPairType paramTerms
  let boolTerm = constructBoolTerm m moduleID isConstLike
  return $ m :< TM.DataIntro attr consDD [] [consNameText, boolTerm, paramListTerm]

constructParamPairTerm :: Handle -> Hint -> MID.ModuleID -> TM.Type -> (T.Text, TM.Type) -> App TM.Term
constructParamPairTerm h m moduleID stringType (paramName, paramType) = do
  let nameTerm = m :< TM.Prim (PV.StaticString stringType paramName)
  let typeTerm = m :< TM.TauIntro paramType
  constructPairTerm h m moduleID (m :< TM.BoxNoema stringType) (m :< TM.Tau) nameTerm typeTerm

mkBinder :: Handle -> Hint -> T.Text -> TM.Type -> App (BinderF TM.Type)
mkBinder h hint name ty = do
  x <- liftIO $ Gensym.newIdentFromText (gensymHandle h) name
  return (hint, VK.Normal, x, ty)

evaluateShowType :: Hint -> TM.Type -> TM.Type -> App TM.Term
evaluateShowType m stringTypeExpr typeExpr = do
  let typeText = toTextType $ weakenType typeExpr
  return $ m :< TM.Prim (PV.StaticString stringTypeExpr typeText)

evaluateStringCons :: Handle -> Hint -> TM.Type -> TM.Term -> TM.Term -> App TM.Term
evaluateStringCons h m stringTypeExpr rune text = do
  case (rune, text) of
    (_ :< TM.Prim (PV.Rune r), _ :< TM.Prim (PV.StaticString _ textValue)) -> do
      let newText = T.cons (Rune.asChar r) textValue
      return $ m :< TM.Prim (PV.StaticString stringTypeExpr newText)
    _ ->
      reportMacroError h m "text-cons requires a rune literal and a static text literal"

evaluateStringUncons :: Handle -> Hint -> MID.ModuleID -> TM.Term -> App TM.Term
evaluateStringUncons h m moduleID text = do
  case text of
    _ :< TM.Prim (PV.StaticString stringTypeExpr textValue) -> do
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
          let restText = m :< TM.Prim (PV.StaticString stringTypeExpr rest)
          let pair = m :< TM.PiElim PEK.Normal (m :< TM.PiElim PEK.Normal pairVar [runeType, m :< TM.BoxNoema stringTypeExpr] [] []) [] [runeValue, restText] []
          return $ m :< TM.PiElim PEK.Normal (m :< TM.PiElim PEK.Normal rightVar [unitTypeVar, pairType] [] []) [] [pair] []
    _ ->
      reportMacroError h m "text-uncons requires a static text literal"

evaluateCompileError :: Handle -> Hint -> TM.Term -> App a
evaluateCompileError h m msg = do
  case msg of
    _ :< TM.Prim (PV.StaticString _ messageText) -> do
      reportMacroError h m messageText
    _ ->
      raiseError m "compile-error requires a static text message"

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
