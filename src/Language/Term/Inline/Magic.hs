module Language.Term.Inline.Magic
  ( evaluateGetTypeTag,
    evaluateGetDataArgs,
    evaluateGetConsSize,
    evaluateGetWrapperContentType,
    evaluateGetVectorContentType,
    evaluateGetConstructorArgTypes,
    evaluateShowType,
    evaluateTextCons,
    evaluateTextUncons,
    evaluateCompileError,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Data.IORef (readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Kernel.Common.TypeTag qualified as TypeTag
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseName qualified as BN
import Language.Common.Binder (BinderF)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.ModuleID qualified as MID
import Language.Common.PrimNumSize qualified as PNS
import Language.Common.PrimType qualified as PT
import Language.Common.Rune qualified as Rune
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL
import Language.Term.Inline.Handle
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Language.Term.Weaken (weakenType)
import Language.WeakTerm.ToText (toTextType)
import Logger.Hint (Hint, showFilePos)

evaluateGetTypeTag :: Hint -> MID.ModuleID -> TM.Type -> App TM.Term
evaluateGetTypeTag m moduleID typeExpr = do
  case typeExpr of
    _ :< TM.Tau ->
      returnTypeTagIntValue m moduleID TypeTag.Type
    _ :< TM.Pi {} ->
      returnTypeTagIntValue m moduleID TypeTag.Function
    _ :< TM.Data (AttrD.Attr {AttrD.consNameList}) _ _ -> do
      case consNameList of
        [(_, [(_, _, _)], _)] -> do
          returnTypeTagIntValue m moduleID TypeTag.Wrapper
        _ -> do
          let isEnum = all (\(_, _, isConstLike) -> isConstLike) consNameList
          if isEnum && not (null consNameList)
            then returnTypeTagIntValue m moduleID TypeTag.Enum
            else returnTypeTagIntValue m moduleID TypeTag.Algebraic
    _ :< TM.BoxNoema _ ->
      returnTypeTagIntValue m moduleID TypeTag.Noema
    _ :< TM.Box _ ->
      returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ :< TM.Code _ ->
      returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ :< TM.PrimType (PT.Int size) ->
      returnTypeTagIntValue m moduleID (TypeTag.fromIntSize size)
    _ :< TM.PrimType (PT.Float size) ->
      returnTypeTagIntValue m moduleID (TypeTag.fromFloatSize size)
    _ :< TM.PrimType PT.Pointer ->
      returnTypeTagIntValue m moduleID TypeTag.Pointer
    _ :< TM.PrimType PT.Rune ->
      returnTypeTagIntValue m moduleID TypeTag.Rune
    _ :< TM.Resource name _ _ _ _ _ -> do
      let binarySGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.binaryLocator}
      let binaryDD = DD.newByGlobalLocator binarySGL BN.binary
      if name == binaryDD
        then returnTypeTagIntValue m moduleID TypeTag.Binary
        else returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ :< TM.TVarGlobal _ _ -> do
      returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ :< TM.TyApp (_ :< TM.TVarGlobal _ name) _ -> do
      let vectorSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.vectorLocator}
      let vectorDD = DD.newByGlobalLocator vectorSGL BN.vector
      if name == vectorDD
        then returnTypeTagIntValue m moduleID TypeTag.Vector
        else returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ -> do
      raiseError m $
        "get-type-tag: unable to determine type tag for this type expression. Got: "
          <> toTextType (weakenType typeExpr)

makeConsNameList :: SGL.StrictGlobalLocator -> [(DD.DefiniteDescription, [BinderF TM.Type], IsConstLike)]
makeConsNameList typeTagSGL = do
  flip map BN.typeTagList $ \tag -> (DD.newByGlobalLocator typeTagSGL tag, [], True)

makeAttrDI :: SGL.StrictGlobalLocator -> TypeTag.TypeTag -> AttrDI.Attr DD.DefiniteDescription (BinderF TM.Type)
makeAttrDI typeTagSGL typeTag = do
  let dataName = DD.newByGlobalLocator typeTagSGL BN.typeTag
  let discriminant = D.MakeDiscriminant $ TypeTag.typeTagToInteger typeTag
  let consNameList = makeConsNameList typeTagSGL
  AttrDI.Attr {dataName, consNameList, discriminant, isConstLike = True}

returnTypeTagIntValue :: Hint -> MID.ModuleID -> TypeTag.TypeTag -> App TM.Term
returnTypeTagIntValue m' moduleID tag = do
  let typeTagSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.typeTagLocator}
  let attr = makeAttrDI typeTagSGL tag
  let tagInt = TypeTag.typeTagToInteger tag
  let index = fromIntegral tagInt
  case drop index BN.typeTagList of
    consBaseName : _ -> do
      let consName = DD.newByGlobalLocator typeTagSGL consBaseName
      return $ m' :< TM.DataIntro attr consName [] []
    [] -> do
      raiseError m' $ "get-type-tag: unknown type-tag discriminant " <> T.pack (show tagInt)

evaluateGetConsSize :: Hint -> TM.Type -> App TM.Term
evaluateGetConsSize m typeExpr = do
  case typeExpr of
    _ :< TM.Data (AttrD.Attr {AttrD.consNameList}) _ _ -> do
      let consCount = length consNameList
      let intType = m :< TM.PrimType (PT.Int PNS.IntSize64)
      return $ m :< TM.Prim (PV.Int intType PNS.IntSize64 (fromIntegral consCount))
    _ ->
      raiseError m "get-cons-size: type expression must be a data type"

evaluateGetWrapperContentType :: Hint -> TM.Type -> App TM.Term
evaluateGetWrapperContentType m typeExpr =
  case typeExpr of
    _ :< TM.Data (AttrD.Attr {AttrD.consNameList}) _ _
      | [(_, [(_, _, t)], _)] <- consNameList -> do
          return $ m :< TM.TauIntro t
    _ ->
      raiseError m "get-wrapper-content-type: type expression must be a wrapper"

evaluateGetVectorContentType :: Hint -> SGL.StrictGlobalLocator -> TM.Type -> App TM.Term
evaluateGetVectorContentType m vectorSgl typeExpr = do
  let vectorDD = DD.newByGlobalLocator vectorSgl BN.vector
  case typeExpr of
    _ :< TM.TyApp (_ :< TM.TVarGlobal _ dd) [contentType]
      | dd == vectorDD -> do
          return $ m :< TM.TauIntro contentType
    _ ->
      raiseError m "get-vector-content-type: type expression must be vector(..)"

evaluateGetDataArgs :: Hint -> SGL.StrictGlobalLocator -> TM.Type -> TM.Type -> App TM.Term
evaluateGetDataArgs m sgl _listExpr typeExpr = do
  case typeExpr of
    _ :< TM.Data _ _ dataArgs -> do
      return $ constructListTerm m sgl dataArgs
    _ ->
      raiseError m "get-data-args: type expression must be a data type"

evaluateGetConstructorArgTypes :: Hint -> SGL.StrictGlobalLocator -> TM.Type -> TM.Term -> App TM.Term
evaluateGetConstructorArgTypes m sgl typeExpr indexExpr = do
  case (typeExpr, indexExpr) of
    (_ :< TM.Data (AttrD.Attr {AttrD.consNameList}) _ _, _ :< TM.Prim (PV.Int _ _ indexInt)) -> do
      let index = fromIntegral indexInt
      if index < 0 || index >= length consNameList
        then
          raiseError m $
            "get-constructor-arg-types: index "
              <> T.pack (show index)
              <> " is out of bounds (valid range: 0-"
              <> T.pack (show (length consNameList - 1))
              <> ")"
        else do
          let (_, binders, _) = consNameList !! index
          let types = map (\(_, _, t) -> t) binders
          return $ constructListTerm m sgl types
    (_ :< TM.Data {}, _) ->
      raiseError m "get-constructor-arg-types: index must be an integer literal, but got a different term"
    _ ->
      raiseError m "get-constructor-arg-types: type expression must be a data type"

constructListTerm :: Hint -> SGL.StrictGlobalLocator -> [TM.Type] -> TM.Term
constructListTerm hint listSgl types = do
  let wrappedTypes = map (\ty -> hint :< TM.TauIntro ty) types
  foldr (constructListCons hint listSgl) (constructListNil hint listSgl) wrappedTypes

constructListNil :: Hint -> SGL.StrictGlobalLocator -> TM.Term
constructListNil hint listSgl = do
  let dataName = coreListType listSgl
  let consNameList = [(coreListNil listSgl, [], True), (coreListCons listSgl, [], False)]
  let attr = AttrDI.Attr {dataName, consNameList, discriminant = D.zero, isConstLike = True}
  hint :< TM.DataIntro attr (coreListNil listSgl) [hint :< TM.Tau] []

constructListCons :: Hint -> SGL.StrictGlobalLocator -> TM.Term -> TM.Term -> TM.Term
constructListCons hint listSgl headType tailList = do
  let dataName = coreListType listSgl
  let consNameList = [(coreListNil listSgl, [], True), (coreListCons listSgl, [], False)]
  let attr = AttrDI.Attr {dataName, consNameList, discriminant = D.increment D.zero, isConstLike = False}
  hint :< TM.DataIntro attr (coreListCons listSgl) [hint :< TM.Tau] [headType, tailList]

coreListType :: SGL.StrictGlobalLocator -> DD.DefiniteDescription
coreListType sglList =
  DD.newByGlobalLocator sglList BN.list

coreListNil :: SGL.StrictGlobalLocator -> DD.DefiniteDescription
coreListNil sglList =
  DD.newByGlobalLocator sglList BN.nil

coreListCons :: SGL.StrictGlobalLocator -> DD.DefiniteDescription
coreListCons sglList =
  DD.newByGlobalLocator sglList BN.consName

evaluateShowType :: Hint -> TM.Type -> TM.Type -> App TM.Term
evaluateShowType m textTypeExpr typeExpr = do
  let typeText = toTextType $ weakenType typeExpr
  return $ m :< TM.Prim (PV.StaticText textTypeExpr typeText)

evaluateTextCons :: Hint -> TM.Type -> TM.Term -> TM.Term -> App TM.Term
evaluateTextCons m textTypeExpr rune text = do
  case (rune, text) of
    (_ :< TM.Prim (PV.Rune r), _ :< TM.Prim (PV.StaticText _ textValue)) -> do
      let newText = T.cons (Rune.asChar r) textValue
      return $ m :< TM.Prim (PV.StaticText textTypeExpr newText)
    _ ->
      raiseError m "text-cons requires a rune literal and a static text literal"

evaluateTextUncons :: Hint -> MID.ModuleID -> TM.Term -> App TM.Term
evaluateTextUncons m moduleID text = do
  case text of
    _ :< TM.Prim (PV.StaticText textTypeExpr textValue) -> do
      let eitherSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.eitherLocator}
      let unitSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.unitLocator}
      let pairSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = SL.pairLocator}
      let unitTypeDD = DD.newByGlobalLocator unitSGL BN.unitType
      let pairTypeDD = DD.newByGlobalLocator pairSGL BN.pairType
      let unitTypeVar = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True}) unitTypeDD
      let pairTypeVar = m :< TM.TVarGlobal (AttrVG.Attr {argNum = AN.fromInt 4, isConstLike = False}) pairTypeDD
      let runeType = m :< TM.PrimType PT.Rune
      let pairType = m :< TM.TyApp pairTypeVar [runeType, m :< TM.BoxNoema textTypeExpr]
      case T.uncons textValue of
        Nothing -> do
          let leftDD = DD.newByGlobalLocator eitherSGL BN.left
          let unitDD = DD.newByGlobalLocator unitSGL BN.unit
          let leftVar = m :< TM.VarGlobal (AttrVG.Attr {argNum = AN.fromInt 3, isConstLike = False}) leftDD
          let unitVar = m :< TM.VarGlobal (AttrVG.Attr {argNum = AN.zero, isConstLike = True}) unitDD
          return $ m :< TM.PiElim False (m :< TM.PiElim False leftVar [unitTypeVar, pairType] []) [] [unitVar]
        Just (c, rest) -> do
          let rightDD = DD.newByGlobalLocator eitherSGL BN.right
          let pairDD = DD.newByGlobalLocator pairSGL BN.pair
          let rightVar = m :< TM.VarGlobal (AttrVG.Attr {argNum = AN.fromInt 3, isConstLike = False}) rightDD
          let pairVar = m :< TM.VarGlobal (AttrVG.Attr {argNum = AN.fromInt 4, isConstLike = False}) pairDD
          let runeValue = m :< TM.Prim (PV.Rune (Rune.fromChar c))
          let restText = m :< TM.Prim (PV.StaticText textTypeExpr rest)
          let pair = m :< TM.PiElim False (m :< TM.PiElim False pairVar [runeType, m :< TM.BoxNoema textTypeExpr] []) [] [runeValue, restText]
          return $ m :< TM.PiElim False (m :< TM.PiElim False rightVar [unitTypeVar, pairType] []) [] [pair]
    _ ->
      raiseError m "text-uncons requires a static text literal"

evaluateCompileError :: Handle -> Hint -> TM.Term -> App a
evaluateCompileError h m msg = do
  case msg of
    _ :< TM.Prim (PV.StaticText _ messageText) -> do
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
