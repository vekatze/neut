module Language.Term.Inline.Magic
  ( evaluateGetTypeTag,
    evaluateGetConsSize,
    evaluateGetConstructorArgTypes,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad (foldM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.TypeTag qualified as TypeTag
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.BaseName qualified as BN
import Language.Common.Binder (BinderF)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Ident (Ident)
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.ModuleID qualified as MID
import Language.Common.PrimNumSize qualified as PNS
import Language.Common.PrimType qualified as PT
import Language.Common.SourceLocator (binaryLocator, typeTagLocator, vectorLocator)
import Language.Common.StrictGlobalLocator qualified as SGL
import Language.Term.FreeVars qualified as FreeVars
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Language.Term.Weaken (weaken, weakenType)
import Language.WeakTerm.ToText (toTextType)
import Logger.Hint (Hint)

evaluateGetTypeTag :: Hint -> MID.ModuleID -> TM.Type -> TM.Type -> App TM.Term
evaluateGetTypeTag m moduleID typeTagExpr typeExpr = do
  case typeExpr of
    _ :< TM.Tau ->
      returnTypeTagIntValue m moduleID TypeTag.Type
    _ :< TM.Pi {} ->
      returnTypeTagIntValue m moduleID TypeTag.Function
    _ :< TM.Data (AttrD.Attr {AttrD.consNameList}) _ _ -> do
      case consNameList of
        [(_, [(_, _, t)], _)] -> do
          evaluateGetTypeTag m moduleID typeTagExpr t -- newtype
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
      let binarySGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = binaryLocator}
      let binaryDD = DD.newByGlobalLocator binarySGL BN.binary
      if name == binaryDD
        then returnTypeTagIntValue m moduleID TypeTag.Binary
        else returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ :< TM.TVarGlobal _ name -> do
      let vectorSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = vectorLocator}
      let vectorDD = DD.newByGlobalLocator vectorSGL BN.vectorInternal
      if name == vectorDD
        then returnTypeTagIntValue m moduleID TypeTag.Vector
        else returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ :< TM.TyApp (_ :< TM.TVarGlobal _ name) _ -> do
      let vectorSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = vectorLocator}
      let vectorDD = DD.newByGlobalLocator vectorSGL BN.vectorInternal
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
  let typeTagSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = typeTagLocator}
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
