module Language.Term.Weaken
  ( weaken,
    weakenType,
    weakenTypeBinder,
    weakenStmt,
    weakenDecisionTree,
  )
where

import Control.Comonad.Cofree
import Data.Bifunctor
import Data.List qualified as List
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.Foreign qualified as F
import Language.Common.Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.StmtKind
import Language.Term.PrimValue qualified as PV
import Language.Term.Stmt
import Language.Term.Term qualified as TM
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakStmt
import Language.WeakTerm.WeakTerm (reflectOpacity)
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

weakenStmt :: Stmt -> WeakStmt
weakenStmt stmt = do
  case stmt of
    StmtDefine isConstLike stmtKind (SavedHint m) name impArgs expArgs defaultArgs codType e -> do
      let stmtKind' = weakenStmtKindTerm stmtKind
      let impArgs' = map weakenTypeBinder impArgs
      let expArgs' = map weakenTypeBinder expArgs
      let defaultArgs' = map (bimap weakenTypeBinder weaken) defaultArgs
      let codType' = weakenType codType
      let e' = weaken e
      WeakStmtDefineTerm isConstLike stmtKind' m name impArgs' expArgs' defaultArgs' codType' e'
    StmtDefineType isConstLike stmtKind (SavedHint m) name impArgs expArgs defaultArgs codType body -> do
      let stmtKind' = weakenStmtKindType stmtKind
      let impArgs' = map weakenTypeBinder impArgs
      let expArgs' = map weakenTypeBinder expArgs
      let defaultArgs' = map (bimap weakenTypeBinder weaken) defaultArgs
      let codType' = weakenType codType
      let body' = weakenType body
      WeakStmtDefineType isConstLike stmtKind' m name impArgs' expArgs' defaultArgs' codType' body'
    StmtVariadic kind (SavedHint m) name -> do
      WeakStmtVariadic kind m name
    StmtForeign foreignList ->
      WeakStmtForeign $ map weakenForeign foreignList

weaken :: TM.Term -> WT.WeakTerm
weaken term =
  case term of
    m :< TM.Var x ->
      m :< WT.Var x
    m :< TM.VarGlobal g argNum ->
      m :< WT.VarGlobal g argNum
    m :< TM.PiIntro attr impArgs expArgs defaultArgs e -> do
      let attr' = weakenAttr attr
      let impArgs' = map weakenTypeBinder impArgs
      let expArgs' = map weakenTypeBinder expArgs
      let defaultArgs' = map (bimap weakenTypeBinder weaken) defaultArgs
      let e' = weaken e
      m :< WT.PiIntro attr' impArgs' expArgs' defaultArgs' e'
    m :< TM.PiElim b e impArgs expArgs -> do
      let e' = weaken e
      let impArgs' = ImpArgs.FullySpecified $ map weakenType impArgs
      let expArgs' = map weaken expArgs
      m :< WT.PiElim b e' impArgs' expArgs' (DefaultArgs.ByKey [])
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      let dataArgs' = map weakenType dataArgs
      let consArgs' = map weaken consArgs
      let attr' = fmap weakenTypeBinder attr
      m :< WT.DataIntro attr' consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets tree -> do
      let (os, es, ts) = unzip3 oets
      let es' = map weaken es
      let ts' = map weakenType ts
      let tree' = weakenDecisionTree tree
      m :< WT.DataElim isNoetic (zip3 os es' ts') tree'
    m :< TM.BoxIntro letSeq e -> do
      m :< WT.BoxIntro (map weakenLet letSeq) (weaken e)
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      let castSeq' = map weakenLet castSeq
      let (mxt', e1') = weakenLet (mxt, e1)
      let uncastSeq' = map weakenLet uncastSeq
      let e2' = weaken e2
      m :< WT.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.CodeIntro e ->
      m :< WT.CodeIntro (weaken e)
    m :< TM.CodeElim e ->
      m :< WT.CodeElim (weaken e)
    m :< TM.TauIntro ty ->
      m :< WT.TauIntro (weakenType ty)
    m :< TM.TauElim mx e1 e2 ->
      m :< WT.TauElim mx (weaken e1) (weaken e2)
    m :< TM.Let opacity mxt e1 e2 ->
      m :< WT.Let (reflectOpacity opacity) (weakenTypeBinder mxt) (weaken e1) (weaken e2)
    m :< TM.Prim prim ->
      m :< WT.Prim (weakenPrimValue prim)
    m :< TM.Magic magic -> do
      m :< WT.Magic (weakenMagic m magic)

weakenType :: TM.Type -> WT.WeakType
weakenType ty =
  case ty of
    m :< TM.Tau ->
      m :< WT.Tau
    m :< TM.TVar x ->
      m :< WT.TVar x
    m :< TM.TVarGlobal g argNum ->
      m :< WT.TVarGlobal g argNum
    m :< TM.TyApp t args ->
      m :< WT.TyApp (weakenType t) (map weakenType args)
    m :< TM.Pi piKind impArgs expArgs defaultArgs t ->
      m :< WT.Pi piKind (map weakenTypeBinder impArgs) (map weakenTypeBinder expArgs) (map (bimap weakenTypeBinder weaken) defaultArgs) (weakenType t)
    m :< TM.Data attr name es -> do
      let es' = map weakenType es
      let attr' = fmap weakenTypeBinder attr
      m :< WT.Data attr' name es'
    m :< TM.Box t ->
      m :< WT.Box (weakenType t)
    m :< TM.BoxNoema t ->
      m :< WT.BoxNoema (weakenType t)
    m :< TM.Code t ->
      m :< WT.Code (weakenType t)
    m :< TM.PrimType pt ->
      m :< WT.PrimType pt
    m :< TM.Void ->
      m :< WT.Void
    m :< TM.Resource dd resourceID unitType discarder copier typeTag -> do
      m :< WT.Resource dd resourceID (weakenType unitType) (weaken discarder) (weaken copier) (weaken typeTag)

weakenMagic :: Hint -> M.Magic BLT.BaseLowType TM.Type TM.Term -> M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm
weakenMagic m magic = do
  case magic of
    M.LowMagic lowMagic ->
      M.WeakMagic $
        M.LowMagic $
          case lowMagic of
            LM.Cast from to value ->
              LM.Cast (weakenType from) (weakenType to) (weaken value)
            LM.Store t unit value pointer ->
              LM.Store (WT.fromBaseLowType m t) (weakenType unit) (weaken value) (weaken pointer)
            LM.Load t pointer ->
              LM.Load (WT.fromBaseLowType m t) (weaken pointer)
            LM.Alloca t size ->
              LM.Alloca (WT.fromBaseLowType m t) (weaken size)
            LM.External domList cod extFunName args varArgs -> do
              let domList' = map (WT.fromBaseLowType m) domList
              let cod' = fmap (WT.fromBaseLowType m) cod
              let varArgs' = map (bimap weaken (WT.fromBaseLowType m)) varArgs
              LM.External domList' cod' extFunName (fmap weaken args) varArgs'
            LM.Global name t ->
              LM.Global name (WT.fromBaseLowType m t)
            LM.OpaqueValue e ->
              LM.OpaqueValue (weaken e)
            LM.CallType func arg1 arg2 ->
              LM.CallType (weaken func) (weaken arg1) (weaken arg2)
    M.GetTypeTag mid typeTagExpr e ->
      M.WeakMagic $ M.GetTypeTag mid (weakenType typeTagExpr) (weakenType e)
    M.GetDataArgs sgl listExpr typeExpr ->
      M.WeakMagic $ M.GetDataArgs sgl (weakenType listExpr) (weakenType typeExpr)
    M.GetConsSize typeExpr ->
      M.WeakMagic $ M.GetConsSize (weakenType typeExpr)
    M.GetWrapperContentType typeExpr ->
      M.WeakMagic $ M.GetWrapperContentType (weakenType typeExpr)
    M.GetVectorContentType sgl typeExpr ->
      M.WeakMagic $ M.GetVectorContentType sgl (weakenType typeExpr)
    M.GetNoemaContentType typeExpr ->
      M.WeakMagic $ M.GetNoemaContentType (weakenType typeExpr)
    M.GetBoxContentType typeExpr ->
      M.WeakMagic $ M.GetBoxContentType (weakenType typeExpr)
    M.GetConstructorArgTypes sgl listExpr typeExpr index ->
      M.WeakMagic $ M.GetConstructorArgTypes sgl (weakenType listExpr) (weakenType typeExpr) (weaken index)
    M.GetConsName textType typeExpr index ->
      M.WeakMagic $ M.GetConsName (weakenType textType) (weakenType typeExpr) (weaken index)
    M.GetConsConstFlag boolType typeExpr index ->
      M.WeakMagic $ M.GetConsConstFlag (weakenType boolType) (weakenType typeExpr) (weaken index)
    M.ShowType textTypeExpr typeExpr ->
      M.WeakMagic $ M.ShowType (weakenType textTypeExpr) (weakenType typeExpr)
    M.TextCons textTypeExpr rune text ->
      M.WeakMagic $ M.TextCons (weakenType textTypeExpr) (weaken rune) (weaken text)
    M.TextUncons mid text ->
      M.WeakMagic $ M.TextUncons mid (weaken text)
    M.CompileError typeExpr msg ->
      M.WeakMagic $ M.CompileError (weakenType typeExpr) (weaken msg)

weakenTypeBinder :: (Hint, Ident, TM.Type) -> (Hint, Ident, WT.WeakType)
weakenTypeBinder (m, x, t) =
  (m, x, weakenType t)

weakenLet :: (BinderF TM.Type, TM.Term) -> (BinderF WT.WeakType, WT.WeakTerm)
weakenLet ((m, x, t), e) =
  ((m, x, weakenType t), weaken e)

weakenAttr :: AttrL.Attr TM.Type -> AttrL.Attr WT.WeakType
weakenAttr AttrL.Attr {lamKind, identity} =
  case lamKind of
    LK.Normal name codType ->
      AttrL.normal' name identity (weakenType codType)
    LK.Fix opacity xt ->
      AttrL.Attr {lamKind = LK.Fix opacity (weakenTypeBinder xt), identity}

weakenPrimValue :: PV.PrimValue TM.Type -> WPV.WeakPrimValue WT.WeakType
weakenPrimValue prim =
  case prim of
    PV.Int t _ integer ->
      WPV.Int (weakenType t) integer
    PV.Float t _ float ->
      WPV.Float (weakenType t) float
    PV.Op op ->
      WPV.Op op
    PV.StaticText t text ->
      WPV.StaticText (weakenType t) text
    PV.Rune r ->
      WPV.Rune r

weakenDecisionTree :: DT.DecisionTree TM.Type TM.Term -> DT.DecisionTree WT.WeakType WT.WeakTerm
weakenDecisionTree tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      let letSeq' = map (bimap weakenTypeBinder weaken) letSeq
      let e' = weaken e
      DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      DT.Unreachable
    DT.Switch (cursorVar, cursor) caseList -> do
      let cursor' = weakenType cursor
      let caseList' = weakenCaseList caseList
      DT.Switch (cursorVar, cursor') caseList'

weakenCaseList :: DT.CaseList TM.Type TM.Term -> DT.CaseList WT.WeakType WT.WeakTerm
weakenCaseList (fallbackClause, clauseList) = do
  let fallbackClause' = weakenDecisionTree fallbackClause
  let clauseList' = map weakenCase clauseList
  (fallbackClause', clauseList')

weakenCase :: DT.Case TM.Type TM.Term -> DT.Case WT.WeakType WT.WeakTerm
weakenCase decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      let cont' = weakenDecisionTree cont
      DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let dataArgs' = map (bimap weakenType weakenType) dataArgs
      let consArgs' = map weakenTypeBinder consArgs
      let cont' = weakenDecisionTree cont
      DT.ConsCase $
        record
          { DT.dataArgs = dataArgs',
            DT.consArgs = consArgs',
            DT.cont = cont'
          }

weakenStmtKindTerm :: StmtKindTerm TM.Type -> StmtKindTerm WT.WeakType
weakenStmtKindTerm stmtKind =
  case stmtKind of
    Define ->
      Define
    Inline ->
      Inline
    Macro ->
      Macro
    MacroInline ->
      MacroInline
    Main t ->
      Main (weakenType t)
    DataIntro dataName dataArgs expConsArgs discriminant -> do
      let dataArgs' = map weakenTypeBinder dataArgs
      let expConsArgs' = map weakenTypeBinder expConsArgs
      DataIntro dataName dataArgs' expConsArgs' discriminant

weakenStmtKindType :: StmtKindType TM.Type -> StmtKindType WT.WeakType
weakenStmtKindType stmtKind =
  case stmtKind of
    Alias ->
      Alias
    AliasOpaque ->
      AliasOpaque
    Data dataName dataArgs consInfoList -> do
      let dataArgs' = map weakenTypeBinder dataArgs
      let (hintList, consNameList, constLikeList, consArgsList, discriminantList) = List.unzip5 consInfoList
      let consArgsList' = map (map weakenTypeBinder) consArgsList
      let consInfoList' = List.zip5 hintList consNameList constLikeList consArgsList' discriminantList
      Data dataName dataArgs' consInfoList'

weakenForeign :: F.Foreign -> WT.WeakForeign
weakenForeign foreignItem@(F.Foreign m _ _ _) =
  fmap (WT.fromBaseLowType m) foreignItem
