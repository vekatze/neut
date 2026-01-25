module Language.Term.Refresh
  ( Handle,
    new,
    refresh,
    refreshType,
  )
where

import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Term.Term qualified as TM

newtype Handle = Handle
  { gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> Handle
new gensymHandle = do
  Handle {..}

refresh :: Handle -> TM.Term -> IO TM.Term
refresh h term =
  case term of
    _ :< TM.Var {} ->
      return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs expArgs defaultArgs e -> do
      newLamID <- liftIO $ Gensym.newCount (gensymHandle h)
      case lamKind of
        LK.Fix opacity xt -> do
          impArgs' <- refreshTypeBinder h impArgs
          defaultArgs' <- refreshDefaultArgs h defaultArgs
          expArgs' <- refreshTypeBinder h expArgs
          [xt'] <- refreshTypeBinder h [xt]
          e' <- refresh h e
          let fixAttr = AttrL.Attr {lamKind = LK.Fix opacity xt', identity = newLamID}
          return (m :< TM.PiIntro fixAttr impArgs' expArgs' defaultArgs' e')
        LK.Normal name codType -> do
          impArgs' <- refreshTypeBinder h impArgs
          defaultArgs' <- refreshDefaultArgs h defaultArgs
          expArgs' <- refreshTypeBinder h expArgs
          codType' <- refreshType h codType
          e' <- refresh h e
          let lamAttr = AttrL.Attr {lamKind = LK.Normal name codType', identity = newLamID}
          return (m :< TM.PiIntro lamAttr impArgs' expArgs' defaultArgs' e')
    m :< TM.PiElim b e impArgs expArgs defaultArgs -> do
      e' <- refresh h e
      impArgs' <- mapM (refreshType h) impArgs
      expArgs' <- mapM (refresh h) expArgs
      defaultArgs' <- mapM (traverse (refresh h)) defaultArgs
      return (m :< TM.PiElim b e' impArgs' expArgs' defaultArgs')
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (refreshType h) dataArgs
      consArgs' <- mapM (refresh h) consArgs
      attr' <- refreshAttrDataIntro h attr
      return $ m :< TM.DataIntro attr' consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (refresh h) es
      let binder = zipWith (\o t -> (m, o, t)) os ts
      (binder', decisionTree') <- refresh'' h binder decisionTree
      let (_, os', ts') = unzip3 binder'
      return $ m :< TM.DataElim isNoetic (zip3 os' es' ts') decisionTree'
    m :< TM.BoxIntro letSeq e -> do
      letSeq' <- mapM (refreshLet h) letSeq
      e' <- refresh h e
      return $ m :< TM.BoxIntro letSeq' e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (refreshLet h) castSeq
      (mxt', e1') <- refreshLet h (mxt, e1)
      uncastSeq' <- mapM (refreshLet h) uncastSeq
      e2' <- refresh h e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.CodeIntro e -> do
      e' <- refresh h e
      return $ m :< TM.CodeIntro e'
    m :< TM.CodeElim e -> do
      e' <- refresh h e
      return $ m :< TM.CodeElim e'
    m :< TM.TauIntro ty -> do
      ty' <- refreshType h ty
      return $ m :< TM.TauIntro ty'
    m :< TM.TauElim mx e1 e2 -> do
      e1' <- refresh h e1
      e2' <- refresh h e2
      return $ m :< TM.TauElim mx e1' e2'
    m :< TM.Let opacity mxt e1 e2 -> do
      e1' <- refresh h e1
      ([mxt'], e2') <- refresh' h [mxt] e2
      return $ m :< TM.Let opacity mxt' e1' e2'
    _ :< TM.Prim _ ->
      return term
    m :< TM.Magic der -> do
      der' <- refreshMagic h der
      return (m :< TM.Magic der')

refreshMagic :: Handle -> M.Magic BLT.BaseLowType TM.Type TM.Term -> IO (M.Magic BLT.BaseLowType TM.Type TM.Term)
refreshMagic h magic =
  case magic of
    M.LowMagic lowMagic -> do
      lowMagic' <- refreshLowMagic h lowMagic
      return $ M.LowMagic lowMagic'
    M.GetTypeTag mid typeTagExpr typeExpr -> do
      typeTagExpr' <- refreshType h typeTagExpr
      typeExpr' <- refreshType h typeExpr
      return $ M.GetTypeTag mid typeTagExpr' typeExpr'
    M.GetDataArgs sgl listExpr typeExpr -> do
      listExpr' <- refreshType h listExpr
      typeExpr' <- refreshType h typeExpr
      return $ M.GetDataArgs sgl listExpr' typeExpr'
    M.GetConsSize typeExpr -> do
      typeExpr' <- refreshType h typeExpr
      return $ M.GetConsSize typeExpr'
    M.GetBoxContentType typeExpr -> do
      typeExpr' <- refreshType h typeExpr
      return $ M.GetBoxContentType typeExpr'
    M.GetConstructorArgTypes sgl listExpr typeExpr index -> do
      listExpr' <- refreshType h listExpr
      typeExpr' <- refreshType h typeExpr
      index' <- refresh h index
      return $ M.GetConstructorArgTypes sgl listExpr' typeExpr' index'
    M.GetConsName textType typeExpr index -> do
      textType' <- refreshType h textType
      typeExpr' <- refreshType h typeExpr
      index' <- refresh h index
      return $ M.GetConsName textType' typeExpr' index'
    M.GetConsConstFlag boolType typeExpr index -> do
      boolType' <- refreshType h boolType
      typeExpr' <- refreshType h typeExpr
      index' <- refresh h index
      return $ M.GetConsConstFlag boolType' typeExpr' index'
    M.ShowType textTypeExpr typeExpr -> do
      textTypeExpr' <- refreshType h textTypeExpr
      typeExpr' <- refreshType h typeExpr
      return $ M.ShowType textTypeExpr' typeExpr'
    M.TextCons textTypeExpr rune text -> do
      textTypeExpr' <- refreshType h textTypeExpr
      rune' <- refresh h rune
      text' <- refresh h text
      return $ M.TextCons textTypeExpr' rune' text'
    M.TextUncons mid text -> do
      text' <- refresh h text
      return $ M.TextUncons mid text'
    M.CompileError typeExpr msg -> do
      typeExpr' <- refreshType h typeExpr
      msg' <- refresh h msg
      return $ M.CompileError typeExpr' msg'

refreshLowMagic :: Handle -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> IO (LM.LowMagic BLT.BaseLowType TM.Type TM.Term)
refreshLowMagic h lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- refreshType h from
      to' <- refreshType h to
      value' <- refresh h value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      value' <- refresh h value
      pointer' <- refresh h pointer
      return $ LM.Store t unit value' pointer'
    LM.Load t pointer -> do
      pointer' <- refresh h pointer
      return $ LM.Load t pointer'
    LM.Alloca t size -> do
      size' <- refresh h size
      return $ LM.Alloca t size'
    LM.External domList cod extFunName args varArgs -> do
      args' <- mapM (refresh h) args
      varArgs' <-
        mapM
          ( \(arg, typ) -> do
              arg' <- refresh h arg
              return (arg', typ)
          )
          varArgs
      return $ LM.External domList cod extFunName args' varArgs'
    LM.Global name t ->
      return $ LM.Global name t
    LM.OpaqueValue e -> do
      e' <- refresh h e
      return $ LM.OpaqueValue e'
    LM.CallType func arg1 arg2 -> do
      func' <- refresh h func
      arg1' <- refresh h arg1
      arg2' <- refresh h arg2
      return $ LM.CallType func' arg1' arg2'

refreshType :: Handle -> TM.Type -> IO TM.Type
refreshType h ty =
  case ty of
    _ :< TM.Tau ->
      return ty
    _ :< TM.TVar {} ->
      return ty
    _ :< TM.TVarGlobal {} ->
      return ty
    m :< TM.TyApp t args -> do
      t' <- refreshType h t
      args' <- mapM (refreshType h) args
      return $ m :< TM.TyApp t' args'
    m :< TM.Pi piKind impArgs expArgs defaultArgs t -> do
      impArgs' <- refreshTypeBinder h impArgs
      expArgs' <- refreshTypeBinder h expArgs
      defaultArgs' <- refreshTypeBinder h defaultArgs
      t' <- refreshType h t
      return (m :< TM.Pi piKind impArgs' expArgs' defaultArgs' t')
    m :< TM.Data attr name es -> do
      es' <- mapM (refreshType h) es
      attr' <- refreshAttrData h attr
      return $ m :< TM.Data attr' name es'
    m :< TM.Box t -> do
      t' <- refreshType h t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- refreshType h t
      return $ m :< TM.BoxNoema t'
    m :< TM.Code t -> do
      t' <- refreshType h t
      return $ m :< TM.Code t'
    _ :< TM.PrimType {} ->
      return ty
    _ :< TM.Void ->
      return ty
    m :< TM.Resource dd resourceID -> do
      return $ m :< TM.Resource dd resourceID

refreshTypeBinder ::
  Handle ->
  [BinderF TM.Type] ->
  IO [BinderF TM.Type]
refreshTypeBinder h binder =
  case binder of
    [] -> do
      return []
    ((m, x, t) : xts) -> do
      t' <- refreshType h t
      xts' <- refreshTypeBinder h xts
      return ((m, x, t') : xts')

refreshLet ::
  Handle ->
  (BinderF TM.Type, TM.Term) ->
  IO (BinderF TM.Type, TM.Term)
refreshLet h ((m, x, t), e) = do
  t' <- refreshType h t
  e' <- refresh h e
  return ((m, x, t'), e')

refresh' ::
  Handle ->
  [BinderF TM.Type] ->
  TM.Term ->
  IO ([BinderF TM.Type], TM.Term)
refresh' h binder e =
  case binder of
    [] -> do
      e' <- refresh h e
      return ([], e')
    ((m, x, t) : xts) -> do
      t' <- refreshType h t
      (xts', e') <- refresh' h xts e
      return ((m, x, t') : xts', e')

refresh'' ::
  Handle ->
  [BinderF TM.Type] ->
  DT.DecisionTree TM.Type TM.Term ->
  IO ([BinderF TM.Type], DT.DecisionTree TM.Type TM.Term)
refresh'' h binder decisionTree =
  case binder of
    [] -> do
      decisionTree' <- refreshDecisionTree h decisionTree
      return ([], decisionTree')
    ((m, x, t) : xts) -> do
      t' <- refreshType h t
      (xts', e') <- refresh'' h xts decisionTree
      return ((m, x, t') : xts', e')

refreshBinder1 ::
  Handle ->
  (BinderF TM.Type, TM.Term) ->
  IO (BinderF TM.Type, TM.Term)
refreshBinder1 h ((m, x, t), e) = do
  e' <- refresh h e
  t' <- refreshType h t
  return ((m, x, t'), e')

refreshLetSeq ::
  Handle ->
  [(BinderF TM.Type, TM.Term)] ->
  IO [(BinderF TM.Type, TM.Term)]
refreshLetSeq h letSeq = do
  case letSeq of
    [] ->
      return []
    letPair : rest -> do
      letPair' <- refreshBinder1 h letPair
      rest' <- refreshLetSeq h rest
      return (letPair' : rest')

refreshDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Type TM.Term ->
  IO (DT.DecisionTree TM.Type TM.Term)
refreshDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- refreshLetSeq h letSeq
      e' <- refresh h e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return tree
    DT.Switch (cursorVar, cursor) caseList -> do
      cursor' <- refreshType h cursor
      caseList' <- refreshCaseList h caseList
      return $ DT.Switch (cursorVar, cursor') caseList'

refreshCaseList ::
  Handle ->
  DT.CaseList TM.Type TM.Term ->
  IO (DT.CaseList TM.Type TM.Term)
refreshCaseList h (fallbackClause, clauseList) = do
  fallbackClause' <- refreshDecisionTree h fallbackClause
  clauseList' <- mapM (refreshCase h) clauseList
  return (fallbackClause', clauseList')

refreshCase ::
  Handle ->
  DT.Case TM.Type TM.Term ->
  IO (DT.Case TM.Type TM.Term)
refreshCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- refreshDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (refreshType h) dataTerms
      dataTypes' <- mapM (refreshType h) dataTypes
      (consArgs', cont') <- refresh'' h consArgs cont
      return $
        DT.ConsCase $
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

refreshDefaultArgs ::
  Handle ->
  [(BinderF TM.Type, TM.Term)] ->
  IO [(BinderF TM.Type, TM.Term)]
refreshDefaultArgs h binderList =
  case binderList of
    [] -> do
      return []
    ((binder, defaultValue) : rest) -> do
      [binder'] <- refreshTypeBinder h [binder]
      defaultValue' <- refresh h defaultValue
      rest' <- refreshDefaultArgs h rest
      return ((binder', defaultValue') : rest')

refreshAttrData :: Handle -> AttrD.Attr name (BinderF TM.Type) -> IO (AttrD.Attr name (BinderF TM.Type))
refreshAttrData h attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <-
    mapM
      ( \(cn, binders, cl) -> do
          binders' <-
            mapM
              ( \(mx, x, t) -> do
                  t' <- refreshType h t
                  return (mx, x, t')
              )
              binders
          return (cn, binders', cl)
      )
      consNameList
  return $ attr {AttrD.consNameList = consNameList'}

refreshAttrDataIntro :: Handle -> AttrDI.Attr name (BinderF TM.Type) -> IO (AttrDI.Attr name (BinderF TM.Type))
refreshAttrDataIntro h attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <-
    mapM
      ( \(cn, binders, cl) -> do
          binders' <-
            mapM
              ( \(mx, x, t) -> do
                  t' <- refreshType h t
                  return (mx, x, t')
              )
              binders
          return (cn, binders', cl)
      )
      consNameList
  return $ attr {AttrDI.consNameList = consNameList'}
