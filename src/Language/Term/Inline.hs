module Language.Term.Inline
  ( Handle,
    new,
    inline,
    inlineType,
    DefInfo (..),
    DefKind (..),
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as GensymHandle
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as CreateSymbol
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LamKind qualified as LK
import Language.Common.Literal qualified as L
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.Opacity qualified as O
import Language.Common.PiElimKind qualified as PEK
import Language.Common.PiKind qualified as PK
import Language.Common.VarKind qualified as VK
import Language.Term.Eq qualified as TermEq
import Language.Term.Inline.ConstantFold qualified as ConstantFold
import Language.Term.Inline.Handle
import Language.Term.Inline.Magic qualified as Magic
import Language.Term.PrimValue qualified as PV
import Language.Term.Refresh qualified as Refresh
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Logger.Hint

new :: GensymHandle.Handle -> DefMap -> TypeDefMap -> Hint -> Int -> IO Handle
new gensymHandle dmap typeDefMap location inlineLimit = do
  let substHandle = Subst.new gensymHandle
  let refreshHandle = Refresh.new gensymHandle
  currentStepRef <- liftIO $ newIORef 0
  guardStack <- liftIO $ newIORef []
  macroCallStack <- liftIO $ newIORef []
  return $ Handle {..}

inline :: Handle -> TM.Term -> App TM.Term
inline h e = do
  inline' h e

inlineType :: Handle -> TM.Type -> App TM.Type
inlineType h t = do
  inlineType' h t

incrementStep :: Handle -> IO ()
incrementStep h = do
  let Handle {currentStepRef} = h
  modifyIORef' currentStepRef (+ 1)

detectPossibleInfiniteLoop :: Handle -> App ()
detectPossibleInfiniteLoop h = do
  let Handle {inlineLimit, currentStepRef, location} = h
  currentStep <- liftIO $ readIORef currentStepRef
  when (inlineLimit < currentStep) $ do
    raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit)

inline' :: Handle -> TM.Term -> App TM.Term
inline' h term = do
  detectPossibleInfiniteLoop h
  liftIO $ incrementStep h
  case term of
    _ :< TM.Var {} ->
      return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs defaultArgs e -> do
      impArgs' <- mapM (inlineTypeBinder h) impArgs
      expArgs' <- mapM (inlineTypeBinder h) expArgs
      defaultArgs' <- mapM (bimapM (inlineTypeBinder h) (inline' h)) defaultArgs
      e' <- inline' h e
      case lamKind of
        LK.Fix opacity isDestPassing (mx, k, x, codType) -> do
          codType' <- inlineType' h codType
          let attr' = attr {AttrL.lamKind = LK.Fix opacity isDestPassing (mx, k, x, codType')}
          return (m :< TM.PiIntro attr' impArgs' expArgs' defaultArgs' e')
        LK.Normal mName isDestPassing codType -> do
          codType' <- inlineType' h codType
          let attr' = attr {AttrL.lamKind = LK.Normal mName isDestPassing codType'}
          return (m :< TM.PiIntro attr' impArgs' expArgs' defaultArgs' e')
    m :< TM.PiElim kind e impArgs expArgs defaultArgs -> do
      e' <- inline' h e
      impArgs' <- mapM (inlineType' h) impArgs
      expArgs' <- mapM (inline' h) expArgs
      defaultArgs' <- mapM (traverse (inline' h)) defaultArgs
      let Handle {dmap} = h
      let rebuildWithDefaults defaultArgsFilled =
            m :< TM.PiElim kind e' impArgs' expArgs' (map Just defaultArgsFilled)
      case e' of
        (_ :< TM.PiIntro (AttrL.Attr {lamKind}) impBinders expBinders defBinders body) -> do
          if length impBinders /= length impArgs'
            then return (m :< TM.PiElim kind e' impArgs' expArgs' defaultArgs')
            else do
              let impIds = map (\(_, _, x, _) -> x) impBinders
              let subType = IntMap.fromList $ zip (map Ident.toInt impIds) (map Subst.Type impArgs')
              let defaultArgsRaw = fillDefaultArgs defaultArgs' (map snd defBinders)
              defaultArgsFilled <- liftIO $ mapM (Subst.subst (substHandle h) subType) defaultArgsRaw
              let expParams = expBinders ++ map fst defBinders
              let expArgsAll = expArgs' ++ defaultArgsFilled
              if length expParams /= length expArgsAll
                then return (m :< TM.PiElim kind e' impArgs' expArgs' defaultArgs')
                else
                  if PEK.isNoetic kind || not (canReduceByLamKind lamKind)
                    then return (rebuildWithDefaults defaultArgsFilled)
                    else do
                      let subSelf = selfSubstForLamKind lamKind e'
                      let expIds = map (\(_, _, x, _) -> x) expParams
                      let subTerm = IntMap.fromList $ zip (map Ident.toInt expIds) (map Subst.Term expArgsAll)
                      if all TM.isValue expArgsAll
                        then do
                          let sub = IntMap.unions [subSelf, subType, subTerm]
                          _ :< body' <- liftIO $ Subst.subst (substHandle h) sub body
                          inline' h $ m :< body'
                        else do
                          let sub = IntMap.unions [subSelf, subType]
                          (expParams', _ :< body') <- liftIO $ Subst.subst' (substHandle h) sub expParams body
                          body'' <- liftIO $ Refresh.refresh (refreshHandle h) $ m :< body'
                          inline' h $ bind (zip expParams' expArgsAll) body''
        (_ :< TM.VarGlobal _ dd)
          | Just defInfo <- Map.lookup dd dmap -> do
              let DefInfo {defImpBinders = impParams, defExpBinders = expBinders, defDefaultArgs = defDefaults, defBody = body, codType, defKind} = defInfo
              if length impParams /= length impArgs'
                then return (m :< TM.PiElim kind e' impArgs' expArgs' defaultArgs')
                else do
                  let impIds = map (\(_, _, x, _) -> x) impParams
                  let subType = IntMap.fromList $ zip (map Ident.toInt impIds) (map Subst.Type impArgs')
                  let expParams = expBinders ++ map fst defDefaults
                  let defaultArgsRaw = fillDefaultArgs defaultArgs' (map snd defDefaults)
                  defaultArgsFilled <- liftIO $ mapM (Subst.subst (substHandle h) subType) defaultArgsRaw
                  let expArgsAll = expArgs' ++ defaultArgsFilled
                  if length expParams /= length expArgsAll
                    then return (m :< TM.PiElim kind e' impArgs' expArgs' defaultArgs')
                    else
                      if PEK.isNoetic kind || not (canInlineDefKind defKind)
                        then return (rebuildWithDefaults defaultArgsFilled)
                        else do
                          let tracer = if isMacroDef defKind then withMacroHint h dd m else id
                          case defKind of
                            Macro -> do
                              mSelf <- lookupGuard h dd impArgs'
                              case mSelf of
                                Just selfTerm -> do
                                  return $ m :< TM.PiElim PEK.Normal selfTerm [] expArgsAll []
                                Nothing -> do
                                  (expBinders', body') <- liftIO $ Subst.subst' (substHandle h) subType expParams body
                                  self <- liftIO $ CreateSymbol.newIdentFromText (gensymHandle h) $ "knot-" <> DD.localLocator dd
                                  codType' <- liftIO $ Subst.substType (substHandle h) subType codType
                                  pushGuard h dd impArgs' (m :< TM.Var self)
                                  body'' <- tracer $ liftIO (Refresh.refresh (refreshHandle h) body') >>= inline h
                                  popGuard h
                                  identity <- liftIO $ Gensym.newCount (gensymHandle h)
                                  let selfType = m :< TM.Pi PK.normal [] expBinders' [] codType'
                                  let attr = AttrL.Attr {lamKind = LK.Fix O.Opaque False (m, VK.Normal, self, selfType), identity}
                                  let fun = m :< TM.PiIntro attr [] expBinders' [] body''
                                  return $ m :< TM.PiElim PEK.Normal fun [] expArgsAll []
                            _ -> do
                              if all TM.isValue expArgsAll
                                then do
                                  let expIds = map (\(_, _, x, _) -> x) expParams
                                  let subTerm = IntMap.fromList $ zip (map Ident.toInt expIds) (map Subst.Term expArgsAll)
                                  let sub = IntMap.union subTerm subType
                                  _ :< body' <- liftIO $ Subst.subst (substHandle h) sub body
                                  body'' <- liftIO $ Refresh.refresh (refreshHandle h) $ m :< body'
                                  tracer $ inline' h body''
                                else do
                                  (expParams', _ :< body') <- liftIO $ Subst.subst' (substHandle h) subType expParams body
                                  body'' <- liftIO $ Refresh.refresh (refreshHandle h) $ m :< body'
                                  tracer $ inline' h $ bind (zip expParams' expArgsAll) body''
        (_ :< TM.Prim (PV.Op op))
          | PEK.isNormal kind -> do
              case ConstantFold.evaluatePrimOp m op expArgs' of
                Just result -> do
                  return result
                Nothing ->
                  return (m :< TM.PiElim kind e' impArgs' expArgs' defaultArgs')
        _ ->
          return (m :< TM.PiElim kind e' impArgs' expArgs' defaultArgs')
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (inlineType' h) dataArgs
      consArgs' <- mapM (inline' h) consArgs
      attr' <- inlineAttrDataIntro h attr
      return $ m :< TM.DataIntro attr' consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (inline' h) es
      ts' <- mapM (inlineType' h) ts
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- inlineDecisionTree h decisionTree
          return $ m :< TM.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ letSeq e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Subst.Term es')
              liftIO (Subst.subst (substHandle h) sub (TM.fromLetSeq letSeq e)) >>= inline' h
            DT.Unreachable ->
              return $ m :< TM.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< TM.DataIntro (AttrDI.Attr {..}) _ _ consArgs), oets'') -> do
                  let (newBaseCursorList, cont) = findClause discriminant fallbackTree caseList
                  let newCursorList = zipWith (\(o, t) arg -> (o, arg, t)) newBaseCursorList consArgs
                  let subst = Subst.subst (substHandle h) (IntMap.singleton (Ident.toInt cursor) (Subst.Term e))
                  liftIO (subst $ m :< TM.DataElim isNoetic (oets'' ++ newCursorList) cont) >>= inline' h
                Just (e, oets'')
                  | Just literal <- asLiteralTerm e -> do
                      let subst = Subst.subst (substHandle h) (IntMap.singleton (Ident.toInt cursor) (Subst.Term e))
                      case findLiteralClause literal caseList of
                        Just cont -> do
                          liftIO (subst $ m :< TM.DataElim isNoetic oets'' cont) >>= inline' h
                        Nothing
                          | L.Int value <- literal,
                            Just ([], cont) <- findConsCaseByDisc (D.MakeDiscriminant value) caseList -> do
                              liftIO (subst $ m :< TM.DataElim isNoetic oets'' cont) >>= inline' h
                          | otherwise -> do
                              decisionTree' <- inlineDecisionTree h decisionTree
                              return $ m :< TM.DataElim isNoetic oets' decisionTree'
                _ -> do
                  decisionTree' <- inlineDecisionTree h decisionTree
                  return $ m :< TM.DataElim isNoetic oets' decisionTree'
    m :< TM.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- mapM (inlineTypeBinder h) xts
      es' <- mapM (inline' h) es
      e' <- inline' h e
      return $ m :< TM.BoxIntro (zip xts' es') e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (bimapM (inlineTypeBinder h) (inline' h)) castSeq
      (mxt', e1') <- bimapM (inlineTypeBinder h) (inline' h) (mxt, e1)
      uncastSeq' <- mapM (bimapM (inlineTypeBinder h) (inline' h)) uncastSeq
      e2' <- inline' h e2
      return $ m :< TM.BoxElim castSeq' mxt' e1' uncastSeq' e2'
    m :< TM.CodeIntro e -> do
      e' <- inline' h e
      return $ m :< TM.CodeIntro e'
    m :< TM.CodeElim e -> do
      e' <- inline' h e
      case e' of
        _ :< TM.CodeIntro e'' ->
          inline' h e''
        _ ->
          return $ m :< TM.CodeElim e'
    m :< TM.TauIntro ty -> do
      ty' <- inlineType' h ty
      return $ m :< TM.TauIntro ty'
    m :< TM.TauElim (mx, x) e1 e2 -> do
      e1' <- inline' h e1
      case e1' of
        _ :< TM.TauIntro ty -> do
          let sub = IntMap.singleton (Ident.toInt x) (Subst.Type ty)
          liftIO (Subst.subst (substHandle h) sub e2) >>= inline' h
        _ -> do
          e2' <- inline' h e2
          return $ m :< TM.TauElim (mx, x) e1' e2'
    m :< TM.Let opacity mxt@(_, _, x, _) e1 e2 -> do
      e1' <- inline' h e1
      case opacity of
        O.Clear
          | TM.isValue e1' -> do
              let sub = IntMap.singleton (Ident.toInt x) (Subst.Term e1')
              liftIO (Subst.subst (substHandle h) sub e2) >>= inline' h
        _ -> do
          mxt' <- inlineTypeBinder h mxt
          e2' <- inline' h e2
          return $ m :< TM.Let opacity mxt' e1' e2'
    m :< TM.Prim prim -> do
      case prim of
        PV.Int intType size value -> do
          intType' <- inlineType' h intType
          return $ m :< TM.Prim (PV.Int intType' size value)
        PV.Float floatType size value -> do
          floatType' <- inlineType' h floatType
          return $ m :< TM.Prim (PV.Float floatType' size value)
        PV.Op {} ->
          return term
        PV.StaticString stringType text -> do
          stringType' <- inlineType' h stringType
          return $ m :< TM.Prim (PV.StaticString stringType' text)
        PV.Rune {} ->
          return term
    m :< TM.Magic magic -> do
      case magic of
        M.LowMagic lowMagic -> do
          case lowMagic of
            LM.Cast _ _ e ->
              inline' h e
            _ -> do
              lowMagic' <- inlineLowMagic h lowMagic
              return (m :< TM.Magic (M.LowMagic lowMagic'))
        M.Calloc sizeType num size -> do
          sizeType' <- inlineType' h sizeType
          num' <- inline' h num
          size' <- inline' h size
          return (m :< TM.Magic (M.Calloc sizeType' num' size'))
        M.Malloc sizeType size -> do
          sizeType' <- inlineType' h sizeType
          size' <- inline' h size
          return (m :< TM.Magic (M.Malloc sizeType' size'))
        M.Realloc sizeType ptr size -> do
          sizeType' <- inlineType' h sizeType
          ptr' <- inline' h ptr
          size' <- inline' h size
          return (m :< TM.Magic (M.Realloc sizeType' ptr' size'))
        M.Free unitType ptr -> do
          unitType' <- inlineType' h unitType
          ptr' <- inline' h ptr
          return (m :< TM.Magic (M.Free unitType' ptr'))
        M.InspectType mid _ typeExpr -> do
          typeExpr' <- inlineType' h typeExpr
          Magic.evaluateInspectType h m mid typeExpr' >>= inline' h
        M.EqType moduleID typeExpr1 typeExpr2 -> do
          typeExpr1' <- inlineType' h typeExpr1
          typeExpr2' <- inlineType' h typeExpr2
          Magic.evaluateEqType m moduleID typeExpr1' typeExpr2'
        M.ShowType stringTypeExpr typeExpr -> do
          stringTypeExpr' <- inlineType' h stringTypeExpr
          typeExpr' <- inlineType' h typeExpr
          Magic.evaluateShowType m stringTypeExpr' typeExpr'
        M.StringCons stringTypeExpr rune text -> do
          stringTypeExpr' <- inlineType' h stringTypeExpr
          rune' <- inline' h rune
          text' <- inline' h text
          Magic.evaluateStringCons h m stringTypeExpr' rune' text'
        M.StringUncons mid text -> do
          text' <- inline' h text
          Magic.evaluateStringUncons h m mid text' >>= inline' h
        M.CompileError _ msg -> do
          msg' <- inline' h msg
          Magic.evaluateCompileError h m msg'

inlineType' :: Handle -> TM.Type -> App TM.Type
inlineType' h ty =
  case ty of
    _ :< TM.Tau ->
      return ty
    _ :< TM.TVar {} ->
      return ty
    _ :< TM.TVarGlobal {} ->
      return ty
    m :< TM.TyApp t args -> do
      case t of
        _ :< TM.TVarGlobal _ dd
          | Just typeDefInfo <- Map.lookup dd (typeDefMap h),
            TypeDef.TypeDefInfo {TypeDef.typeDefBinders = binders, TypeDef.typeDefBody = body} <- typeDefInfo,
            length binders == length args -> do
              args' <- mapM (inlineType' h) args
              let binderIds = map (\(_, _, x, _) -> x) binders
              let subType = IntMap.fromList $ zip (map Ident.toInt binderIds) (map Subst.Type args')
              body' <- liftIO $ Subst.substType (substHandle h) subType body
              inlineType' h body'
        _ -> do
          t' <- inlineType' h t
          args' <- mapM (inlineType' h) args
          return $ m :< TM.TyApp t' args'
    m :< TM.Pi piKind impArgs expArgs defaultArgs cod -> do
      impArgs' <- mapM (inlineTypeBinder h) impArgs
      expArgs' <- mapM (inlineTypeBinder h) expArgs
      defaultArgs' <- mapM (inlineTypeBinder h) defaultArgs
      cod' <- inlineType' h cod
      return $ m :< TM.Pi piKind impArgs' expArgs' defaultArgs' cod'
    m :< TM.Data attr name es -> do
      es' <- mapM (inlineType' h) es
      return $ m :< TM.Data attr name es'
    m :< TM.Box t -> do
      t' <- inlineType' h t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- inlineType' h t
      return $ m :< TM.BoxNoema t'
    m :< TM.Code t -> do
      t' <- inlineType' h t
      return $ m :< TM.Code t'
    _ :< TM.PrimType {} ->
      return ty
    _ :< TM.Void ->
      return ty
    m :< TM.Resource dd resourceID -> do
      return $ m :< TM.Resource dd resourceID

inlineLowMagic :: Handle -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> App (LM.LowMagic BLT.BaseLowType TM.Type TM.Term)
inlineLowMagic h lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      from' <- inlineType' h from
      to' <- inlineType' h to
      value' <- inline' h value
      return $ LM.Cast from' to' value'
    LM.Store t unit value pointer -> do
      value' <- inline' h value
      pointer' <- inline' h pointer
      return $ LM.Store t unit value' pointer'
    LM.Load t pointer -> do
      pointer' <- inline' h pointer
      return $ LM.Load t pointer'
    LM.Alloca t size -> do
      size' <- inline' h size
      return $ LM.Alloca t size'
    LM.External domList cod extFunName args varArgs -> do
      args' <- mapM (inline' h) args
      varArgs' <- mapM (bimapM (inline' h) pure) varArgs
      return $ LM.External domList cod extFunName args' varArgs'
    LM.Global name t ->
      return $ LM.Global name t
    LM.OpaqueValue e -> do
      e' <- inline' h e
      return $ LM.OpaqueValue e'
    LM.CallType func arg1 arg2 -> do
      func' <- inline' h func
      arg1' <- inline' h arg1
      arg2' <- inline' h arg2
      return $ LM.CallType func' arg1' arg2'

inlineTypeBinder :: Handle -> BinderF TM.Type -> App (BinderF TM.Type)
inlineTypeBinder h (m, k, x, t) = do
  t' <- inlineType' h t
  return (m, k, x, t')

inlineAttrDataIntro :: Handle -> AttrDI.Attr name (BinderF TM.Type) -> App (AttrDI.Attr name (BinderF TM.Type))
inlineAttrDataIntro h attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- mapM (inlineTypeBinder h) binders
    return (cn, binders', cl)
  return $ attr {AttrDI.consNameList = consNameList'}

inlineDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Type TM.Term ->
  App (DT.DecisionTree TM.Type TM.Term)
inlineDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (inlineTypeBinder h) (inline' h)) letSeq
      e' <- inline' h e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- inlineType' h cursor
      clauseList' <- inlineCaseList h clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

inlineCaseList ::
  Handle ->
  DT.CaseList TM.Type TM.Term ->
  App (DT.CaseList TM.Type TM.Term)
inlineCaseList h (fallbackTree, clauseList) = do
  fallbackTree' <- inlineDecisionTree h fallbackTree
  clauseList' <- mapM (inlineCase h) clauseList
  return (fallbackTree', clauseList')

inlineCase ::
  Handle ->
  DT.Case TM.Type TM.Term ->
  App (DT.Case TM.Type TM.Term)
inlineCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- inlineDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (inlineType' h) dataTerms
      dataTypes' <- mapM (inlineType' h) dataTypes
      consArgs' <- mapM (inlineTypeBinder h) consArgs
      cont' <- inlineDecisionTree h cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = consArgs',
              DT.cont = cont'
            }

findClause ::
  D.Discriminant ->
  DT.DecisionTree TM.Type TM.Term ->
  [DT.Case TM.Type TM.Term] ->
  ([(Ident, TM.Type)], DT.DecisionTree TM.Type TM.Term)
findClause consDisc fallbackTree clauseList =
  case clauseList of
    [] ->
      ([], fallbackTree)
    clause : rest ->
      case DT.findCase consDisc clause of
        Just (consArgs, clauseTree) ->
          (consArgs, clauseTree)
        Nothing ->
          findClause consDisc fallbackTree rest

findLiteralClause ::
  L.Literal ->
  [DT.Case TM.Type TM.Term] ->
  Maybe (DT.DecisionTree TM.Type TM.Term)
findLiteralClause literal clauseList =
  case clauseList of
    [] ->
      Nothing
    clause : rest ->
      case clause of
        DT.LiteralCase _ literal' cont
          | literal == literal' ->
              Just cont
        _ ->
          findLiteralClause literal rest

findConsCaseByDisc ::
  D.Discriminant ->
  [DT.Case TM.Type TM.Term] ->
  Maybe ([BinderF TM.Type], DT.DecisionTree TM.Type TM.Term)
findConsCaseByDisc disc clauseList =
  case clauseList of
    [] ->
      Nothing
    clause : rest ->
      case clause of
        DT.ConsCase (DT.ConsCaseRecord {disc = clauseDisc, consArgs, cont})
          | disc == clauseDisc ->
              Just (consArgs, cont)
        _ ->
          findConsCaseByDisc disc rest

asLiteralTerm :: TM.Term -> Maybe L.Literal
asLiteralTerm term =
  case term of
    _ :< TM.Prim (PV.Int _ _ value) ->
      Just (L.Int value)
    _ :< TM.Prim (PV.Rune rune) ->
      Just (L.Rune rune)
    _ ->
      Nothing

lookupSplit :: Ident -> [(Ident, b, c)] -> Maybe (b, [(Ident, b, c)])
lookupSplit cursor =
  lookupSplit' cursor []

lookupSplit' :: Ident -> [(Ident, b, c)] -> [(Ident, b, c)] -> Maybe (b, [(Ident, b, c)])
lookupSplit' cursor acc oets =
  case oets of
    [] ->
      Nothing
    oet@(o, e, _) : rest ->
      if o == cursor
        then Just (e, reverse acc ++ rest)
        else lookupSplit' cursor (oet : acc) rest

fillDefaultArgs ::
  [Maybe TM.Term] ->
  [TM.Term] ->
  [TM.Term]
fillDefaultArgs overrides defaults = do
  zipWith (flip fromMaybe) overrides defaults

bind :: [(BinderF TM.Type, TM.Term)] -> TM.Term -> TM.Term
bind binder cont =
  case binder of
    [] ->
      cont
    ((m, k, x, t), e1) : rest -> do
      m :< TM.Let O.Clear (m, k, x, t) e1 (bind rest cont)

pushGuard :: Handle -> DD.DefiniteDescription -> [TM.Type] -> TM.Term -> App ()
pushGuard h dd typeArgs selfVar = do
  let entry = GuardEntry dd typeArgs selfVar
  let Handle {guardStack} = h
  liftIO $ modifyIORef' guardStack (entry :)

popGuard :: Handle -> App ()
popGuard h = do
  let Handle {guardStack} = h
  liftIO $ modifyIORef' guardStack (drop 1)

lookupGuard :: Handle -> DD.DefiniteDescription -> [TM.Type] -> App (Maybe TM.Term)
lookupGuard h dd typeArgs = do
  let Handle {guardStack} = h
  stack <- liftIO $ readIORef guardStack
  return $ guardSelf <$> find (matchesGuardEntry dd typeArgs) stack

matchesGuardEntry :: DD.DefiniteDescription -> [TM.Type] -> GuardEntry -> Bool
matchesGuardEntry dd' args entry =
  guardFunction entry == dd' && TermEq.eqTypes (guardTypeArgs entry) args

withMacroHint :: Handle -> DD.DefiniteDescription -> Hint -> App a -> App a
withMacroHint h dd hint action = do
  let Handle {macroCallStack} = h
  liftIO $ modifyIORef' macroCallStack ((dd, hint) :)
  result <- action
  liftIO $ modifyIORef' macroCallStack (drop 1)
  return result

canReduceByLamKind :: LK.LamKindF a -> Bool
canReduceByLamKind lamKind =
  case lamKind of
    LK.Normal {} ->
      True
    LK.Fix O.Clear False _ ->
      True
    _ ->
      False

selfSubstForLamKind :: LK.LamKindF a -> TM.Term -> Subst.Subst
selfSubstForLamKind lamKind selfTerm =
  case lamKind of
    LK.Fix O.Clear False (_, _, selfIdent, _) ->
      IntMap.singleton (Ident.toInt selfIdent) (Subst.Term selfTerm)
    _ ->
      IntMap.empty

isMacroDef :: DefKind -> Bool
isMacroDef dk =
  case dk of
    Macro ->
      True
    MacroInline ->
      True
    _ ->
      False

canInlineDefKind :: DefKind -> Bool
canInlineDefKind dk =
  case dk of
    NoInline ->
      False
    _ ->
      True
