module Language.Term.Inline
  ( Handle,
    new,
    inline,
    DefInfo (..),
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
import Data.Set qualified as S
import Data.Text qualified as T
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as GensymHandle
import Kernel.Common.TypeTag qualified as TypeTag
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseName qualified as BN
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as CreateSymbol
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.ModuleID qualified as MID
import Language.Common.Opacity qualified as O
import Language.Common.PrimNumSize qualified as PNS
import Language.Common.PrimType qualified as PT
import Language.Common.SourceLocator (binaryLocator, typeTagLocator, vectorLocator)
import Language.Common.StrictGlobalLocator qualified as SGL
import Language.Term.Eq qualified as TermEq
import Language.Term.FreeVars qualified as FreeVars
import Language.Term.Prim qualified as P
import Language.Term.PrimValue qualified as PV
import Language.Term.Refresh qualified as Refresh
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Language.Term.Weaken (weaken)
import Language.WeakTerm.ToText
import Logger.Hint

data DefInfo = DefInfo
  { defBinders :: [BinderF TM.Term],
    defBody :: TM.Term,
    codType :: TM.Term,
    isTemplate :: Bool
  }

type DefMap =
  Map.HashMap DD.DefiniteDescription DefInfo

data GuardEntry = GuardEntry
  { guardFunction :: DD.DefiniteDescription,
    guardTypeArgs :: [TM.Term],
    guardSelf :: TM.Term
  }

data Handle = Handle
  { substHandle :: Subst.Handle,
    refreshHandle :: Refresh.Handle,
    dmap :: DefMap,
    inlineLimit :: Int,
    currentStepRef :: IORef Int,
    location :: Hint,
    guardStack :: IORef [GuardEntry],
    gensymHandle :: GensymHandle.Handle
  }

new :: GensymHandle.Handle -> DefMap -> Hint -> Int -> IO Handle
new gensymHandle dmap location inlineLimit = do
  let substHandle = Subst.new gensymHandle
  let refreshHandle = Refresh.new gensymHandle
  currentStepRef <- liftIO $ newIORef 0
  guardStack <- liftIO $ newIORef []
  return $ Handle {..}

inline :: Handle -> TM.Term -> App TM.Term
inline h e = do
  inline' h e

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
    _ :< TM.Tau {} ->
      return term
    _ :< TM.Var {} ->
      return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.Pi piKind impArgs expArgs cod -> do
      impArgs' <- forM impArgs $ \(binder, maybeType) -> do
        binder' <- inlineBinder h binder
        maybeType' <- traverse (inline' h) maybeType
        return (binder', maybeType')
      expArgs' <- mapM (inlineBinder h) expArgs
      cod' <- inline' h cod
      return (m :< TM.Pi piKind impArgs' expArgs' cod')
    m :< TM.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- forM impArgs $ \(binder, maybeType) -> do
        binder' <- inlineBinder h binder
        maybeType' <- traverse (inline' h) maybeType
        return (binder', maybeType')
      expArgs' <- mapM (inlineBinder h) expArgs
      e' <- inline' h e
      case lamKind of
        LK.Fix (mx, x, codType) -> do
          codType' <- inline' h codType
          return (m :< TM.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, codType')}) impArgs' expArgs' e')
        LK.Normal mName codType -> do
          codType' <- inline' h codType
          return (m :< TM.PiIntro (attr {AttrL.lamKind = LK.Normal mName codType'}) impArgs' expArgs' e')
    m :< TM.PiElim isNoetic e impArgs expArgs -> do
      e' <- inline' h e
      impArgs' <- mapM (inline' h) impArgs
      expArgs' <- mapM (inline' h) expArgs
      if isNoetic
        then return $ m :< TM.PiElim isNoetic e' impArgs' expArgs'
        else do
          let Handle {dmap} = h
          case e' of
            (_ :< TM.PiIntro (AttrL.Attr {lamKind = LK.Normal {}}) impArgsBinder expArgsBinder body)
              | xts <- map fst impArgsBinder ++ expArgsBinder,
                length xts == length (impArgs' ++ expArgs') -> do
                  let allArgs = impArgs' ++ expArgs'
                  if all TM.isValue allArgs
                    then do
                      let (_, xs, _) = unzip3 xts
                      let sub = IntMap.fromList $ zip (map Ident.toInt xs) (map Right allArgs)
                      _ :< body' <- liftIO $ Subst.subst (substHandle h) sub body
                      inline' h $ m :< body'
                    else do
                      (xts', _ :< body') <- liftIO $ Subst.subst' (substHandle h) IntMap.empty xts body
                      inline' h $ bind (zip xts' allArgs) (m :< body')
            (_ :< TM.VarGlobal _ dd)
              | Just defInfo <- Map.lookup dd dmap -> do
                  let DefInfo {defBinders = xts, defBody = body, codType, isTemplate} = defInfo
                  let allArgs = impArgs' ++ expArgs'
                  if isTemplate
                    then do
                      mSelf <- lookupGuard h dd allArgs
                      case mSelf of
                        Just selfTerm ->
                          return selfTerm
                        Nothing -> do
                          selfIdent <- liftIO $ CreateSymbol.newIdentFromText (gensymHandle h) "knot"
                          let (_, xs, _) = unzip3 xts
                          let sub = IntMap.fromList $ zip (map Ident.toInt xs) (map Right allArgs)
                          _ :< codType' <- liftIO $ Subst.subst (substHandle h) sub codType
                          pushGuard h dd allArgs (m :< TM.PiElim False (m :< TM.Var selfIdent) [] [])
                          (xts', _ :< body') <- liftIO $ Subst.subst' (substHandle h) IntMap.empty xts body
                          body'' <- liftIO $ Refresh.refresh (refreshHandle h) $ m :< body'
                          body''' <- inline' h $ bind (zip xts' allArgs) body''
                          popGuard h
                          identity <- liftIO $ Gensym.newCount (gensymHandle h)
                          let attr = AttrL.Attr {lamKind = LK.Fix (m, selfIdent, m :< codType'), identity}
                          return $ m :< TM.PiElim False (m :< TM.PiIntro attr [] [] body''') [] []
                    else do
                      if all TM.isValue allArgs
                        then do
                          let (_, xs, _) = unzip3 xts
                          let sub = IntMap.fromList $ zip (map Ident.toInt xs) (map Right allArgs)
                          _ :< body' <- liftIO $ Subst.subst (substHandle h) sub body
                          body'' <- liftIO $ Refresh.refresh (refreshHandle h) $ m :< body'
                          inline' h body''
                        else do
                          (xts', _ :< body') <- liftIO $ Subst.subst' (substHandle h) IntMap.empty xts body
                          body'' <- liftIO $ Refresh.refresh (refreshHandle h) $ m :< body'
                          inline' h $ bind (zip xts' allArgs) body''
            _ ->
              return (m :< TM.PiElim isNoetic e' impArgs' expArgs')
    m :< TM.Data attr name es -> do
      es' <- mapM (inline' h) es
      return $ m :< TM.Data attr name es'
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (inline' h) dataArgs
      consArgs' <- mapM (inline' h) consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (inline' h) es
      ts' <- mapM (inline' h) ts
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- inlineDecisionTree h decisionTree
          return $ m :< TM.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ letSeq e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Right es')
              liftIO (Subst.subst (substHandle h) sub (TM.fromLetSeq letSeq e)) >>= inline' h
            DT.Unreachable ->
              return $ m :< TM.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< TM.DataIntro (AttrDI.Attr {..}) _ _ consArgs), oets'') -> do
                  let (newBaseCursorList, cont) = findClause discriminant fallbackTree caseList
                  let newCursorList = zipWith (\(o, t) arg -> (o, arg, t)) newBaseCursorList consArgs
                  let sub = IntMap.singleton (Ident.toInt cursor) (Right e)
                  dataElim' <- liftIO $ Subst.subst (substHandle h) sub $ m :< TM.DataElim isNoetic (oets'' ++ newCursorList) cont
                  inline' h dataElim'
                _ -> do
                  decisionTree' <- inlineDecisionTree h decisionTree
                  return $ m :< TM.DataElim isNoetic oets' decisionTree'
    m :< TM.Box t -> do
      t' <- inline' h t
      return $ m :< TM.Box t'
    m :< TM.BoxNoema t -> do
      t' <- inline' h t
      return $ m :< TM.BoxNoema t'
    m :< TM.BoxIntro letSeq e -> do
      letSeq' <- mapM (bimapM (inlineBinder h) (inline' h)) letSeq
      e' <- inline' h e
      return $ m :< TM.BoxIntro letSeq' e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (bimapM (inlineBinder h) (inline' h)) castSeq
      e1' <- inline' h e1
      e2' <- inline' h e2
      uncastSeq' <- mapM (bimapM (inlineBinder h) (inline' h)) uncastSeq
      return $ m :< TM.BoxElim castSeq' mxt e1' uncastSeq' e2'
    m :< TM.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- inline' h e1
      case opacity of
        O.Clear
          | TM.isValue e1' -> do
              let sub = IntMap.singleton (Ident.toInt x) (Right e1')
              liftIO (Subst.subst (substHandle h) sub e2) >>= inline' h
        _ -> do
          t' <- inline' h t
          e2' <- inline' h e2
          return $ m :< TM.Let opacity (mx, x, t') e1' e2'
    m :< TM.Prim prim -> do
      case prim of
        P.Type _ ->
          return term
        P.Value pv ->
          case pv of
            PV.Int intType size value -> do
              intType' <- inline h intType
              return $ m :< TM.Prim (P.Value (PV.Int intType' size value))
            PV.Float floatType size value -> do
              floatType' <- inline h floatType
              return $ m :< TM.Prim (P.Value (PV.Float floatType' size value))
            PV.Op {} ->
              return term
            PV.StaticText {} ->
              return term
            PV.Rune {} ->
              return term
    (m :< TM.Magic magic) -> do
      case magic of
        M.LowMagic lowMagic ->
          case lowMagic of
            LM.Cast _ _ e ->
              inline' h e
            _ -> do
              magic' <- traverse (inline' h) magic
              return (m :< TM.Magic magic')
        M.GetTypeTag mid typeTagExpr typeExpr -> do
          typeExpr' <- inline' h typeExpr
          evaluateGetTypeTag m mid typeTagExpr typeExpr'
        M.GetConsSize typeExpr -> do
          typeExpr' <- inline' h typeExpr
          evaluateGetConsSize h m typeExpr'
        M.GetConstructorArgTypes sgl _ typeExpr indexExpr -> do
          typeExpr' <- inline' h typeExpr
          indexExpr' <- inline' h indexExpr
          evaluateGetConstructorArgTypes h m sgl typeExpr' indexExpr'
        M.CompileError msg -> do
          raiseError (location h) $ "compile-error: " <> msg
    _ :< TM.Void ->
      return term
    m :< TM.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- inline' h unitType
      discarder' <- inline' h discarder
      copier' <- inline' h copier
      typeTag' <- inline' h typeTag
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier' typeTag'

evaluateGetTypeTag :: Hint -> MID.ModuleID -> TM.Term -> TM.Term -> App TM.Term
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
    _ :< TM.Prim (P.Type (PT.Int size)) ->
      returnTypeTagIntValue m moduleID (TypeTag.fromIntSize size)
    _ :< TM.Prim (P.Type (PT.Float size)) ->
      returnTypeTagIntValue m moduleID (TypeTag.fromFloatSize size)
    _ :< TM.Prim (P.Type PT.Pointer) ->
      returnTypeTagIntValue m moduleID TypeTag.Pointer
    _ :< TM.Prim (P.Type PT.Rune) ->
      returnTypeTagIntValue m moduleID TypeTag.Rune
    _ :< TM.Resource name _ _ _ _ _ -> do
      let binarySGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = binaryLocator}
      let binaryDD = DD.newByGlobalLocator binarySGL BN.binary
      if name == binaryDD
        then returnTypeTagIntValue m moduleID TypeTag.Binary
        else returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ :< TM.PiElim False (_ :< TM.VarGlobal _ name) [] [] -> do
      let vectorSGL = SGL.StrictGlobalLocator {moduleID, sourceLocator = vectorLocator}
      let vectorDD = DD.newByGlobalLocator vectorSGL BN.vectorInternal
      if name == vectorDD
        then returnTypeTagIntValue m moduleID TypeTag.Vector
        else returnTypeTagIntValue m moduleID TypeTag.Opaque
    _ -> do
      raiseError m $
        "get-type-tag: unable to determine type tag for this type expression. Got: "
          <> toText (weaken typeExpr)

makeConsNameList :: SGL.StrictGlobalLocator -> [(DD.DefiniteDescription, [BinderF TM.Term], IsConstLike)]
makeConsNameList typeTagSGL = do
  flip map BN.typeTagList $ \tag -> (DD.newByGlobalLocator typeTagSGL tag, [], True)

makeAttrDI :: SGL.StrictGlobalLocator -> TypeTag.TypeTag -> AttrDI.Attr DD.DefiniteDescription (BinderF TM.Term)
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

evaluateGetConsSize :: Handle -> Hint -> TM.Term -> App TM.Term
evaluateGetConsSize _h m typeExpr = do
  case typeExpr of
    _ :< TM.Data (AttrD.Attr {AttrD.consNameList}) _ _ -> do
      let consCount = length consNameList
      let intType = m :< TM.Prim (P.Type (PT.Int PNS.IntSize64))
      return $ m :< TM.Prim (P.Value (PV.Int intType PNS.IntSize64 (fromIntegral consCount)))
    _ ->
      raiseError m "get-cons-size: type expression must be a data type"

evaluateGetConstructorArgTypes :: Handle -> Hint -> SGL.StrictGlobalLocator -> TM.Term -> TM.Term -> App TM.Term
evaluateGetConstructorArgTypes _h m sgl typeExpr indexExpr = do
  -- Returns a list of types for the (index)th constructor
  case (typeExpr, indexExpr) of
    (_ :< TM.Data (AttrD.Attr {AttrD.consNameList}) _ _, _ :< TM.Prim (P.Value (PV.Int _ _ indexInt))) -> do
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
          checkConstructorArgNoDependencies m binders
          let types = map (\(_, _, t) -> t) binders
          return $ constructListTerm m sgl types
    (_ :< TM.Data {}, _) ->
      raiseError m "get-constructor-arg-types: index must be an integer literal"
    _ ->
      raiseError m "get-constructor-arg-types: type expression must be a data type"

checkConstructorArgNoDependencies :: Hint -> [BinderF TM.Term] -> App ()
checkConstructorArgNoDependencies hint binders = do
  foldM_ (checkConstructorArgBinder hint) [] binders

checkConstructorArgBinder :: Hint -> [Ident] -> BinderF TM.Term -> App [Ident]
checkConstructorArgBinder hint boundVars (_, x, t) = do
  let fvs = FreeVars.freeVars t
  let illegalDeps = S.intersection (S.fromList boundVars) fvs
  if S.null illegalDeps
    then return (x : boundVars)
    else
      raiseError hint $
        "get-constructor-arg-types: constructor argument types must not depend on previous arguments. "
          <> "Variable(s) "
          <> T.pack (show (S.toList illegalDeps))
          <> " in type of '"
          <> Ident.toText x
          <> "' depend on previous arguments"

constructListTerm :: Hint -> SGL.StrictGlobalLocator -> [TM.Term] -> TM.Term
constructListTerm hint listSgl =
  foldr (constructListCons hint listSgl) (constructListNil hint listSgl)

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

inlineBinder :: Handle -> BinderF TM.Term -> App (BinderF TM.Term)
inlineBinder h (m, x, t) = do
  t' <- inline' h t
  return (m, x, t')

inlineDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Term ->
  App (DT.DecisionTree TM.Term)
inlineDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (inlineBinder h) (inline' h)) letSeq
      e' <- inline' h e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- inline' h cursor
      clauseList' <- inlineCaseList h clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

inlineCaseList ::
  Handle ->
  DT.CaseList TM.Term ->
  App (DT.CaseList TM.Term)
inlineCaseList h (fallbackTree, clauseList) = do
  fallbackTree' <- inlineDecisionTree h fallbackTree
  clauseList' <- mapM (inlineCase h) clauseList
  return (fallbackTree', clauseList')

inlineCase ::
  Handle ->
  DT.Case TM.Term ->
  App (DT.Case TM.Term)
inlineCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- inlineDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (inline' h) dataTerms
      dataTypes' <- mapM (inline' h) dataTypes
      let (ms, xs, ts) = unzip3 consArgs
      ts' <- mapM (inline' h) ts
      cont' <- inlineDecisionTree h cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = zip3 ms xs ts',
              DT.cont = cont'
            }

findClause ::
  D.Discriminant ->
  DT.DecisionTree TM.Term ->
  [DT.Case TM.Term] ->
  ([(Ident, TM.Term)], DT.DecisionTree TM.Term)
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

bind :: [(BinderF TM.Term, TM.Term)] -> TM.Term -> TM.Term
bind binder cont =
  case binder of
    [] ->
      cont
    ((m, x, t), e1) : rest -> do
      m :< TM.Let O.Clear (m, x, t) e1 (bind rest cont)

-- Guard stack operations
pushGuard :: Handle -> DD.DefiniteDescription -> [TM.Term] -> TM.Term -> App ()
pushGuard h dd typeArgs selfVar = do
  let entry = GuardEntry dd typeArgs selfVar
  let Handle {guardStack} = h
  liftIO $ modifyIORef' guardStack (entry :)

popGuard :: Handle -> App ()
popGuard h = do
  let Handle {guardStack} = h
  liftIO $ modifyIORef' guardStack (drop 1)

lookupGuard :: Handle -> DD.DefiniteDescription -> [TM.Term] -> App (Maybe TM.Term)
lookupGuard h dd typeArgs = do
  let Handle {guardStack} = h
  stack <- liftIO $ readIORef guardStack
  return $ guardSelf <$> find (matchesGuardEntry dd typeArgs) stack

matchesGuardEntry :: DD.DefiniteDescription -> [TM.Term] -> GuardEntry -> Bool
matchesGuardEntry dd' args entry =
  guardFunction entry == dd' && TermEq.eqTerms (guardTypeArgs entry) args
