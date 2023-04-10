module Scene.Clarify
  ( clarify,
    registerFoundationalTypes,
  )
where

import Context.App
import Context.Clarify qualified as Clarify
import Context.CompDefinition qualified as CompDefinition
import Context.DataDefinition qualified as DataDefinition
import Context.Enum qualified as Enum
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.Maybe
import Entity.Arity qualified as A
import Entity.BaseName qualified as BN
import Entity.Binder
import Entity.Comp qualified as C
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.EnumCase qualified as EC
import Entity.ExternalName qualified as EN
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Noema qualified as N
import Entity.Opacity qualified as O
import Entity.Prim qualified as P
import Entity.PrimNumSize
import Entity.PrimOp
import Entity.PrimValue qualified as PV
import Entity.Stmt
import Entity.Term qualified as TM
import Entity.Term.Chain qualified as TM
import Entity.Term.FromPrimNum
import Scene.Clarify.Linearize
import Scene.Clarify.Sigma
import Scene.Clarify.Utility
import Scene.Comp.Reduce qualified as Reduce
import Scene.Term.Subst qualified as TM

clarify :: [Stmt] -> App ([C.CompDef], Maybe C.Comp)
clarify defList = do
  mMainDefiniteDescription <- Env.getCurrentSource >>= Locator.getMainDefiniteDescription
  case mMainDefiniteDescription of
    Just mainName -> do
      auxEnv <- withSpecializedCtx $ do
        registerImmediateS4
        registerClosureS4
        Clarify.getAuxEnv
      defList' <- clarifyDefList defList
      mainTerm <- Reduce.reduce $ C.PiElimDownElim (C.VarGlobal mainName (A.Arity 0)) []
      return (defList' ++ Map.toList auxEnv, Just mainTerm)
    Nothing -> do
      defList' <- clarifyDefList defList
      return (defList', Nothing)

clarifyDefList :: [Stmt] -> App [C.CompDef]
clarifyDefList stmtList = do
  (stmtList', auxEnv) <- withSpecializedCtx $ do
    stmtList' <- mapM clarifyDef stmtList
    auxEnv <- Clarify.getAuxEnv >>= reduceDefMap
    return (stmtList', auxEnv)
  CompDefinition.union auxEnv
  stmtList'' <- forM stmtList' $ \(x, (opacity, args, e)) -> do
    e' <- Reduce.reduce e
    CompDefinition.insert x (opacity, args, e')
    return (x, (opacity, args, e'))
  return $ stmtList'' ++ Map.toList auxEnv

registerFoundationalTypes :: App ()
registerFoundationalTypes = do
  auxEnv <- withSpecializedCtx $ do
    registerImmediateS4
    registerClosureS4
    Clarify.getAuxEnv
  forM_ (Map.toList auxEnv) $ uncurry CompDefinition.insert

reduceDefMap :: CompDefinition.DefMap -> App CompDefinition.DefMap
reduceDefMap defMap = do
  forM defMap $ \(opacity, args, e) -> do
    e' <- Reduce.reduce e
    return (opacity, args, e')

withSpecializedCtx :: App a -> App a
withSpecializedCtx action = do
  Clarify.initialize
  action

clarifyDef :: Stmt -> App (DD.DefiniteDescription, (O.Opacity, [Ident], C.Comp))
clarifyDef stmt =
  case stmt of
    StmtDefine stmtKind _ f _ xts _ e -> do
      case stmtKind of
        Data name dataArgs consInfoList -> do
          dataType <- clarifyData name dataArgs consInfoList
          xts' <- dropFst <$> clarifyBinder IntMap.empty xts
          dataType' <- linearize xts' dataType >>= Reduce.reduce
          return (name, (O.Transparent, map fst xts', dataType'))
        _ -> do
          (xts', e') <- clarifyStmtDefine xts e
          return (f, (toLowOpacity stmtKind, xts', e'))
    StmtDefineResource m name discarder copier -> do
      switchValue <- Gensym.newIdentFromText "switchValue"
      value <- Gensym.newIdentFromText "value"
      discarder' <- clarifyTerm IntMap.empty (m :< TM.PiElim discarder [m :< TM.Var value]) >>= Reduce.reduce
      copier' <- clarifyTerm IntMap.empty (m :< TM.PiElim copier [m :< TM.Var value]) >>= Reduce.reduce
      return
        ( name,
          ( O.Transparent,
            [switchValue, value],
            C.EnumElim (C.VarLocal switchValue) copier' [(EC.Int 0, discarder')]
          )
        )

clarifyData :: DD.DefiniteDescription -> [BinderF TM.Term] -> [(DD.DefiniteDescription, [BinderF TM.Term], D.Discriminant)] -> App C.Comp
clarifyData name dataArgs consInfoList = do
  isEnum <- Enum.isMember name
  if isEnum
    then returnEnumS4 name
    else do
      let dataInfo = map (\(_, consArgs, discriminant) -> (discriminant, dataArgs, consArgs)) consInfoList
      dataInfo' <- mapM clarifyDataClause dataInfo
      returnSigmaDataS4 name dataInfo'

clarifyStmtDefine ::
  [BinderF TM.Term] ->
  TM.Term ->
  App ([Ident], C.Comp)
clarifyStmtDefine xts e = do
  xts' <- dropFst <$> clarifyBinder IntMap.empty xts
  e' <- clarifyTerm (TM.insTypeEnv xts IntMap.empty) e
  e'' <- linearize xts' e' >>= Reduce.reduce
  return (map fst xts', e'')

clarifyTerm :: TM.TypeEnv -> TM.Term -> App C.Comp
clarifyTerm tenv term =
  case term of
    _ :< TM.Tau ->
      return returnImmediateS4
    _ :< TM.Var x -> do
      return $ C.UpIntro $ C.VarLocal x
    _ :< TM.VarGlobal x arity -> do
      return $
        C.UpIntro $
          C.SigmaIntro
            [ immediateS4,
              C.SigmaIntro [],
              C.VarGlobal x arity
            ]
    _ :< TM.Pi {} ->
      return returnClosureS4
    _ :< TM.PiIntro kind mxts e -> do
      clarifyLambda tenv kind (TM.chainOf tenv [term]) mxts e
    _ :< TM.PiElim e es -> do
      es' <- mapM (clarifyPlus tenv) es
      e' <- clarifyTerm tenv e
      callClosure e' es'
    _ :< TM.Data name _ -> do
      let name' = DD.getFormDD name
      return $ C.UpIntro $ C.VarGlobal name' A.arityS4
    _ :< TM.DataIntro _ consName disc dataArgs consArgs -> do
      isEnum <- Enum.isMember consName
      if isEnum
        then return $ C.UpIntro $ C.Int (IntSize 64) (D.reify disc)
        else do
          (zs, es, xs) <- fmap unzip3 $ mapM (clarifyPlus tenv) $ dataArgs ++ consArgs
          return $
            bindLet (zip zs es) $
              C.UpIntro $
                C.SigmaIntro $
                  C.Int (IntSize 64) (D.reify disc) : xs
    m :< TM.DataElim isNoetic xets tree -> do
      let (xs, es, _) = unzip3 xets
      let mxts = map (m,,m :< TM.Tau) xs
      es' <- mapM (clarifyTerm tenv) es
      tree' <- clarifyDecisionTree (TM.insTypeEnv mxts tenv) isNoetic IntMap.empty tree
      return $ irreducibleBindLet (zip xs es') tree'
    _ :< TM.Noema {} ->
      return returnImmediateS4
    _ :< TM.Cell {} ->
      return returnImmediateS4
    _ :< TM.CellIntro e -> do
      (contentVarName, content', contentVar) <- clarifyPlus tenv e
      return $
        bindLet [(contentVarName, content')] $
          C.UpIntro (C.SigmaIntro [contentVar])
    _ :< TM.CellElim e -> do
      (contentVarName, content', contentVar) <- clarifyPlus tenv e
      resultVar <- Gensym.newIdentFromText "discriminant"
      return $
        bindLet [(contentVarName, content')] $
          C.SigmaElim True [resultVar] contentVar $
            C.UpIntro (C.VarLocal resultVar)
    m :< TM.Prim prim ->
      case prim of
        P.Type _ ->
          return returnImmediateS4
        P.Value primValue ->
          case primValue of
            PV.Int size l ->
              return $ C.UpIntro (C.Int size l)
            PV.UInt size l ->
              return $ C.UpIntro (C.Int size l)
            PV.Float size l ->
              return $ C.UpIntro (C.Float size l)
            PV.Op op ->
              clarifyPrimOp tenv op m
    _ :< TM.ResourceType name -> do
      return $ C.UpIntro $ C.VarGlobal name A.arityS4
    _ :< TM.Magic der -> do
      clarifyMagic tenv der

type DataArgsMap = IntMap.IntMap [(Ident, TM.Term)]

clarifyDataClause ::
  (D.Discriminant, [BinderF TM.Term], [BinderF TM.Term]) ->
  App (D.Discriminant, [(Ident, C.Comp)])
clarifyDataClause (discriminant, dataArgs, consArgs) = do
  let args = dataArgs ++ consArgs
  args' <- dropFst <$> clarifyBinder IntMap.empty args
  return (discriminant, args')

clarifyDecisionTree :: TM.TypeEnv -> N.IsNoetic -> DataArgsMap -> DT.DecisionTree TM.Term -> App C.Comp
clarifyDecisionTree tenv isNoetic dataArgsMap tree =
  case tree of
    DT.Leaf consumedCursorList cont -> do
      cont' <- clarifyTerm tenv cont
      if isNoetic
        then return cont'
        else tidyCursorList tenv dataArgsMap consumedCursorList cont'
    DT.Unreachable -> do
      return C.Unreachable
    DT.Switch (cursor, t@(m :< _)) (fallbackClause, clauseList) -> do
      let chain = TM.chainOfClauseList tenv m (fallbackClause, clauseList)
      let aligner = alignFreeVariable tenv chain
      fallbackClause' <- clarifyDecisionTree tenv isNoetic dataArgsMap fallbackClause >>= aligner
      (enumCaseList, clauseList') <- mapAndUnzipM (clarifyCase tenv isNoetic dataArgsMap cursor) clauseList
      clauseList'' <- mapM aligner clauseList'
      b <- isEnumType t
      if b
        then return $ C.EnumElim (C.VarLocal cursor) fallbackClause' (zip enumCaseList clauseList'')
        else do
          discriminantVar <- Gensym.newIdentFromText "discriminant"
          return $
            C.UpElim True discriminantVar (C.Primitive (C.Magic (M.Load LT.voidPtr (C.VarLocal cursor)))) $
              C.EnumElim (C.VarLocal discriminantVar) fallbackClause' (zip enumCaseList clauseList'')

isEnumType :: TM.Term -> App Bool
isEnumType term =
  case term of
    _ :< TM.Data dataName _ -> do
      Enum.isMember dataName
    _ :< TM.PiElim (_ :< TM.Data dataName _) _ -> do
      Enum.isMember dataName
    _ :< TM.PiElim (_ :< TM.VarGlobal dataName _) _ -> do
      Enum.isMember dataName
    _ ->
      Throw.raiseCritical' "Clarify.isEnumType"

tidyCursorList :: TM.TypeEnv -> DataArgsMap -> [Ident] -> C.Comp -> App C.Comp
tidyCursorList tenv dataArgsMap consumedCursorList cont =
  case consumedCursorList of
    [] ->
      return cont
    cursor : rest -> do
      case IntMap.lookup (Ident.toInt cursor) dataArgsMap of
        Nothing ->
          error "tidyCursor"
        Just dataArgs -> do
          let (dataArgVars, dataTypes) = unzip dataArgs
          dataTypes' <- mapM (clarifyTerm tenv) dataTypes
          unitVar <- Gensym.newIdentFromText "unit-tidy"
          cont' <- tidyCursorList tenv dataArgsMap rest cont
          linearize (zip dataArgVars dataTypes') $
            C.UpElim True unitVar (C.Primitive (C.Magic (M.External EN.free [C.VarLocal cursor]))) cont'

clarifyCase ::
  TM.TypeEnv ->
  N.IsNoetic ->
  DataArgsMap ->
  Ident ->
  DT.Case TM.Term ->
  App (EC.EnumCase, C.Comp)
clarifyCase tenv isNoetic dataArgsMap cursor (DT.Cons _ consName disc dataArgs consArgs cont) = do
  let (_, dataTypes) = unzip dataArgs
  dataArgVars <- mapM (const $ Gensym.newIdentFromText "dataArg") dataTypes
  let dataArgsMap' = IntMap.insert (Ident.toInt cursor) (zip dataArgVars dataTypes) dataArgsMap
  let consArgs' = map (\(m, x, _) -> (m, x, m :< TM.Tau)) consArgs
  body' <- clarifyDecisionTree (TM.insTypeEnv consArgs' tenv) isNoetic dataArgsMap' cont
  b <- Enum.isMember consName
  if b
    then return (EC.Int (D.reify disc), body')
    else do
      discriminantVar <- Gensym.newIdentFromText "discriminant"
      return
        ( EC.Int (D.reify disc),
          C.SigmaElim
            False
            (discriminantVar : dataArgVars ++ map (\(_, x, _) -> x) consArgs)
            (C.VarLocal cursor)
            body'
        )

alignFreeVariable :: TM.TypeEnv -> [BinderF TM.Term] -> C.Comp -> App C.Comp
alignFreeVariable tenv fvs e = do
  fvs' <- dropFst <$> clarifyBinder tenv fvs
  linearize fvs' e

clarifyMagic :: TM.TypeEnv -> M.Magic TM.Term -> App C.Comp
clarifyMagic tenv der =
  case der of
    M.Cast from to value -> do
      (fromVarName, from', fromVar) <- clarifyPlus tenv from
      (toVarName, to', toVar) <- clarifyPlus tenv to
      (valueVarName, value', valueVar) <- clarifyPlus tenv value
      return $
        bindLet [(fromVarName, from'), (toVarName, to'), (valueVarName, value')] $
          C.Primitive (C.Magic (M.Cast fromVar toVar valueVar))
    M.Store lt pointer value -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus tenv pointer
      (valueVarName, value', valueVar) <- clarifyPlus tenv value
      return $
        bindLet [(pointerVarName, pointer'), (valueVarName, value')] $
          C.Primitive (C.Magic (M.Store lt pointerVar valueVar))
    M.Load lt pointer -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus tenv pointer
      return $
        bindLet [(pointerVarName, pointer')] $
          C.Primitive (C.Magic (M.Load lt pointerVar))
    M.Syscall syscallNum args -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus tenv) args
      return $
        bindLet (zip xs args') $
          C.Primitive (C.Magic (M.Syscall syscallNum xsAsVars))
    M.External extFunName args -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus tenv) args
      return $
        bindLet (zip xs args') $
          C.Primitive (C.Magic (M.External extFunName xsAsVars))

clarifyLambda ::
  TM.TypeEnv ->
  LK.LamKindF TM.Term ->
  [BinderF TM.Term] ->
  [BinderF TM.Term] ->
  TM.Term ->
  App C.Comp
clarifyLambda tenv kind fvs mxts e@(m :< _) = do
  case kind of
    LK.Fix (_, recFuncName, _) -> do
      liftedName <- Locator.attachCurrentLocator $ BN.lambdaName $ Ident.toInt recFuncName
      let appArgs = fvs ++ mxts
      let appArgs' = map (\(mx, x, _) -> mx :< TM.Var x) appArgs
      let arity = A.fromInt $ length appArgs'
      let lamApp = m :< TM.PiIntro (LK.Normal O.Transparent) mxts (m :< TM.PiElim (m :< TM.VarGlobal liftedName arity) appArgs')
      liftedBody <- TM.subst (IntMap.fromList [(Ident.toInt recFuncName, Right lamApp)]) e
      (liftedArgs, liftedBody') <- clarifyStmtDefine appArgs liftedBody
      Clarify.insertToAuxEnv liftedName (O.Opaque, liftedArgs, liftedBody')
      clarifyTerm tenv lamApp
    LK.Normal opacity -> do
      e' <- clarifyTerm (TM.insTypeEnv (catMaybes [LK.fromLamKind kind] ++ mxts) tenv) e
      returnClosure tenv opacity fvs mxts e'

newClosureNames :: App ((Ident, C.Value), Ident, (Ident, C.Value), (Ident, C.Value))
newClosureNames = do
  closureVarInfo <- Gensym.newValueVarLocalWith "closure"
  typeVarName <- Gensym.newIdentFromText "exp"
  envVarInfo <- Gensym.newValueVarLocalWith "env"
  lamVarInfo <- Gensym.newValueVarLocalWith "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

clarifyPlus :: TM.TypeEnv -> TM.Term -> App (Ident, C.Comp, C.Value)
clarifyPlus tenv e = do
  e' <- clarifyTerm tenv e
  (varName, var) <- Gensym.newValueVarLocalWith "var"
  return (varName, e', var)

clarifyBinder :: TM.TypeEnv -> [BinderF TM.Term] -> App [(Hint, Ident, C.Comp)]
clarifyBinder tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm tenv t
      xts' <- clarifyBinder (IntMap.insert (Ident.toInt x) t tenv) xts
      return $ (m, x, t') : xts'

clarifyPrimOp :: TM.TypeEnv -> PrimOp -> Hint -> App C.Comp
clarifyPrimOp tenv op m = do
  let (domList, _) = getTypeInfo op
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- mapAndUnzipM (const (Gensym.newValueVarLocalWith "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  returnClosure tenv O.Transparent [] mxts $ C.Primitive (C.PrimOp op varList)

returnClosure ::
  TM.TypeEnv ->
  O.Opacity ->
  [BinderF TM.Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF TM.Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  C.Comp -> -- the `e` in `lam (x1, ..., xn). e`
  App C.Comp
returnClosure tenv opacity fvs xts e = do
  fvs'' <- dropFst <$> clarifyBinder tenv fvs
  xts'' <- dropFst <$> clarifyBinder tenv xts
  fvEnvSigma <- closureEnvS4 $ map Right fvs''
  let fvEnv = C.SigmaIntro (map (\(x, _) -> C.VarLocal x) fvs'')
  let arity = A.fromInt $ length xts'' + 1 -- arity == count(xts) + env
  i <- Gensym.newCount
  name <- Locator.attachCurrentLocator $ BN.lambdaName i
  registerClosure name opacity xts'' fvs'' e
  return $ C.UpIntro $ C.SigmaIntro [fvEnvSigma, fvEnv, C.VarGlobal name arity]

registerClosure ::
  DD.DefiniteDescription ->
  O.Opacity ->
  [(Ident, C.Comp)] ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  App ()
registerClosure name opacity xts1 xts2 e = do
  e' <- linearize (xts2 ++ xts1) e
  (envVarName, envVar) <- Gensym.newValueVarLocalWith "env"
  let args = map fst xts1 ++ [envVarName]
  body <- Reduce.reduce $ C.SigmaElim True (map fst xts2) envVar e'
  Clarify.insertToAuxEnv name (opacity, args, body)

callClosure :: C.Comp -> [(Ident, C.Comp, C.Value)] -> App C.Comp
callClosure e zexes = do
  let (zs, es', xs) = unzip3 zexes
  ((closureVarName, closureVar), typeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames
  return $
    bindLet
      ((closureVarName, e) : zip zs es')
      ( C.SigmaElim
          True
          [typeVarName, envVarName, lamVarName]
          closureVar
          (C.PiElimDownElim lamVar (xs ++ [envVar]))
      )

dropFst :: [(a, b, c)] -> [(b, c)]
dropFst xyzs = do
  let (_, ys, zs) = unzip3 xyzs
  zip ys zs
