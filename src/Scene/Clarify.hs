module Scene.Clarify
  ( clarify,
    Context,
  )
where

import qualified Context.CompDefinition as CompDefinition
import qualified Context.DataDefinition as DataDefinition
import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap as IntMap
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace
import qualified Entity.Arity as A
import qualified Entity.BaseName as BN
import Entity.Binder
import qualified Entity.Comp as C
import Entity.Comp.FreeVars
import qualified Entity.Comp.Reduce as Reduce
import Entity.Comp.Subst
import qualified Entity.DecisionTree as DT
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import qualified Entity.EnumCase as EC
import qualified Entity.ExternalName as EN
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.LowType as LT
import qualified Entity.Magic as M
import qualified Entity.Opacity as O
import qualified Entity.Prim as P
import Entity.PrimNumSize
import Entity.PrimOp
import qualified Entity.PrimValue as PV
import qualified Entity.Source as Source
import Entity.Stmt
import qualified Entity.Term as TM
import qualified Entity.Term.Chain as TM
import Entity.Term.FromPrimNum
import qualified Entity.Term.Weaken as TM
import qualified Entity.WeakTerm.ToText as WT
import qualified Scene.Clarify.Context as Clarify
import Scene.Clarify.Linearize
import Scene.Clarify.Sigma
import Scene.Clarify.Utility

class
  ( Clarify.Context m,
    Reduce.Context m,
    Throw.Context m,
    Log.Context m,
    DataDefinition.Context m
  ) =>
  Context m

clarify :: Context m => Source.Source -> [Stmt] -> m ([C.CompDef], Maybe C.Comp)
clarify source defList = do
  mMainDefiniteDescription <- Locator.getMainDefiniteDescription source
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

clarifyDefList :: Context m => [Stmt] -> m [C.CompDef]
clarifyDefList stmtList = do
  (stmtList', auxEnv) <- withSpecializedCtx $ do
    stmtList' <- mapM clarifyDef stmtList
    auxEnv <- Clarify.getAuxEnv >>= reduceDefMap
    return (stmtList', auxEnv)
  CompDefinition.union auxEnv
  stmtList'' <- forM stmtList' $ \(x, (opacity, args, e)) -> do
    e' <- Reduce.reduce e
    -- Log.printNote' $ DD.reify x
    -- Log.printNote' $ T.pack $ show args
    -- Log.printNote' $ T.pack $ show e
    return (x, (opacity, args, e'))
  forM_ stmtList'' $ uncurry CompDefinition.insert
  return $ stmtList'' ++ Map.toList auxEnv

reduceDefMap :: Context m => CompDefinition.DefMap -> m CompDefinition.DefMap
reduceDefMap defMap = do
  forM defMap $ \(opacity, args, e) -> do
    e' <- Reduce.reduce e
    return (opacity, args, e')

withSpecializedCtx :: Context m => m a -> m a
withSpecializedCtx action = do
  Clarify.initialize
  action

clarifyDef :: Context m => Stmt -> m (DD.DefiniteDescription, (O.Opacity, [Ident], C.Comp))
clarifyDef stmt =
  case stmt of
    StmtDefine stmtKind _ f _ xts _ e -> do
      e' <- clarifyTerm (TM.insTypeEnv xts IntMap.empty) e
      xts' <- dropFst <$> clarifyBinder IntMap.empty xts
      e'' <- linearize xts' e' >>= Reduce.reduce
      return (f, (toOpacity stmtKind, map fst xts', e''))

clarifyTerm :: Context m => TM.TypeEnv -> TM.Term -> m C.Comp
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
      clarifyLambda tenv kind mxts e $ TM.chainOf tenv [term]
    _ :< TM.PiElim e es -> do
      es' <- mapM (clarifyPlus tenv) es
      e' <- clarifyTerm tenv e
      callClosure e' es'
    m :< TM.Data name _ -> do
      mDataInfo <- DataDefinition.lookup name
      case mDataInfo of
        Nothing ->
          Throw.raiseCritical m "mDataInfo"
        Just dataInfo -> do
          dataInfo' <- mapM clarifyDataClause dataInfo
          returnSigmaDataS4 name dataInfo'
    _ :< TM.DataIntro _ _ disc dataArgs consArgs -> do
      (zs, es, xs) <- fmap unzip3 $ mapM (clarifyPlus tenv) $ dataArgs ++ consArgs
      return $
        bindLet (zip zs es) $
          C.UpIntro $
            C.SigmaIntro $
              C.Int (IntSize 64) (D.reify disc) : xs
    m :< TM.DataElim xets tree -> do
      let (xs, es, _) = unzip3 xets
      let mxts = map (m,,m :< TM.Tau) xs
      es' <- mapM (clarifyTerm tenv) es
      tree' <- clarifyDecisionTree (TM.insTypeEnv mxts tenv) IntMap.empty tree
      return $ bindLet (zip xs es') tree'
    _ :< TM.Sigma {} -> do
      return returnClosureS4
    m :< TM.SigmaIntro es -> do
      k <- Gensym.newIdentFromText "sigma"
      clarifyTerm tenv $
        m
          :< TM.PiIntro
            LK.Normal
            [(m, k, m :< TM.Pi [] (m :< TM.Tau))]
            (m :< TM.PiElim (m :< TM.Var k) es)
    m :< TM.SigmaElim xts e1 e2 -> do
      clarifyTerm tenv $ m :< TM.PiElim e1 [m :< TM.PiIntro LK.Normal xts e2]
    m :< TM.Let mxt e1 e2 -> do
      clarifyTerm tenv $ m :< TM.PiElim (m :< TM.PiIntro LK.Normal [mxt] e2) [e1]
    m :< TM.Prim prim ->
      case prim of
        P.Type _ ->
          return returnImmediateS4
        P.Value primValue ->
          case primValue of
            PV.Int size l ->
              return $ C.UpIntro (C.Int size l)
            PV.Float size l ->
              return $ C.UpIntro (C.Float size l)
            PV.Op op ->
              clarifyPrimOp tenv op m
    _ :< TM.Enum {} ->
      return returnImmediateS4
    _ :< TM.EnumIntro label ->
      return $ C.UpIntro $ C.EnumIntro label
    _ :< TM.EnumElim {} -> do
      -- _ :< TM.EnumElim (e, _) bs -> do
      -- let (enumCaseList, es) = unzip bs
      -- let fvs = TM.chainOf tenv es
      -- es' <- (mapM (clarifyTerm tenv) >=> alignFreeVariables tenv fvs) es
      -- (y, e', yVar) <- clarifyPlus tenv e
      error "enum-elim"
    -- return $ bindLet [(y, e')] $ C.EnumElim yVar (zip (map forgetHint enumCaseList) es')
    _ :< TM.Magic der -> do
      clarifyMagic tenv der

type DataArgsMap = IntMap.IntMap [(Ident, TM.Term)]

clarifyDataClause ::
  Context m =>
  (D.Discriminant, [BinderF TM.Term], [BinderF TM.Term]) ->
  m (D.Discriminant, [(Ident, C.Comp)])
clarifyDataClause (discriminant, dataArgs, consArgs) = do
  let args = dataArgs ++ consArgs
  args' <- dropFst <$> clarifyBinder IntMap.empty args
  return (discriminant, args')

clarifyDecisionTree :: Context m => TM.TypeEnv -> DataArgsMap -> DT.DecisionTree TM.Term -> m C.Comp
clarifyDecisionTree tenv dataArgsMap tree =
  case tree of
    DT.Leaf consumedCursorList cont -> do
      cont' <- clarifyTerm tenv cont
      tidyCursorList tenv dataArgsMap consumedCursorList cont'
    DT.Unreachable -> do
      return C.Unreachable
    DT.Switch (cursor, m :< _) (fallbackClause, clauseList) -> do
      let chain = TM.chainOfClauseList tenv m (fallbackClause, clauseList)
      -- p $ "chain: " <> show (map (\(_, x, _) -> x) chain)
      let aligner = alignFreeVariable tenv chain
      fallbackClause' <- clarifyDecisionTree tenv dataArgsMap fallbackClause >>= aligner
      (enumCaseList, clauseList') <- mapAndUnzipM (clarifyCase tenv dataArgsMap cursor) clauseList
      clauseList'' <- mapM aligner clauseList'
      discriminantVar <- Gensym.newIdentFromText "discriminant"
      return $
        C.UpElim discriminantVar (C.Primitive (C.Magic (M.Load LT.voidPtr (C.VarLocal cursor)))) $
          C.EnumElim (C.VarLocal discriminantVar) fallbackClause' (zip enumCaseList clauseList'')

tidyCursorList :: Context m => TM.TypeEnv -> DataArgsMap -> [Ident] -> C.Comp -> m C.Comp
tidyCursorList tenv dataArgsMap consumedCursorList cont =
  case consumedCursorList of
    [] ->
      return cont
    cursor : rest -> do
      cont' <- tidyCursorList tenv dataArgsMap rest cont
      tidyCursor tenv dataArgsMap cursor cont'

tidyCursor :: Context m => TM.TypeEnv -> DataArgsMap -> Ident -> C.Comp -> m C.Comp
tidyCursor tenv dataArgsMap consumedCursor cont =
  case IntMap.lookup (Ident.toInt consumedCursor) dataArgsMap of
    Nothing ->
      error "tidyCursor"
    Just dataArgs -> do
      let (dataArgVars, dataTypes) = unzip dataArgs
      dataTypes' <- mapM (clarifyTerm tenv) dataTypes
      unitVar <- Gensym.newIdentFromText "unit-tidy"
      linearize (zip dataArgVars dataTypes') $
        C.UpElim unitVar (C.Primitive (C.Magic (M.External EN.free [C.VarLocal consumedCursor]))) cont

clarifyCase :: Context m => TM.TypeEnv -> DataArgsMap -> Ident -> DT.Case TM.Term -> m (EC.CompEnumCase, C.Comp)
clarifyCase tenv dataArgsMap cursor (DT.Cons _ disc dataArgs consArgs cont) = do
  let (_, dataTypes) = unzip dataArgs
  dataArgVars <- mapM (const $ Gensym.newIdentFromText "dataArg") dataTypes
  let dataArgsMap' = IntMap.insert (Ident.toInt cursor) (zip dataArgVars dataTypes) dataArgsMap
  let consArgs' = map (\(m, x, _) -> (m, x, m :< TM.Tau)) consArgs
  body' <- clarifyDecisionTree (TM.insTypeEnv consArgs' tenv) dataArgsMap' cont
  discriminantVar <- Gensym.newIdentFromText "discriminant"
  return
    ( () :< EC.Int (D.reify disc),
      C.SigmaElim
        False
        (discriminantVar : dataArgVars ++ map (\(_, x, _) -> x) consArgs)
        (C.VarLocal cursor)
        body'
    )

-- p :: Monad m => String -> m ()
-- p str =
--   trace str (return ())

alignFreeVariable :: Context m => TM.TypeEnv -> [BinderF TM.Term] -> C.Comp -> m C.Comp
alignFreeVariable tenv fvs e = do
  fvs' <- dropFst <$> clarifyBinder tenv fvs
  linearize fvs' e

clarifyMagic :: Context m => TM.TypeEnv -> M.Magic TM.Term -> m C.Comp
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
  Context m =>
  TM.TypeEnv ->
  LK.LamKindF TM.Term ->
  [(Hint, Ident, TM.Term)] ->
  TM.Term ->
  [BinderF TM.Term] ->
  m C.Comp
clarifyLambda tenv kind mxts e fvs = do
  e' <- clarifyTerm (TM.insTypeEnv (catMaybes [LK.fromLamKind kind] ++ mxts) tenv) e
  case kind of
    LK.Fix (_, x, _)
      | S.member x (freeVars e') ->
          returnClosure tenv O.Opaque kind fvs mxts e'
      | otherwise ->
          returnClosure tenv O.Transparent LK.Normal fvs mxts e'
    _ ->
      returnClosure tenv O.Transparent kind fvs mxts e'

newClosureNames :: Gensym.Context m => m ((Ident, C.Value), Ident, (Ident, C.Value), (Ident, C.Value))
newClosureNames = do
  closureVarInfo <- Gensym.newValueVarLocalWith "closure"
  typeVarName <- Gensym.newIdentFromText "exp"
  envVarInfo <- Gensym.newValueVarLocalWith "env"
  lamVarInfo <- Gensym.newValueVarLocalWith "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

clarifyPlus :: Context m => TM.TypeEnv -> TM.Term -> m (Ident, C.Comp, C.Value)
clarifyPlus tenv e = do
  e' <- clarifyTerm tenv e
  (varName, var) <- Gensym.newValueVarLocalWith "var"
  return (varName, e', var)

clarifyBinder :: Context m => TM.TypeEnv -> [BinderF TM.Term] -> m [(Hint, Ident, C.Comp)]
clarifyBinder tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm tenv t
      xts' <- clarifyBinder (IntMap.insert (Ident.toInt x) t tenv) xts
      return $ (m, x, t') : xts'

clarifyPrimOp :: Context m => TM.TypeEnv -> PrimOp -> Hint -> m C.Comp
clarifyPrimOp tenv op@(PrimOp _ domList _) m = do
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- mapAndUnzipM (const (Gensym.newValueVarLocalWith "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  returnClosure tenv O.Transparent LK.Normal [] mxts $ C.Primitive (C.PrimOp op varList)

returnClosure ::
  Context m =>
  TM.TypeEnv ->
  O.Opacity -> -- whether the closure is reducible
  LK.LamKindF TM.Term -> -- the name of newly created closure
  [BinderF TM.Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF TM.Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  C.Comp -> -- the `e` in `lam (x1, ..., xn). e`
  m C.Comp
returnClosure tenv opacity kind fvs xts e = do
  fvs'' <- dropFst <$> clarifyBinder tenv fvs
  xts'' <- dropFst <$> clarifyBinder tenv xts
  fvEnvSigma <- closureEnvS4 $ map Right fvs''
  let fvEnv = C.SigmaIntro (map (\(x, _) -> C.VarLocal x) fvs'')
  let arity = A.fromInt $ length xts'' + 1 -- arity == count(xts) + env
  case kind of
    LK.Normal -> do
      i <- Gensym.newCount
      name <- Locator.attachCurrentLocator $ BN.lambdaName i
      registerIfNecessary name opacity xts'' fvs'' e
      return $ C.UpIntro $ C.SigmaIntro [fvEnvSigma, fvEnv, C.VarGlobal name arity]
    LK.Fix (_, name, _) -> do
      name' <- Locator.attachCurrentLocator $ BN.lambdaName $ Ident.toInt name
      let cls = C.SigmaIntro [fvEnvSigma, fvEnv, C.VarGlobal name' arity]
      e' <- subst (IntMap.fromList [(Ident.toInt name, cls)]) IntMap.empty e
      registerIfNecessary name' O.Opaque xts'' fvs'' e'
      return $ C.UpIntro cls

registerIfNecessary ::
  Context m =>
  DD.DefiniteDescription ->
  O.Opacity ->
  [(Ident, C.Comp)] ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  m ()
registerIfNecessary name opacity xts1 xts2 e = do
  b <- Clarify.isAlreadyRegistered name
  unless b $ do
    e' <- linearize (xts2 ++ xts1) e
    (envVarName, envVar) <- Gensym.newValueVarLocalWith "env"
    let args = map fst xts1 ++ [envVarName]
    body <- Reduce.reduce $ C.SigmaElim True (map fst xts2) envVar e'
    Clarify.insertToAuxEnv name (opacity, args, body)

callClosure :: Gensym.Context m => C.Comp -> [(Ident, C.Comp, C.Value)] -> m C.Comp
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
