module Scene.Clarify
  ( clarify,
    Context,
  )
where

import qualified Context.CompDefinition as CompDefinition
import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Entity.Arity as A
import qualified Entity.BaseName as BN
import Entity.Binder
import qualified Entity.Comp as C
import Entity.Comp.FreeVars
import qualified Entity.Comp.Reduce as Reduce
import Entity.Comp.Subst
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import qualified Entity.EnumCase as EC
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.Magic as M
import qualified Entity.Opacity as O
import Entity.Pattern
import qualified Entity.Prim as Prim
import Entity.PrimNumSize
import Entity.PrimOp
import qualified Entity.Source as Source
import Entity.Stmt
import qualified Entity.Term as TM
import Entity.Term.FromPrimNum
import qualified Scene.Clarify.Context as Clarify
import Scene.Clarify.Linearize
import Scene.Clarify.Sigma
import Scene.Clarify.Utility

class
  ( Clarify.Context m,
    Reduce.Context m,
    Throw.Context m
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
        registerCellS4
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
    auxEnv <- Clarify.getAuxEnv
    return (stmtList', auxEnv)
  CompDefinition.union auxEnv
  stmtList'' <- forM stmtList' $ \(x, (opacity, args, e)) -> do
    e' <- Reduce.reduce e
    return (x, (opacity, args, e'))
  forM_ stmtList'' $ uncurry CompDefinition.insert
  return $ stmtList'' ++ Map.toList auxEnv

withSpecializedCtx :: Context m => m a -> m a
withSpecializedCtx action = do
  Clarify.initialize
  action

clarifyDef :: Context m => Stmt -> m (DD.DefiniteDescription, (O.Opacity, [Ident], C.Comp))
clarifyDef stmt =
  case stmt of
    StmtDefine opacity _ f _ xts _ e -> do
      e' <- clarifyTerm (insTypeEnv xts IntMap.empty) e
      xts' <- dropFst <$> clarifyBinder IntMap.empty xts
      e'' <- linearize xts' e' >>= Reduce.reduce
      return (f, (opacity, map fst xts', e''))

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
      clarifyLambda tenv kind mxts e $ nubFreeVariables $ chainOf tenv term
    _ :< TM.PiElim e es -> do
      es' <- mapM (clarifyPlus tenv) es
      e' <- clarifyTerm tenv e
      callClosure e' es'
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
        Prim.Op op ->
          clarifyPrimOp tenv op m
        Prim.Type _ ->
          return returnImmediateS4
    _ :< TM.Int size l ->
      return $ C.UpIntro (C.Int size l)
    _ :< TM.Float size l ->
      return $ C.UpIntro (C.Float size l)
    _ :< TM.Enum {} ->
      return returnImmediateS4
    _ :< TM.EnumIntro label ->
      return $ C.UpIntro $ C.EnumIntro label
    _ :< TM.EnumElim (e, _) bs -> do
      let (enumCaseList, es) = unzip bs
      let fvs = chainFromTermList tenv es
      es' <- (mapM (clarifyTerm tenv) >=> alignFreeVariables tenv fvs) es
      (y, e', yVar) <- clarifyPlus tenv e
      return $ bindLet [(y, e')] $ C.EnumElim yVar (zip (map forgetHint enumCaseList) es')
    _ :< TM.Magic der -> do
      clarifyMagic tenv der
    _ :< TM.Match (e, _) clauseList -> do
      ((dataVarName, dataVar), typeVarName, (envVarName, envVar), (tagVarName, tagVar)) <- newClosureNames
      let fvs = chainFromTermList tenv $ map caseClauseToLambda clauseList
      clauseList' <- forM (zip clauseList [0 ..]) $ \(((_, consName, arity, xts), body), i) -> do
        closure <- clarifyLambda tenv LK.Normal xts body fvs
        (closureVarName, closureVar) <- Gensym.newValueVarLocalWith "clause"
        return
          ( () :< EC.Int i,
            C.UpElim closureVarName closure $
              C.PiElimDownElim (C.VarGlobal (DD.getConsDD consName) arity) [closureVar, envVar]
          )
      dataTerm <- clarifyTerm tenv e
      return $
        C.UpElim dataVarName dataTerm $
          C.SigmaElim True [typeVarName, envVarName, tagVarName] dataVar $
            C.EnumElim tagVar clauseList'

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
  e' <- clarifyTerm (insTypeEnv (catMaybes [LK.fromLamKind kind] ++ mxts) tenv) e
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

caseClauseToLambda :: (PatternF TM.Term, TM.Term) -> TM.Term
caseClauseToLambda pat =
  case pat of
    ((mPat, _, _, xts), body) ->
      mPat :< TM.PiIntro LK.Normal xts body

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

chainFromTermList :: TM.TypeEnv -> [TM.Term] -> [BinderF TM.Term]
chainFromTermList tenv es =
  nubFreeVariables $ concatMap (chainOf tenv) es

alignFreeVariables :: Context m => TM.TypeEnv -> [BinderF TM.Term] -> [C.Comp] -> m [C.Comp]
alignFreeVariables tenv fvs es = do
  es' <- mapM (returnClosure tenv O.Transparent LK.Normal fvs []) es
  mapM (`callClosure` []) es'

nubFreeVariables :: [BinderF TM.Term] -> [BinderF TM.Term]
nubFreeVariables =
  nubBy (\(_, x, _) (_, y, _) -> x == y)

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
  fvs' <- clarifyBinder tenv fvs
  xts' <- clarifyBinder tenv xts
  let xts'' = dropFst xts'
  let fvs'' = dropFst fvs'
  fvEnvSigma <- closureEnvS4 $ map Right fvs''
  let fvEnv = C.SigmaIntro (map (\(_, x, _) -> C.VarLocal x) fvs')
  let arity = A.fromInt $ length xts'' + 1 -- arity == count(xts) + env
  case kind of
    LK.Normal -> do
      i <- Gensym.newCount
      name <- Locator.attachCurrentLocator $ BN.lambdaName i
      registerIfNecessary name opacity xts'' fvs'' e
      return $ C.UpIntro $ C.SigmaIntro [fvEnvSigma, fvEnv, C.VarGlobal name arity]
    LK.Cons _ consName discriminant _ -> do
      let consDD = DD.getConsDD consName
      registerIfNecessary consDD opacity xts'' fvs'' e
      return $ C.UpIntro $ C.SigmaIntro [fvEnvSigma, fvEnv, C.Int (IntSize 64) (D.reify discriminant)]
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

chainOf :: TM.TypeEnv -> TM.Term -> [BinderF TM.Term]
chainOf tenv term =
  case term of
    _ :< TM.Tau ->
      []
    m :< TM.Var x -> do
      let t = (IntMap.!) tenv (Ident.toInt x)
      let xts = chainOf tenv t
      xts ++ [(m, x, t)]
    _ :< TM.VarGlobal {} ->
      []
    _ :< TM.Pi {} ->
      []
    _ :< TM.PiIntro kind xts e ->
      chainOf' tenv (catMaybes [LK.fromLamKind kind] ++ xts) [e]
    _ :< TM.PiElim e es -> do
      let xs1 = chainOf tenv e
      let xs2 = concatMap (chainOf tenv) es
      xs1 ++ xs2
    _ :< TM.Sigma xts ->
      chainOf' tenv xts []
    _ :< TM.SigmaIntro es ->
      concatMap (chainOf tenv) es
    _ :< TM.SigmaElim xts e1 e2 -> do
      let xs1 = chainOf tenv e1
      let xs2 = chainOf' tenv xts [e2]
      xs1 ++ xs2
    _ :< TM.Let mxt e1 e2 -> do
      let xs1 = chainOf tenv e1
      let xs2 = chainOf' tenv [mxt] [e2]
      xs1 ++ xs2
    _ :< TM.Prim _ ->
      []
    _ :< TM.Int _ _ ->
      []
    _ :< TM.Float _ _ ->
      []
    _ :< TM.Enum {} ->
      []
    _ :< TM.EnumIntro {} ->
      []
    _ :< TM.EnumElim (e, t) les -> do
      let xs0 = chainOf tenv t
      let xs1 = chainOf tenv e
      let es = map snd les
      let xs2 = concatMap (chainOf tenv) es
      xs0 ++ xs1 ++ xs2
    _ :< TM.Magic der ->
      foldMap (chainOf tenv) der
    _ :< TM.Match (e, _) patList -> do
      let xs1 = chainOf tenv e
      let xs2 = concatMap (\((_, _, _, xts), body) -> chainOf' tenv xts [body]) patList
      xs1 ++ xs2

chainOf' :: TM.TypeEnv -> [BinderF TM.Term] -> [TM.Term] -> [BinderF TM.Term]
chainOf' tenv binder es =
  case binder of
    [] ->
      concatMap (chainOf tenv) es
    (_, x, t) : xts -> do
      let xs1 = chainOf tenv t
      let xs2 = chainOf' (IntMap.insert (Ident.toInt x) t tenv) xts es
      xs1 ++ filter (\(_, y, _) -> y /= x) xs2

dropFst :: [(a, b, c)] -> [(b, c)]
dropFst xyzs = do
  let (_, ys, zs) = unzip3 xyzs
  zip ys zs

insTypeEnv :: [BinderF TM.Term] -> TM.TypeEnv -> TM.TypeEnv
insTypeEnv xts tenv =
  case xts of
    [] ->
      tenv
    (_, x, t) : rest ->
      insTypeEnv rest $ IntMap.insert (Ident.toInt x) t tenv

forgetHint :: EC.EnumCase -> EC.CompEnumCase
forgetHint (_ :< enumCase) =
  case enumCase of
    EC.Label label ->
      () :< EC.Label label
    EC.Int i ->
      () :< EC.Int i
    EC.Default ->
      () :< EC.Default
