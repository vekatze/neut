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
import Entity.Comp
import Entity.Comp.FreeVars
import qualified Entity.Comp.Reduce as Reduce
import Entity.Comp.Subst
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.EnumCase
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Magic
import Entity.Opacity
import Entity.Pattern
import qualified Entity.Prim as Prim
import Entity.PrimNumSize
import Entity.PrimOp
import qualified Entity.Source as Source
import Entity.Stmt
import Entity.Term
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

clarify :: Context m => Source.Source -> [Stmt] -> m ([CompDef], Maybe Comp)
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
      mainTerm <- Reduce.reduce $ CompPiElimDownElim (ValueVarGlobal mainName (A.Arity 0)) []
      return (defList' ++ Map.toList auxEnv, Just mainTerm)
    Nothing -> do
      defList' <- clarifyDefList defList
      return (defList', Nothing)

clarifyDefList :: Context m => [Stmt] -> m [CompDef]
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

clarifyDef :: Context m => Stmt -> m (DD.DefiniteDescription, (Opacity, [Ident], Comp))
clarifyDef stmt =
  case stmt of
    StmtDefine opacity _ f _ xts _ e -> do
      e' <- clarifyTerm (insTypeEnv xts IntMap.empty) e
      xts' <- dropFst <$> clarifyBinder IntMap.empty xts
      e'' <- linearize xts' e' >>= Reduce.reduce
      return (f, (opacity, map fst xts', e''))

clarifyTerm :: Context m => TypeEnv -> Term -> m Comp
clarifyTerm tenv term =
  case term of
    _ :< TermTau ->
      return returnImmediateS4
    _ :< TermVar x -> do
      return $ CompUpIntro $ ValueVarLocal x
    _ :< TermVarGlobal x arity -> do
      return $
        CompUpIntro $
          ValueSigmaIntro
            [ immediateS4,
              ValueSigmaIntro [],
              ValueVarGlobal x arity
            ]
    _ :< TermPi {} ->
      return returnClosureS4
    _ :< TermPiIntro kind mxts e -> do
      clarifyLambda tenv kind mxts e $ nubFreeVariables $ chainOf tenv term
    _ :< TermPiElim e es -> do
      es' <- mapM (clarifyPlus tenv) es
      e' <- clarifyTerm tenv e
      callClosure e' es'
    _ :< TermSigma {} -> do
      return returnClosureS4
    m :< TermSigmaIntro es -> do
      k <- Gensym.newIdentFromText "sigma"
      clarifyTerm tenv $
        m
          :< TermPiIntro
            LamKindNormal
            [(m, k, m :< TermPi [] (m :< TermTau))]
            (m :< TermPiElim (m :< TermVar k) es)
    m :< TermSigmaElim xts e1 e2 -> do
      clarifyTerm tenv $ m :< TermPiElim e1 [m :< TermPiIntro LamKindNormal xts e2]
    m :< TermLet mxt e1 e2 -> do
      clarifyTerm tenv $ m :< TermPiElim (m :< TermPiIntro LamKindNormal [mxt] e2) [e1]
    m :< TermPrim prim ->
      case prim of
        Prim.Op op ->
          clarifyPrimOp tenv op m
        Prim.Type _ ->
          return returnImmediateS4
    _ :< TermInt size l ->
      return $ CompUpIntro (ValueInt size l)
    _ :< TermFloat size l ->
      return $ CompUpIntro (ValueFloat size l)
    _ :< TermEnum {} ->
      return returnImmediateS4
    _ :< TermEnumIntro label ->
      return $ CompUpIntro $ ValueEnumIntro label
    _ :< TermEnumElim (e, _) bs -> do
      let (enumCaseList, es) = unzip bs
      let fvs = chainFromTermList tenv es
      es' <- (mapM (clarifyTerm tenv) >=> alignFreeVariables tenv fvs) es
      (y, e', yVar) <- clarifyPlus tenv e
      return $ bindLet [(y, e')] $ CompEnumElim yVar (zip (map forgetHint enumCaseList) es')
    _ :< TermMagic der -> do
      clarifyMagic tenv der
    _ :< TermMatch (e, _) clauseList -> do
      ((dataVarName, dataVar), typeVarName, (envVarName, envVar), (tagVarName, tagVar)) <- newClosureNames
      let fvs = chainFromTermList tenv $ map caseClauseToLambda clauseList
      clauseList' <- forM (zip clauseList [0 ..]) $ \(((_, consName, arity, xts), body), i) -> do
        closure <- clarifyLambda tenv LamKindNormal xts body fvs
        (closureVarName, closureVar) <- Gensym.newValueVarLocalWith "clause"
        return
          ( () :< EnumCaseInt i,
            CompUpElim closureVarName closure $
              CompPiElimDownElim (ValueVarGlobal (DD.getConsDD consName) arity) [closureVar, envVar]
          )
      dataTerm <- clarifyTerm tenv e
      return $
        CompUpElim dataVarName dataTerm $
          CompSigmaElim True [typeVarName, envVarName, tagVarName] dataVar $
            CompEnumElim tagVar clauseList'

clarifyMagic :: Context m => TypeEnv -> Magic Term -> m Comp
clarifyMagic tenv der =
  case der of
    MagicCast from to value -> do
      (fromVarName, from', fromVar) <- clarifyPlus tenv from
      (toVarName, to', toVar) <- clarifyPlus tenv to
      (valueVarName, value', valueVar) <- clarifyPlus tenv value
      return $
        bindLet [(fromVarName, from'), (toVarName, to'), (valueVarName, value')] $
          CompPrimitive (PrimitiveMagic (MagicCast fromVar toVar valueVar))
    MagicStore lt pointer value -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus tenv pointer
      (valueVarName, value', valueVar) <- clarifyPlus tenv value
      return $
        bindLet [(pointerVarName, pointer'), (valueVarName, value')] $
          CompPrimitive (PrimitiveMagic (MagicStore lt pointerVar valueVar))
    MagicLoad lt pointer -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus tenv pointer
      return $
        bindLet [(pointerVarName, pointer')] $
          CompPrimitive (PrimitiveMagic (MagicLoad lt pointerVar))
    MagicSyscall syscallNum args -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus tenv) args
      return $
        bindLet (zip xs args') $
          CompPrimitive (PrimitiveMagic (MagicSyscall syscallNum xsAsVars))
    MagicExternal extFunName args -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus tenv) args
      return $
        bindLet (zip xs args') $
          CompPrimitive (PrimitiveMagic (MagicExternal extFunName xsAsVars))

clarifyLambda ::
  Context m =>
  TypeEnv ->
  LamKindF Term ->
  [(Hint, Ident, Term)] ->
  Term ->
  [BinderF Term] ->
  m Comp
clarifyLambda tenv kind mxts e fvs = do
  e' <- clarifyTerm (insTypeEnv (catMaybes [fromLamKind kind] ++ mxts) tenv) e
  case kind of
    LamKindFix (_, x, _)
      | S.member x (freeVars e') ->
          returnClosure tenv OpacityOpaque kind fvs mxts e'
      | otherwise ->
          returnClosure tenv OpacityTransparent LamKindNormal fvs mxts e'
    _ ->
      returnClosure tenv OpacityTransparent kind fvs mxts e'

newClosureNames :: Gensym.Context m => m ((Ident, Value), Ident, (Ident, Value), (Ident, Value))
newClosureNames = do
  closureVarInfo <- Gensym.newValueVarLocalWith "closure"
  typeVarName <- Gensym.newIdentFromText "exp"
  envVarInfo <- Gensym.newValueVarLocalWith "env"
  lamVarInfo <- Gensym.newValueVarLocalWith "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

caseClauseToLambda :: (PatternF Term, Term) -> Term
caseClauseToLambda pat =
  case pat of
    ((mPat, _, _, xts), body) ->
      mPat :< TermPiIntro LamKindNormal xts body

clarifyPlus :: Context m => TypeEnv -> Term -> m (Ident, Comp, Value)
clarifyPlus tenv e = do
  e' <- clarifyTerm tenv e
  (varName, var) <- Gensym.newValueVarLocalWith "var"
  return (varName, e', var)

clarifyBinder :: Context m => TypeEnv -> [BinderF Term] -> m [(Hint, Ident, Comp)]
clarifyBinder tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm tenv t
      xts' <- clarifyBinder (IntMap.insert (Ident.toInt x) t tenv) xts
      return $ (m, x, t') : xts'

chainFromTermList :: TypeEnv -> [Term] -> [BinderF Term]
chainFromTermList tenv es =
  nubFreeVariables $ concatMap (chainOf tenv) es

alignFreeVariables :: Context m => TypeEnv -> [BinderF Term] -> [Comp] -> m [Comp]
alignFreeVariables tenv fvs es = do
  es' <- mapM (returnClosure tenv OpacityTransparent LamKindNormal fvs []) es
  mapM (`callClosure` []) es'

nubFreeVariables :: [BinderF Term] -> [BinderF Term]
nubFreeVariables =
  nubBy (\(_, x, _) (_, y, _) -> x == y)

clarifyPrimOp :: Context m => TypeEnv -> PrimOp -> Hint -> m Comp
clarifyPrimOp tenv op@(PrimOp _ domList _) m = do
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- mapAndUnzipM (const (Gensym.newValueVarLocalWith "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  returnClosure tenv OpacityTransparent LamKindNormal [] mxts $ CompPrimitive (PrimitivePrimOp op varList)

returnClosure ::
  Context m =>
  TypeEnv ->
  Opacity -> -- whether the closure is reducible
  LamKindF Term -> -- the name of newly created closure
  [BinderF Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  Comp -> -- the `e` in `lam (x1, ..., xn). e`
  m Comp
returnClosure tenv opacity kind fvs xts e = do
  fvs' <- clarifyBinder tenv fvs
  xts' <- clarifyBinder tenv xts
  let xts'' = dropFst xts'
  let fvs'' = dropFst fvs'
  fvEnvSigma <- closureEnvS4 $ map Right fvs''
  let fvEnv = ValueSigmaIntro (map (\(_, x, _) -> ValueVarLocal x) fvs')
  let arity = A.fromInt $ length xts'' + 1 -- arity == count(xts) + env
  case kind of
    LamKindNormal -> do
      i <- Gensym.newCount
      name <- Locator.attachCurrentLocator $ BN.lambdaName i
      registerIfNecessary name opacity xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal name arity]
    LamKindCons _ consName discriminant _ -> do
      let consDD = DD.getConsDD consName
      registerIfNecessary consDD opacity xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueInt (IntSize 64) (D.reify discriminant)]
    LamKindFix (_, name, _) -> do
      name' <- Locator.attachCurrentLocator $ BN.lambdaName $ Ident.toInt name
      let cls = ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal name' arity]
      e' <- subst (IntMap.fromList [(Ident.toInt name, cls)]) IntMap.empty e
      registerIfNecessary name' OpacityOpaque xts'' fvs'' e'
      return $ CompUpIntro cls

registerIfNecessary ::
  Context m =>
  DD.DefiniteDescription ->
  Opacity ->
  [(Ident, Comp)] ->
  [(Ident, Comp)] ->
  Comp ->
  m ()
registerIfNecessary name opacity xts1 xts2 e = do
  b <- Clarify.isAlreadyRegistered name
  unless b $ do
    e' <- linearize (xts2 ++ xts1) e
    (envVarName, envVar) <- Gensym.newValueVarLocalWith "env"
    let args = map fst xts1 ++ [envVarName]
    body <- Reduce.reduce $ CompSigmaElim True (map fst xts2) envVar e'
    Clarify.insertToAuxEnv name (opacity, args, body)

callClosure :: Gensym.Context m => Comp -> [(Ident, Comp, Value)] -> m Comp
callClosure e zexes = do
  let (zs, es', xs) = unzip3 zexes
  ((closureVarName, closureVar), typeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames
  return $
    bindLet
      ((closureVarName, e) : zip zs es')
      ( CompSigmaElim
          True
          [typeVarName, envVarName, lamVarName]
          closureVar
          (CompPiElimDownElim lamVar (xs ++ [envVar]))
      )

chainOf :: TypeEnv -> Term -> [BinderF Term]
chainOf tenv term =
  case term of
    _ :< TermTau ->
      []
    m :< TermVar x -> do
      let t = (IntMap.!) tenv (Ident.toInt x)
      let xts = chainOf tenv t
      xts ++ [(m, x, t)]
    _ :< TermVarGlobal {} ->
      []
    _ :< TermPi {} ->
      []
    _ :< TermPiIntro kind xts e ->
      chainOf' tenv (catMaybes [fromLamKind kind] ++ xts) [e]
    _ :< TermPiElim e es -> do
      let xs1 = chainOf tenv e
      let xs2 = concatMap (chainOf tenv) es
      xs1 ++ xs2
    _ :< TermSigma xts ->
      chainOf' tenv xts []
    _ :< TermSigmaIntro es ->
      concatMap (chainOf tenv) es
    _ :< TermSigmaElim xts e1 e2 -> do
      let xs1 = chainOf tenv e1
      let xs2 = chainOf' tenv xts [e2]
      xs1 ++ xs2
    _ :< TermLet mxt e1 e2 -> do
      let xs1 = chainOf tenv e1
      let xs2 = chainOf' tenv [mxt] [e2]
      xs1 ++ xs2
    _ :< TermPrim _ ->
      []
    _ :< TermInt _ _ ->
      []
    _ :< TermFloat _ _ ->
      []
    _ :< TermEnum {} ->
      []
    _ :< TermEnumIntro {} ->
      []
    _ :< TermEnumElim (e, t) les -> do
      let xs0 = chainOf tenv t
      let xs1 = chainOf tenv e
      let es = map snd les
      let xs2 = concatMap (chainOf tenv) es
      xs0 ++ xs1 ++ xs2
    _ :< TermMagic der ->
      foldMap (chainOf tenv) der
    _ :< TermMatch (e, _) patList -> do
      let xs1 = chainOf tenv e
      let xs2 = concatMap (\((_, _, _, xts), body) -> chainOf' tenv xts [body]) patList
      xs1 ++ xs2

chainOf' :: TypeEnv -> [BinderF Term] -> [Term] -> [BinderF Term]
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

insTypeEnv :: [BinderF Term] -> TypeEnv -> TypeEnv
insTypeEnv xts tenv =
  case xts of
    [] ->
      tenv
    (_, x, t) : rest ->
      insTypeEnv rest $ IntMap.insert (Ident.toInt x) t tenv

forgetHint :: EnumCase -> CompEnumCase
forgetHint (_ :< enumCase) =
  case enumCase of
    EnumCaseLabel label ->
      () :< EnumCaseLabel label
    EnumCaseInt i ->
      () :< EnumCaseInt i
    EnumCaseDefault ->
      () :< EnumCaseDefault
