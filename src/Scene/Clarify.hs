module Scene.Clarify
  ( clarify,
  )
where

import Codec.Binary.UTF8.String
import qualified Context.App as App
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
import qualified Data.Text as T
import qualified Entity.Arity as A
import qualified Entity.BaseName as BN
import Entity.Binder
import Entity.Comp
import Entity.Comp.FreeVars
import Entity.Comp.Reduce
import Entity.Comp.Subst
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.EnumCase
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.LowType
import Entity.Magic
import Entity.Opacity
import Entity.Pattern
import qualified Entity.Prim as Prim
import Entity.PrimNum
import Entity.PrimNumSize
import Entity.PrimOp
import Entity.Source
import Entity.Stmt
import Entity.Term
import Entity.Term.FromPrimNum
import Scene.Clarify.Context
import Scene.Clarify.Linearize
import Scene.Clarify.Sigma
import Scene.Clarify.Utility

clarify :: App.Context -> Source -> [Stmt] -> IO ([CompDef], Maybe Comp)
clarify ctx source defList = do
  mMainDefiniteDescription <- Locator.getMainDefiniteDescription (App.locator ctx) source
  case mMainDefiniteDescription of
    Just mainName -> do
      auxEnv <- withSpecializedCtx ctx $ \clarifyCtx -> do
        registerImmediateS4 clarifyCtx
        registerClosureS4 clarifyCtx
        registerCellS4 clarifyCtx
        readAuxEnv clarifyCtx
      defList' <- clarifyDefList ctx defList
      mainTerm <- reduce ctx $ CompPiElimDownElim (ValueVarGlobal mainName (A.Arity 0)) []
      return (defList' ++ Map.toList auxEnv, Just mainTerm)
    Nothing -> do
      defList' <- clarifyDefList ctx defList
      return (defList', Nothing)

clarifyDefList :: App.Context -> [Stmt] -> IO [CompDef]
clarifyDefList ctx stmtList = do
  (stmtList', auxEnv) <- withSpecializedCtx ctx $ \clarifyCtx -> do
    stmtList' <- mapM (clarifyDef clarifyCtx) stmtList
    auxEnv <- readAuxEnv clarifyCtx
    return (stmtList', auxEnv)
  CompDefinition.union (App.compDefinition ctx) auxEnv
  stmtList'' <- forM stmtList' $ \(x, (opacity, args, e)) -> do
    e' <- reduce ctx e
    return (x, (opacity, args, e'))
  forM_ stmtList'' $ uncurry $ CompDefinition.insert (App.compDefinition ctx)
  return $ stmtList'' ++ Map.toList auxEnv

withSpecializedCtx :: App.Context -> (Context -> IO a) -> IO a
withSpecializedCtx ctx action = do
  clarifyCtx <- specialize ctx
  action clarifyCtx

clarifyDef :: Context -> Stmt -> IO (DD.DefiniteDescription, (Opacity, [Ident], Comp))
clarifyDef ctx stmt =
  case stmt of
    StmtDefine opacity _ f _ xts _ e -> do
      e' <- clarifyTerm ctx (insTypeEnv xts IntMap.empty) e
      xts' <- dropFst <$> clarifyBinder ctx IntMap.empty xts
      e'' <- linearize (App.gensym (base ctx)) xts' e' >>= reduce (base ctx)
      return (f, (opacity, map fst xts', e''))
    StmtDefineResource m name discarder copier -> do
      switchValue <- Gensym.newIdentFromText (App.gensym (base ctx)) "switchValue"
      value <- Gensym.newIdentFromText (App.gensym (base ctx)) "value"
      discarder' <- clarifyTerm ctx IntMap.empty (m :< TermPiElim discarder [m :< TermVar value]) >>= reduce (base ctx)
      copier' <- clarifyTerm ctx IntMap.empty (m :< TermPiElim copier [m :< TermVar value]) >>= reduce (base ctx)
      return
        ( name,
          ( OpacityTransparent,
            [switchValue, value],
            CompEnumElim (ValueVarLocal switchValue) $ switch discarder' copier'
          )
        )

clarifyTerm :: Context -> TypeEnv -> Term -> IO Comp
clarifyTerm ctx tenv term =
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
      clarifyLambda ctx tenv kind mxts e $ nubFreeVariables $ chainOf tenv term
    _ :< TermPiElim e es -> do
      es' <- mapM (clarifyPlus ctx tenv) es
      e' <- clarifyTerm ctx tenv e
      callClosure (App.gensym (base ctx)) e' es'
    _ :< TermSigma {} -> do
      return returnClosureS4
    m :< TermSigmaIntro es -> do
      k <- Gensym.newIdentFromText (App.gensym (base ctx)) "sigma"
      clarifyTerm ctx tenv $
        m
          :< TermPiIntro
            LamKindNormal
            [(m, k, m :< TermPi [] (m :< TermTau))]
            (m :< TermPiElim (m :< TermVar k) es)
    m :< TermSigmaElim xts e1 e2 -> do
      clarifyTerm ctx tenv $ m :< TermPiElim e1 [m :< TermPiIntro LamKindNormal xts e2]
    m :< TermLet mxt e1 e2 -> do
      clarifyTerm ctx tenv $ m :< TermPiElim (m :< TermPiIntro LamKindNormal [mxt] e2) [e1]
    m :< TermPrim prim ->
      case prim of
        Prim.Op op ->
          clarifyPrimOp ctx tenv op m
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
      es' <- (mapM (clarifyTerm ctx tenv) >=> alignFreeVariables ctx tenv fvs) es
      (y, e', yVar) <- clarifyPlus ctx tenv e
      return $ bindLet [(y, e')] $ CompEnumElim yVar (zip (map forgetHint enumCaseList) es')
    _ :< TermMagic der -> do
      clarifyMagic ctx tenv der
    _ :< TermMatch mSubject (e, _) clauseList -> do
      ((dataVarName, dataVar), typeVarName, (envVarName, envVar), (tagVarName, tagVar)) <- newClosureNames (App.gensym (base ctx))
      let fvs = chainFromTermList tenv $ map caseClauseToLambda clauseList
      clauseList' <- forM (zip clauseList [0 ..]) $ \(((_, consName, arity, xts), body), i) -> do
        closure <- clarifyLambda ctx tenv LamKindNormal xts body fvs
        (closureVarName, closureVar) <- Gensym.newValueVarLocalWith (App.gensym (base ctx)) "clause"
        return
          ( () :< EnumCaseInt i,
            CompUpElim
              closureVarName
              closure
              $ CompPiElimDownElim
                (ValueVarGlobal (getClauseConsName consName (isJust mSubject)) arity)
                [closureVar, envVar]
          )
      dataTerm <- clarifyTerm ctx tenv e
      return $
        CompUpElim
          dataVarName
          dataTerm
          $ CompSigmaElim
            (isJust mSubject)
            [typeVarName, envVarName, tagVarName]
            dataVar
            $ CompEnumElim tagVar clauseList'
    _ :< TermNoema {} -> do
      return returnImmediateS4
    m :< TermNoemaIntro _ e ->
      case e of
        _ :< TermVar x ->
          return $ CompUpIntro (ValueVarLocalIdeal x)
        _ ->
          Throw.raiseCritical (App.throw (base ctx)) m "compiler bug: found a non-variable noetic value"
    m :< TermNoemaElim s e -> do
      e' <- clarifyTerm ctx (IntMap.insert (Ident.toInt s) (m :< TermTau) tenv) e
      return $ CompUpElim s (CompUpIntro (ValueSigmaIntro [])) e'
    _ :< TermArray elemType -> do
      return $ CompUpIntro $ ValueVarGlobal (DD.array elemType) A.arityS4
    _ :< TermArrayIntro elemType elems -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus ctx tenv) elems
      return $
        bindLet (zip xs args') $
          CompUpIntro (ValueArrayIntro elemType xsAsVars)
    _ :< TermArrayAccess _ elemType array index -> do
      (arrayVarName, array', arrayVar) <- clarifyPlus ctx tenv array
      (indexVarName, index', indexVar) <- clarifyPlus ctx tenv index
      return $
        bindLet [(arrayVarName, array'), (indexVarName, index')] $
          CompArrayAccess elemType arrayVar indexVar
    m :< TermText ->
      clarifyTerm ctx tenv $ m :< TermArray (PrimNumInt $ IntSize 8)
    m :< TermTextIntro text -> do
      let i8s = encode $ T.unpack text
      let i8s' = map (\x -> m :< TermInt (IntSize 8) (toInteger x)) i8s
      clarifyTerm ctx tenv $ m :< TermArrayIntro (PrimNumInt (IntSize 8)) i8s'
    _ :< TermCell {} -> do
      return returnCellS4
    _ :< TermCellIntro contentType content -> do
      (contentTypeVarName, contentType', contentTypeVar) <- clarifyPlus ctx tenv contentType
      (contentVarName, content', contentVar) <- clarifyPlus ctx tenv content
      return $
        bindLet [(contentTypeVarName, contentType'), (contentVarName, content')] $
          CompUpIntro (ValueSigmaIntro [contentTypeVar, contentVar])
    _ :< TermCellRead cell -> do
      (cellVarName, cell', cellVar) <- clarifyPlus ctx tenv cell
      (typeVarName, typeVar) <- Gensym.newValueVarLocalWith (App.gensym (base ctx)) "typeVar"
      valueVarName <- Gensym.newIdentFromText (App.gensym (base ctx)) "valueVar"
      returnClonedValue <- toRelevantApp (App.gensym (base ctx)) valueVarName (CompUpIntro typeVar)
      return $
        bindLet [(cellVarName, cell')] $
          CompSigmaElim True [typeVarName, valueVarName] cellVar returnClonedValue
    _ :< TermCellWrite cell newValue -> do
      (typeVarName, typeVar) <- Gensym.newValueVarLocalWith (App.gensym (base ctx)) "typeVar"
      (cellVarName, cell', cellVar) <- clarifyPlus ctx tenv cell
      oldValueVarName <- Gensym.newIdentFromText (App.gensym (base ctx)) "oldValueVar"
      (newValueVarName, newValue', newValueVar) <- clarifyPlus ctx tenv newValue
      discardOldContent <- toAffineApp (App.gensym (base ctx)) oldValueVarName (CompUpIntro typeVar)
      placeHolder <- Gensym.newIdentFromText (App.gensym (base ctx)) "placeholder"
      (addrVarName, addrVar) <- Gensym.newValueVarLocalWith (App.gensym (base ctx)) "address"
      return $
        bindLet [(cellVarName, cell'), (newValueVarName, newValue')] $
          CompSigmaElim True [typeVarName, oldValueVarName] cellVar $
            CompUpElim placeHolder discardOldContent $
              CompUpElim addrVarName (add cellVar (ValueInt (IntSize 64) 8)) $
                CompPrimitive $
                  PrimitiveMagic (MagicStore voidPtr addrVar newValueVar)
    _ :< TermResourceType name -> do
      return $ CompUpIntro $ ValueVarGlobal name A.arityS4

add :: Value -> Value -> Comp
add v1 v2 = do
  let i64 = PrimNumInt (IntSize 64)
  CompPrimitive $ PrimitivePrimOp (PrimOp "add" [i64, i64] i64) [v1, v2]

clarifyMagic :: Context -> TypeEnv -> Magic Term -> IO Comp
clarifyMagic ctx tenv der =
  case der of
    MagicCast from to value -> do
      (fromVarName, from', fromVar) <- clarifyPlus ctx tenv from
      (toVarName, to', toVar) <- clarifyPlus ctx tenv to
      (valueVarName, value', valueVar) <- clarifyPlus ctx tenv value
      return $
        bindLet [(fromVarName, from'), (toVarName, to'), (valueVarName, value')] $
          CompPrimitive (PrimitiveMagic (MagicCast fromVar toVar valueVar))
    MagicStore lt pointer value -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus ctx tenv pointer
      (valueVarName, value', valueVar) <- clarifyPlus ctx tenv value
      return $
        bindLet [(pointerVarName, pointer'), (valueVarName, value')] $
          CompPrimitive (PrimitiveMagic (MagicStore lt pointerVar valueVar))
    MagicLoad lt pointer -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus ctx tenv pointer
      return $
        bindLet [(pointerVarName, pointer')] $
          CompPrimitive (PrimitiveMagic (MagicLoad lt pointerVar))
    MagicSyscall syscallNum args -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus ctx tenv) args
      return $
        bindLet (zip xs args') $
          CompPrimitive (PrimitiveMagic (MagicSyscall syscallNum xsAsVars))
    MagicExternal extFunName args -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus ctx tenv) args
      return $
        bindLet (zip xs args') $
          CompPrimitive (PrimitiveMagic (MagicExternal extFunName xsAsVars))

clarifyLambda ::
  Context ->
  TypeEnv ->
  LamKindF Term ->
  [(Hint, Ident, Term)] ->
  Term ->
  [BinderF Term] ->
  IO Comp
clarifyLambda ctx tenv kind mxts e fvs = do
  e' <- clarifyTerm ctx (insTypeEnv (catMaybes [fromLamKind kind] ++ mxts) tenv) e
  case kind of
    LamKindFix (_, x, _)
      | S.member x (freeVars e') ->
        returnClosure ctx tenv OpacityOpaque kind fvs mxts e'
      | otherwise ->
        returnClosure ctx tenv OpacityTransparent LamKindNormal fvs mxts e'
    _ ->
      returnClosure ctx tenv OpacityTransparent kind fvs mxts e'

newClosureNames :: Gensym.Context -> IO ((Ident, Value), Ident, (Ident, Value), (Ident, Value))
newClosureNames ctx = do
  closureVarInfo <- Gensym.newValueVarLocalWith ctx "closure"
  typeVarName <- Gensym.newIdentFromText ctx "exp"
  envVarInfo <- Gensym.newValueVarLocalWith ctx "env"
  lamVarInfo <- Gensym.newValueVarLocalWith ctx "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

caseClauseToLambda :: (PatternF Term, Term) -> Term
caseClauseToLambda pat =
  case pat of
    ((mPat, _, _, xts), body) ->
      mPat :< TermPiIntro LamKindNormal xts body

clarifyPlus :: Context -> TypeEnv -> Term -> IO (Ident, Comp, Value)
clarifyPlus ctx tenv e = do
  e' <- clarifyTerm ctx tenv e
  (varName, var) <- Gensym.newValueVarLocalWith (App.gensym (base ctx)) "var"
  return (varName, e', var)

clarifyBinder :: Context -> TypeEnv -> [BinderF Term] -> IO [(Hint, Ident, Comp)]
clarifyBinder ctx tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm ctx tenv t
      xts' <- clarifyBinder ctx (IntMap.insert (Ident.toInt x) t tenv) xts
      return $ (m, x, t') : xts'

chainFromTermList :: TypeEnv -> [Term] -> [BinderF Term]
chainFromTermList tenv es =
  nubFreeVariables $ concatMap (chainOf tenv) es

alignFreeVariables :: Context -> TypeEnv -> [BinderF Term] -> [Comp] -> IO [Comp]
alignFreeVariables ctx tenv fvs es = do
  es' <- mapM (returnClosure ctx tenv OpacityTransparent LamKindNormal fvs []) es
  mapM (\e -> callClosure (App.gensym (base ctx)) e []) es'

nubFreeVariables :: [BinderF Term] -> [BinderF Term]
nubFreeVariables =
  nubBy (\(_, x, _) (_, y, _) -> x == y)

clarifyPrimOp :: Context -> TypeEnv -> PrimOp -> Hint -> IO Comp
clarifyPrimOp ctx tenv op@(PrimOp _ domList _) m = do
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- unzip <$> mapM (const (Gensym.newValueVarLocalWith (App.gensym (base ctx)) "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  returnClosure ctx tenv OpacityTransparent LamKindNormal [] mxts $ CompPrimitive (PrimitivePrimOp op varList)

returnClosure ::
  Context ->
  TypeEnv ->
  Opacity -> -- whether the closure is reducible
  LamKindF Term -> -- the name of newly created closure
  [BinderF Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  Comp -> -- the `e` in `lam (x1, ..., xn). e`
  IO Comp
returnClosure ctx tenv opacity kind fvs xts e = do
  fvs' <- clarifyBinder ctx tenv fvs
  xts' <- clarifyBinder ctx tenv xts
  let xts'' = dropFst xts'
  let fvs'' = dropFst fvs'
  fvEnvSigma <- closureEnvS4 ctx $ map Right fvs''
  let fvEnv = ValueSigmaIntro (map (\(_, x, _) -> ValueVarLocal x) fvs')
  let arity = A.fromInt $ length xts'' + 1 -- arity == count(xts) + env
  case kind of
    LamKindNormal -> do
      i <- Gensym.newCount (App.gensym (base ctx))
      name <- Locator.attachCurrentLocator (App.locator (base ctx)) $ BN.lambdaName i
      registerIfNecessary ctx name opacity False xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal name arity]
    LamKindCons _ consName discriminant _ -> do
      let consDD = DD.getConsDD consName
      registerIfNecessary ctx consDD opacity True xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueInt (IntSize 64) (D.reify discriminant)]
    LamKindFix (_, name, _) -> do
      name' <- Locator.attachCurrentLocator (App.locator (base ctx)) $ BN.lambdaName $ Ident.toInt name
      let cls = ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal name' arity]
      e' <- subst (App.gensym (base ctx)) (IntMap.fromList [(Ident.toInt name, cls)]) IntMap.empty e
      registerIfNecessary ctx name' OpacityOpaque False xts'' fvs'' e'
      return $ CompUpIntro cls

registerIfNecessary ::
  Context ->
  DD.DefiniteDescription ->
  Opacity ->
  Bool ->
  [(Ident, Comp)] ->
  [(Ident, Comp)] ->
  Comp ->
  IO ()
registerIfNecessary ctx name opacity isNoetic xts1 xts2 e = do
  b <- isAlreadyRegistered ctx name
  unless b $ do
    e' <- linearize (App.gensym (base ctx)) (xts2 ++ xts1) e
    (envVarName, envVar) <- Gensym.newValueVarLocalWith (App.gensym (base ctx)) "env"
    let args = map fst xts1 ++ [envVarName]
    body <- reduce (base ctx) $ CompSigmaElim False (map fst xts2) envVar e'
    insertToAuxEnv ctx name (opacity, args, body)
    when isNoetic $ do
      bodyNoetic <- reduce (base ctx) $ CompSigmaElim True (map fst xts2) envVar e'
      insertToAuxEnv ctx (DD.getNoeticDD name) (opacity, args, bodyNoetic)

callClosure :: Gensym.Context -> Comp -> [(Ident, Comp, Value)] -> IO Comp
callClosure ctx e zexes = do
  let (zs, es', xs) = unzip3 zexes
  ((closureVarName, closureVar), typeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames ctx
  return $
    bindLet
      ((closureVarName, e) : zip zs es')
      ( CompSigmaElim
          False
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
    _ :< TermMatch mSubject (e, _) patList -> do
      let xs1 = concatMap (chainOf tenv) (maybeToList mSubject)
      let xs2 = chainOf tenv e
      let xs3 = concatMap (\((_, _, _, xts), body) -> chainOf' tenv xts [body]) patList
      xs1 ++ xs2 ++ xs3
    _ :< TermNoema s t ->
      chainOf tenv s ++ chainOf tenv t
    m :< TermNoemaIntro s e ->
      (m, s, m :< TermTau) : chainOf tenv e
    m :< TermNoemaElim s e ->
      filter (\(_, y, _) -> y /= s) $ chainOf (IntMap.insert (Ident.toInt s) (m :< TermTau) tenv) e
    _ :< TermArray _ ->
      []
    _ :< TermArrayIntro _ elems -> do
      concatMap (chainOf tenv) elems
    _ :< TermArrayAccess subject _ array index -> do
      concatMap (chainOf tenv) [subject, array, index]
    _ :< TermText ->
      []
    _ :< TermTextIntro _ ->
      []
    _ :< TermCell contentType ->
      chainOf tenv contentType
    _ :< TermCellIntro contentType content -> do
      concatMap (chainOf tenv) [contentType, content]
    _ :< TermCellRead cell -> do
      chainOf tenv cell
    _ :< TermCellWrite cell newValue -> do
      concatMap (chainOf tenv) [cell, newValue]
    _ :< TermResourceType {} ->
      []

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

getClauseConsName :: DD.DefiniteDescription -> Bool -> DD.DefiniteDescription
getClauseConsName basename isNoetic = do
  let consName' = DD.getConsDD basename
  if isNoetic
    then DD.getNoeticDD consName'
    else consName'
