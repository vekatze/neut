module Scene.Clarify
  ( clarifyMain,
    clarifyOther,
  )
where

import Codec.Binary.UTF8.String
import Context.App
import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.Function
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.Comp
import Entity.Comp.FreeVars
import Entity.Comp.Reduce
import Entity.Comp.Subst
import Entity.EnumCase
import Entity.Global
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.LowType
import Entity.Magic
import Entity.Opacity
import Entity.Pattern
import Entity.PrimNum
import qualified Entity.PrimNum.FromText as PrimNum
import Entity.PrimNum.ToText
import Entity.PrimNumSize
import Entity.PrimOp
import qualified Entity.PrimOp.FromText as PrimOp
import Entity.Stmt
import Entity.Term
import Entity.Term.FromPrimNum
import Scene.Clarify.Linearize
import Scene.Clarify.Sigma
import Scene.Clarify.Utility

clarifyMain :: Axis -> T.Text -> [Stmt] -> IO ([CompDef], Comp)
clarifyMain axis mainName defList = do
  _ <- returnImmediateS4 (axis & gensym)
  _ <- returnClosureS4 axis
  defList' <- clarifyDefList axis defList
  mainTerm <- reduce (axis & gensym) $ CompPiElimDownElim (ValueVarGlobal (wrapWithQuote mainName)) []
  return (defList', mainTerm)

clarifyOther :: Axis -> [Stmt] -> IO [CompDef]
clarifyOther axis defList = do
  clarifyDefList axis defList

clarifyDefList :: Axis -> [Stmt] -> IO [CompDef]
clarifyDefList axis defList = do
  compDefEnv <- readIORef compDefEnvRef
  writeIORef compDefEnvRef Map.empty
  defList' <- mapM (clarifyDef axis) defList
  newDefEnv <- Map.toList <$> readIORef compDefEnvRef
  modifyIORef' compDefEnvRef $ Map.union compDefEnv
  mapM_ register defList'
  defList'' <- forM defList' $ \(x, (opacity, args, e)) -> do
    e' <- reduce (axis & gensym) e
    return (wrapWithQuote x, (opacity, args, e'))
  return $ defList'' ++ newDefEnv

register :: (T.Text, (Opacity, [Ident], Comp)) -> IO ()
register (x, (opacity, args, e)) =
  insDefEnv (wrapWithQuote x) opacity args e

clarifyDef :: Axis -> Stmt -> IO (T.Text, (Opacity, [Ident], Comp))
clarifyDef axis stmt =
  case stmt of
    StmtDefine opacity _ f _ xts _ e -> do
      e' <- clarifyTerm axis (insTypeEnv xts IntMap.empty) e
      xts' <- dropFst <$> clarifyBinder axis IntMap.empty xts
      e'' <- linearize (axis & gensym) xts' e' >>= reduce (axis & gensym)
      return (f, (opacity, map fst xts', e''))
    StmtDefineResource m name discarder copier -> do
      switchValue <- Gensym.newIdentFromText (axis & gensym) "switchValue"
      value <- Gensym.newIdentFromText (axis & gensym) "value"
      discarder' <- clarifyTerm axis IntMap.empty (m :< TermPiElim discarder [m :< TermVar value]) >>= reduce (axis & gensym)
      copier' <- clarifyTerm axis IntMap.empty (m :< TermPiElim copier [m :< TermVar value]) >>= reduce (axis & gensym)
      return
        ( name,
          ( OpacityTransparent,
            [switchValue, value],
            CompEnumElim (ValueVarLocal switchValue) $ switch discarder' copier'
          )
        )

clarifyTerm :: Axis -> TypeEnv -> Term -> IO Comp
clarifyTerm axis tenv term =
  case term of
    _ :< TermTau ->
      returnImmediateS4 (axis & gensym)
    _ :< TermVar x -> do
      return $ CompUpIntro $ ValueVarLocal x
    _ :< TermVarGlobal x -> do
      resourceTypeSet <- readIORef resourceTypeSetRef
      if S.member x resourceTypeSet
        then return $ CompUpIntro $ ValueVarGlobal $ wrapWithQuote x
        else do
          imm <- immediateS4 (axis & gensym)
          return $
            CompUpIntro $
              ValueSigmaIntro
                [ imm,
                  ValueSigmaIntro [],
                  ValueVarGlobal $ wrapWithQuote x
                ]
    _ :< TermPi {} ->
      returnClosureS4 axis
    _ :< TermPiIntro kind mxts e -> do
      clarifyLambda axis tenv kind mxts e $ nubFreeVariables $ chainOf tenv term
    _ :< TermPiElim e es -> do
      es' <- mapM (clarifyPlus axis tenv) es
      e' <- clarifyTerm axis tenv e
      callClosure (axis & gensym) e' es'
    _ :< TermSigma {} -> do
      returnClosureS4 axis
    m :< TermSigmaIntro es -> do
      k <- Gensym.newIdentFromText (axis & gensym) "sigma"
      clarifyTerm axis tenv $
        m
          :< TermPiIntro
            LamKindNormal
            [(m, k, m :< TermPi [] (m :< TermTau))]
            (m :< TermPiElim (m :< TermVar k) es)
    m :< TermSigmaElim xts e1 e2 -> do
      clarifyTerm axis tenv $ m :< TermPiElim e1 [m :< TermPiIntro LamKindNormal xts e2]
    m :< TermLet mxt e1 e2 -> do
      clarifyTerm axis tenv $ m :< TermPiElim (m :< TermPiIntro LamKindNormal [mxt] e2) [e1]
    m :< TermConst x ->
      clarifyConst axis tenv m x
    _ :< TermInt size l ->
      return $ CompUpIntro (ValueInt size l)
    _ :< TermFloat size l ->
      return $ CompUpIntro (ValueFloat size l)
    _ :< TermEnum {} ->
      returnImmediateS4 (axis & gensym)
    _ :< TermEnumIntro internal l ->
      return $ CompUpIntro $ ValueEnumIntro internal l
    _ :< TermEnumElim (e, _) bs -> do
      let (enumCaseList, es) = unzip bs
      let fvs = chainFromTermList tenv es
      es' <- (mapM (clarifyTerm axis tenv) >=> alignFreeVariables axis tenv fvs) es
      (y, e', yVar) <- clarifyPlus axis tenv e
      return $ bindLet [(y, e')] $ CompEnumElim yVar (zip (map forgetHint enumCaseList) es')
    _ :< TermMagic der -> do
      clarifyMagic axis tenv der
    _ :< TermMatch mSubject (e, _) clauseList -> do
      ((dataVarName, dataVar), typeVarName, (envVarName, envVar), (tagVarName, tagVar)) <- newClosureNames (axis & gensym)
      let fvs = chainFromTermList tenv $ map caseClauseToLambda clauseList
      clauseList' <- forM (zip clauseList [0 ..]) $ \(((_, consName, xts), body), i) -> do
        closure <- clarifyLambda axis tenv LamKindNormal xts body fvs
        (closureVarName, closureVar) <- Gensym.newValueVarLocalWith (axis & gensym) "clause"
        return
          ( () :< EnumCaseInt i,
            CompUpElim
              closureVarName
              closure
              $ CompPiElimDownElim
                (ValueVarGlobal (getClauseConsName consName (isJust mSubject)))
                [closureVar, envVar]
          )
      dataTerm <- clarifyTerm axis tenv e
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
      returnImmediateS4 (axis & gensym)
    m :< TermNoemaIntro _ e ->
      case e of
        _ :< TermVar x ->
          return $ CompUpIntro (ValueVarLocalIdeal x)
        _ ->
          (axis & throw & Throw.raiseCritical) m "compiler bug: found a non-variable noetic value"
    m :< TermNoemaElim s e -> do
      e' <- clarifyTerm axis (IntMap.insert (Ident.toInt s) (m :< TermTau) tenv) e
      return $ CompUpElim s (CompUpIntro (ValueSigmaIntro [])) e'
    _ :< TermArray elemType -> do
      let constName = "unsafe-" <> toText elemType <> "-array-internal"
      return $ CompUpIntro $ ValueVarGlobal $ wrapWithQuote constName
    _ :< TermArrayIntro elemType elems -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus axis tenv) elems
      return $
        bindLet (zip xs args') $
          CompUpIntro (ValueArrayIntro elemType xsAsVars)
    _ :< TermArrayAccess _ elemType array index -> do
      (arrayVarName, array', arrayVar) <- clarifyPlus axis tenv array
      (indexVarName, index', indexVar) <- clarifyPlus axis tenv index
      return $
        bindLet [(arrayVarName, array'), (indexVarName, index')] $
          CompArrayAccess elemType arrayVar indexVar
    m :< TermText ->
      clarifyTerm axis tenv $ m :< TermArray (PrimNumInt $ IntSize 8)
    m :< TermTextIntro text -> do
      let i8s = encode $ T.unpack text
      let i8s' = map (\x -> m :< TermInt (IntSize 8) (toInteger x)) i8s
      clarifyTerm axis tenv $ m :< TermArrayIntro (PrimNumInt (IntSize 8)) i8s'
    _ :< TermCell {} -> do
      returnCellS4 axis
    _ :< TermCellIntro contentType content -> do
      (contentTypeVarName, contentType', contentTypeVar) <- clarifyPlus axis tenv contentType
      (contentVarName, content', contentVar) <- clarifyPlus axis tenv content
      return $
        bindLet [(contentTypeVarName, contentType'), (contentVarName, content')] $
          CompUpIntro (ValueSigmaIntro [contentTypeVar, contentVar])
    _ :< TermCellRead cell -> do
      (cellVarName, cell', cellVar) <- clarifyPlus axis tenv cell
      (typeVarName, typeVar) <- Gensym.newValueVarLocalWith (axis & gensym) "typeVar"
      valueVarName <- Gensym.newIdentFromText (axis & gensym) "valueVar"
      returnClonedValue <- toRelevantApp (axis & gensym) valueVarName (CompUpIntro typeVar)
      return $
        bindLet [(cellVarName, cell')] $
          CompSigmaElim True [typeVarName, valueVarName] cellVar returnClonedValue
    _ :< TermCellWrite cell newValue -> do
      (typeVarName, typeVar) <- Gensym.newValueVarLocalWith (axis & gensym) "typeVar"
      (cellVarName, cell', cellVar) <- clarifyPlus axis tenv cell
      oldValueVarName <- Gensym.newIdentFromText (axis & gensym) "oldValueVar"
      (newValueVarName, newValue', newValueVar) <- clarifyPlus axis tenv newValue
      discardOldContent <- toAffineApp (axis & gensym) oldValueVarName (CompUpIntro typeVar)
      placeHolder <- Gensym.newIdentFromText (axis & gensym) "placeholder"
      (addrVarName, addrVar) <- Gensym.newValueVarLocalWith (axis & gensym) "address"
      return $
        bindLet [(cellVarName, cell'), (newValueVarName, newValue')] $
          CompSigmaElim True [typeVarName, oldValueVarName] cellVar $
            CompUpElim placeHolder discardOldContent $
              CompUpElim addrVarName (add cellVar (ValueInt (IntSize 64) 8)) $
                CompPrimitive $
                  PrimitiveMagic (MagicStore voidPtr addrVar newValueVar)

add :: Value -> Value -> Comp
add v1 v2 = do
  let i64 = PrimNumInt (IntSize 64)
  CompPrimitive $ PrimitivePrimOp (PrimOp "add" [i64, i64] i64) [v1, v2]

clarifyMagic :: Axis -> TypeEnv -> Magic Term -> IO Comp
clarifyMagic axis tenv der =
  case der of
    MagicCast from to value -> do
      (fromVarName, from', fromVar) <- clarifyPlus axis tenv from
      (toVarName, to', toVar) <- clarifyPlus axis tenv to
      (valueVarName, value', valueVar) <- clarifyPlus axis tenv value
      return $
        bindLet [(fromVarName, from'), (toVarName, to'), (valueVarName, value')] $
          CompPrimitive (PrimitiveMagic (MagicCast fromVar toVar valueVar))
    MagicStore lt pointer value -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus axis tenv pointer
      (valueVarName, value', valueVar) <- clarifyPlus axis tenv value
      return $
        bindLet [(pointerVarName, pointer'), (valueVarName, value')] $
          CompPrimitive (PrimitiveMagic (MagicStore lt pointerVar valueVar))
    MagicLoad lt pointer -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus axis tenv pointer
      return $
        bindLet [(pointerVarName, pointer')] $
          CompPrimitive (PrimitiveMagic (MagicLoad lt pointerVar))
    MagicSyscall syscallNum args -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus axis tenv) args
      return $
        bindLet (zip xs args') $
          CompPrimitive (PrimitiveMagic (MagicSyscall syscallNum xsAsVars))
    MagicExternal extFunName args -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus axis tenv) args
      return $
        bindLet (zip xs args') $
          CompPrimitive (PrimitiveMagic (MagicExternal extFunName xsAsVars))

clarifyLambda ::
  Axis ->
  TypeEnv ->
  LamKindF Term ->
  [(Hint, Ident, Term)] ->
  Term ->
  [BinderF Term] ->
  IO Comp
clarifyLambda axis tenv kind mxts e fvs = do
  e' <- clarifyTerm axis (insTypeEnv (catMaybes [fromLamKind kind] ++ mxts) tenv) e
  case kind of
    LamKindFix (_, x, _)
      | S.member x (freeVars e') ->
        returnClosure axis tenv OpacityOpaque kind fvs mxts e'
      | otherwise ->
        returnClosure axis tenv OpacityTransparent LamKindNormal fvs mxts e'
    _ ->
      returnClosure axis tenv OpacityTransparent kind fvs mxts e'

newClosureNames :: Gensym.Axis -> IO ((Ident, Value), Ident, (Ident, Value), (Ident, Value))
newClosureNames axis = do
  closureVarInfo <- Gensym.newValueVarLocalWith axis "closure"
  typeVarName <- Gensym.newIdentFromText axis "exp"
  envVarInfo <- Gensym.newValueVarLocalWith axis "env"
  lamVarInfo <- Gensym.newValueVarLocalWith axis "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

caseClauseToLambda :: (PatternF Term, Term) -> Term
caseClauseToLambda pat =
  case pat of
    ((mPat, _, xts), body) ->
      mPat :< TermPiIntro LamKindNormal xts body

clarifyPlus :: Axis -> TypeEnv -> Term -> IO (Ident, Comp, Value)
clarifyPlus axis tenv e = do
  e' <- clarifyTerm axis tenv e
  (varName, var) <- Gensym.newValueVarLocalWith (axis & gensym) "var"
  return (varName, e', var)

clarifyBinder :: Axis -> TypeEnv -> [BinderF Term] -> IO [(Hint, Ident, Comp)]
clarifyBinder axis tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm axis tenv t
      xts' <- clarifyBinder axis (IntMap.insert (Ident.toInt x) t tenv) xts
      return $ (m, x, t') : xts'

chainFromTermList :: TypeEnv -> [Term] -> [BinderF Term]
chainFromTermList tenv es =
  nubFreeVariables $ concatMap (chainOf tenv) es

alignFreeVariables :: Axis -> TypeEnv -> [BinderF Term] -> [Comp] -> IO [Comp]
alignFreeVariables axis tenv fvs es = do
  es' <- mapM (returnClosure axis tenv OpacityTransparent LamKindNormal fvs []) es
  mapM (\e -> callClosure (axis & gensym) e []) es'

nubFreeVariables :: [BinderF Term] -> [BinderF Term]
nubFreeVariables =
  nubBy (\(_, x, _) (_, y, _) -> x == y)

clarifyConst :: Axis -> TypeEnv -> Hint -> T.Text -> IO Comp
clarifyConst axis tenv m constName
  | Just op <- PrimOp.fromText constName =
    clarifyPrimOp axis tenv op m
  | Just _ <- PrimNum.fromText constName =
    returnImmediateS4 (axis & gensym)
  | otherwise = do
    (axis & throw & Throw.raiseCritical) m $ "undefined constant: " <> constName

clarifyPrimOp :: Axis -> TypeEnv -> PrimOp -> Hint -> IO Comp
clarifyPrimOp axis tenv op@(PrimOp _ domList _) m = do
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- unzip <$> mapM (const (Gensym.newValueVarLocalWith (axis & gensym) "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  returnClosure axis tenv OpacityTransparent LamKindNormal [] mxts $ CompPrimitive (PrimitivePrimOp op varList)

returnClosure ::
  Axis ->
  TypeEnv ->
  Opacity -> -- whether the closure is reducible
  LamKindF Term -> -- the name of newly created closure
  [BinderF Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  Comp -> -- the `e` in `lam (x1, ..., xn). e`
  IO Comp
returnClosure axis tenv isReducible kind fvs xts e = do
  fvs' <- clarifyBinder axis tenv fvs
  xts' <- clarifyBinder axis tenv xts
  let xts'' = dropFst xts'
  let fvs'' = dropFst fvs'
  fvEnvSigma <- closureEnvS4 axis $ map Right fvs''
  let fvEnv = ValueSigmaIntro (map (\(_, x, _) -> ValueVarLocal x) fvs')
  case kind of
    LamKindNormal -> do
      i <- Gensym.newCount (axis & gensym)
      name <- Locator.attachCurrentLocator (axis & locator) $ "lambda;" <> T.pack (show i)
      registerIfNecessary (axis & gensym) name isReducible False xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal (wrapWithQuote name)]
    LamKindCons _ consName consNumber _ -> do
      let consName' = getLamConsName consName
      registerIfNecessary (axis & gensym) consName' isReducible True xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueInt (IntSize 64) consNumber]
    LamKindFix (_, name, _) -> do
      let name' = Ident.toText' name
      let cls = ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal (wrapWithQuote name')]
      e' <- subst (axis & gensym) (IntMap.fromList [(Ident.toInt name, cls)]) IntMap.empty e
      registerIfNecessary (axis & gensym) name' OpacityOpaque False xts'' fvs'' e'
      return $ CompUpIntro cls

registerIfNecessary ::
  Gensym.Axis ->
  T.Text ->
  Opacity ->
  Bool ->
  [(Ident, Comp)] ->
  [(Ident, Comp)] ->
  Comp ->
  IO ()
registerIfNecessary axis name isReducible isNoetic xts1 xts2 e = do
  compDefEnv <- readIORef compDefEnvRef
  unless (name `Map.member` compDefEnv) $ do
    e' <- linearize axis (xts2 ++ xts1) e
    (envVarName, envVar) <- Gensym.newValueVarLocalWith axis "env"
    let args = map fst xts1 ++ [envVarName]
    body <- reduce axis $ CompSigmaElim False (map fst xts2) envVar e'
    insDefEnv (wrapWithQuote name) isReducible args body
    when isNoetic $ do
      bodyNoetic <- reduce axis $ CompSigmaElim True (map fst xts2) envVar e'
      insDefEnv (wrapWithQuote $ name <> ";noetic") isReducible args bodyNoetic

callClosure :: Gensym.Axis -> Comp -> [(Ident, Comp, Value)] -> IO Comp
callClosure axis e zexes = do
  let (zs, es', xs) = unzip3 zexes
  ((closureVarName, closureVar), typeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames axis
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
    _ :< TermConst _ ->
      []
    _ :< TermInt _ _ ->
      []
    _ :< TermFloat _ _ ->
      []
    _ :< TermEnum {} ->
      []
    _ :< TermEnumIntro _ _ ->
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
      let xs3 = concatMap (\((_, _, xts), body) -> chainOf' tenv xts [body]) patList
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
    EnumCaseLabel labelInfo label ->
      () :< EnumCaseLabel labelInfo label
    EnumCaseInt i ->
      () :< EnumCaseInt i
    EnumCaseDefault ->
      () :< EnumCaseDefault

getLamConsName :: T.Text -> T.Text
getLamConsName basename =
  basename <> ";cons"

getClauseConsName :: T.Text -> Bool -> T.Text
getClauseConsName basename isNoetic = do
  let consName' = getLamConsName basename
  wrapWithQuote $ if isNoetic then consName' <> ";noetic" else consName'
