module Clarify
  ( clarifyMain,
    clarifyOther,
  )
where

import Clarify.Linearize (linearize)
import Clarify.Sigma
  ( closureEnvS4,
    immediateS4,
    returnClosureS4,
    returnImmediateS4,
  )
import Clarify.Utility
  ( bindLet,
    insDefEnv,
    wrapWithQuote,
  )
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM, unless, when, (>=>))
import Data.Basic
  ( BinderF,
    CompEnumCase,
    EnumCase,
    EnumCaseF (EnumCaseDefault, EnumCaseInt, EnumCaseLabel),
    Hint,
    Ident,
    LamKindF (..),
    Opacity (..),
    PatternF,
    asInt,
    asText',
    fromLamKind,
  )
import Data.Comp
  ( Comp (..),
    CompDef,
    Primitive (PrimitiveDerangement, PrimitivePrimOp),
    Value (..),
    varComp,
  )
import Data.Global
  ( compDefEnvRef,
    newCount,
    newIdentFromText,
    newValueVarLocalWith,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef, writeIORef)
import qualified Data.IntMap as IntMap
import Data.List (nubBy)
import Data.Log (raiseCritical, raiseError)
import Data.LowType
  ( Derangement (DerangementNop),
    PrimOp (..),
    asLowTypeMaybe,
    asPrimOp,
  )
import Data.Maybe (catMaybes, isJust, maybeToList)
import Data.Namespace (attachSectionPrefix)
import qualified Data.Set as S
import Data.Stmt (Stmt (..))
import Data.Term
  ( Term,
    TermF
      ( TermConst,
        TermDerangement,
        TermEnum,
        TermEnumElim,
        TermEnumIntro,
        TermFloat,
        TermIgnore,
        TermInt,
        TermMatch,
        TermPi,
        TermPiElim,
        TermPiIntro,
        TermTau,
        TermVar,
        TermVarGlobal
      ),
    TypeEnv,
    lowTypeToType,
  )
import qualified Data.Text as T
import Reduce.Comp (reduceComp, substComp)

clarifyMain :: T.Text -> [Stmt] -> IO ([CompDef], Comp)
clarifyMain mainName defList = do
  _ <- returnImmediateS4
  _ <- returnClosureS4
  defList' <- clarifyDefList defList
  mainTerm <- reduceComp $ CompPiElimDownElim (ValueVarGlobal (wrapWithQuote mainName)) []
  return (defList', mainTerm)

clarifyOther :: [Stmt] -> IO [CompDef]
clarifyOther defList = do
  clarifyDefList defList

clarifyDefList :: [Stmt] -> IO [CompDef]
clarifyDefList defList = do
  compDefEnv <- readIORef compDefEnvRef
  writeIORef compDefEnvRef Map.empty
  defList' <- mapM clarifyDef defList
  newDefEnv <- Map.toList <$> readIORef compDefEnvRef
  modifyIORef' compDefEnvRef $ Map.union compDefEnv
  mapM_ register defList'
  defList'' <- forM defList' $ \(x, (opacity, args, e)) -> do
    e' <- reduceComp e
    return (wrapWithQuote x, (opacity, args, e'))
  return $ defList'' ++ newDefEnv

register :: (T.Text, (Opacity, [Ident], Comp)) -> IO ()
register (x, (opacity, args, e)) =
  insDefEnv (wrapWithQuote x) opacity args e

clarifyDef :: Stmt -> IO (T.Text, (Opacity, [Ident], Comp))
clarifyDef (StmtDefine opacity _ f xts _ e) = do
  e' <- clarifyTerm (insTypeEnv xts IntMap.empty) e >>= reduceComp
  let xts' = map (\(_, x, _) -> x) xts
  return (f, (opacity, xts', e'))

clarifyTerm :: TypeEnv -> Term -> IO Comp
clarifyTerm tenv term =
  case term of
    _ :< TermTau ->
      returnImmediateS4
    _ :< TermVar x -> do
      return $ CompUpIntro $ ValueVarLocal x
    _ :< TermVarGlobal x -> do
      imm <- immediateS4
      return $
        CompUpIntro $
          ValueSigmaIntro
            [ imm,
              ValueSigmaIntro [],
              ValueVarGlobal $ wrapWithQuote x
            ]
    _ :< TermPi {} ->
      returnClosureS4
    m :< TermPiIntro kind mxts e -> do
      clarifyLambda m tenv kind mxts e $ nubFreeVariables $ chainOf tenv term
    _ :< TermPiElim e es -> do
      es' <- mapM (clarifyPlus tenv) es
      e' <- clarifyTerm tenv e
      callClosure e' es'
    m :< TermConst x ->
      clarifyConst tenv m x
    _ :< TermInt size l ->
      return $ CompUpIntro (ValueInt size l)
    _ :< TermFloat size l ->
      return $ CompUpIntro (ValueFloat size l)
    _ :< TermEnum _ ->
      returnImmediateS4
    _ :< TermEnumIntro l ->
      return $ CompUpIntro $ ValueEnumIntro l
    m :< TermEnumElim (e, _) bs -> do
      let (enumCaseList, es) = unzip bs
      let fvs = chainFromTermList tenv es
      es' <- (mapM (clarifyTerm tenv) >=> alignFreeVariables tenv m fvs) es
      (y, e', yVar) <- clarifyPlus tenv e
      return $ bindLet [(y, e')] $ CompEnumElim yVar (zip (map forgetHint enumCaseList) es')
    _ :< TermDerangement expKind es -> do
      case (expKind, es) of
        (DerangementNop, [e]) ->
          clarifyTerm tenv e
        _ -> do
          (xs, es', xsAsVars) <- unzip3 <$> mapM (clarifyPlus tenv) es
          return $ bindLet (zip xs es') $ CompPrimitive (PrimitiveDerangement expKind xsAsVars)
    _ :< TermMatch mSubject (e, _) clauseList -> do
      ((dataVarName, dataVar), typeVarName, (envVarName, envVar), (tagVarName, tagVar)) <- newClosureNames
      let fvs = chainFromTermList tenv $ map caseClauseToLambda clauseList
      clauseList' <- forM (zip clauseList [0 ..]) $ \(((mPat, consName, xts), body), i) -> do
        closure <- clarifyLambda mPat tenv LamKindNormal xts body fvs
        (closureVarName, closureVar) <- newValueVarLocalWith "clause"
        return
          ( () :< EnumCaseInt i,
            CompUpElim
              closureVarName
              closure
              $ CompPiElimDownElim
                (ValueVarGlobal (getClauseConsName consName (isJust mSubject)))
                [closureVar, envVar]
          )
      dataTerm <- clarifyTerm tenv e
      return $
        CompUpElim
          dataVarName
          dataTerm
          $ CompSigmaElim
            (isJust mSubject)
            [typeVarName, envVarName, tagVarName]
            dataVar
            $ CompEnumElim tagVar clauseList'
    _ :< TermIgnore e -> do
      e' <- clarifyTerm tenv e
      return $ CompIgnore e'

clarifyLambda ::
  Hint ->
  TypeEnv ->
  LamKindF Term ->
  [(Hint, Ident, Term)] ->
  Term ->
  [BinderF Term] ->
  IO Comp
clarifyLambda m tenv kind mxts e fvs = do
  e' <- clarifyTerm (insTypeEnv (catMaybes [fromLamKind kind] ++ mxts) tenv) e
  case kind of
    LamKindFix (_, x, _)
      | S.member x (varComp e') ->
        returnClosure tenv OpacityOpaque kind fvs m mxts e'
      | otherwise ->
        returnClosure tenv OpacityTransparent LamKindNormal fvs m mxts e'
    _ ->
      returnClosure tenv OpacityTransparent kind fvs m mxts e'

newClosureNames :: IO ((Ident, Value), Ident, (Ident, Value), (Ident, Value))
newClosureNames = do
  closureVarInfo <- newValueVarLocalWith "closure"
  typeVarName <- newIdentFromText "exp"
  envVarInfo <- newValueVarLocalWith "env"
  lamVarInfo <- newValueVarLocalWith "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

caseClauseToLambda :: (PatternF Term, Term) -> Term
caseClauseToLambda pat =
  case pat of
    ((mPat, _, xts), body) ->
      mPat :< TermPiIntro LamKindNormal xts body

clarifyPlus :: TypeEnv -> Term -> IO (Ident, Comp, Value)
clarifyPlus tenv e = do
  e' <- clarifyTerm tenv e
  (varName, var) <- newValueVarLocalWith "var"
  return (varName, e', var)

clarifyBinder :: TypeEnv -> [BinderF Term] -> IO [(Hint, Ident, Comp)]
clarifyBinder tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm tenv t
      xts' <- clarifyBinder (IntMap.insert (asInt x) t tenv) xts
      return $ (m, x, t') : xts'

chainFromTermList :: TypeEnv -> [Term] -> [BinderF Term]
chainFromTermList tenv es =
  nubFreeVariables $ concatMap (chainOf tenv) es

alignFreeVariables :: TypeEnv -> Hint -> [BinderF Term] -> [Comp] -> IO [Comp]
alignFreeVariables tenv m fvs es = do
  es' <- mapM (returnClosure tenv OpacityTransparent LamKindNormal fvs m []) es
  mapM (`callClosure` []) es'

nubFreeVariables :: [BinderF Term] -> [BinderF Term]
nubFreeVariables =
  nubBy (\(_, x, _) (_, y, _) -> x == y)

clarifyConst :: TypeEnv -> Hint -> T.Text -> IO Comp
clarifyConst tenv m constName
  | Just op <- asPrimOp constName =
    clarifyPrimOp tenv op m
  | Just _ <- asLowTypeMaybe constName =
    returnImmediateS4
  | otherwise = do
    raiseCritical m $ "undefined constant: " <> constName

clarifyPrimOp :: TypeEnv -> PrimOp -> Hint -> IO Comp
clarifyPrimOp tenv op@(PrimOp _ domList _) m = do
  argTypeList <- mapM (lowTypeToType m) domList
  (xs, varList) <- unzip <$> mapM (const (newValueVarLocalWith "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  returnClosure tenv OpacityTransparent LamKindNormal [] m mxts $ CompPrimitive (PrimitivePrimOp op varList)

returnClosure ::
  TypeEnv ->
  Opacity -> -- whether the closure is reducible
  LamKindF Term -> -- the name of newly created closure
  [BinderF Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  Hint -> -- meta of lambda
  [BinderF Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  Comp -> -- the `e` in `lam (x1, ..., xn). e`
  IO Comp
returnClosure tenv isReducible kind fvs m xts e = do
  fvs' <- clarifyBinder tenv fvs
  xts' <- clarifyBinder tenv xts
  let xts'' = dropFst xts'
  let fvs'' = dropFst fvs'
  fvEnvSigma <- closureEnvS4 $ map Right fvs''
  let fvEnv = ValueSigmaIntro (map (\(_, x, _) -> ValueVarLocal x) fvs')
  case kind of
    LamKindNormal -> do
      i <- newCount
      name <- attachSectionPrefix $ "lambda;" <> T.pack (show i)
      registerIfNecessary name isReducible False xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal (wrapWithQuote name)]
    LamKindCons _ consName consNumber _ -> do
      let consName' = getLamConsName consName
      registerIfNecessary consName' isReducible True xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueInt 64 consNumber]
    LamKindFix (_, name, _) -> do
      let name' = asText' name
      let cls = ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal (wrapWithQuote name')]
      e' <- substComp (IntMap.fromList [(asInt name, cls)]) IntMap.empty e
      registerIfNecessary name' OpacityOpaque False xts'' fvs'' e'
      return $ CompUpIntro cls
    LamKindResourceHandler -> do
      unless (null fvs'') $
        raiseError m "this resource-lambda is not closed"
      e' <- linearize xts'' e >>= reduceComp
      i <- newCount
      let name = "resource-handler;" <> T.pack (show i)
      insDefEnv (wrapWithQuote name) isReducible (map fst xts'') e'
      return $ CompUpIntro $ ValueVarGlobal (wrapWithQuote name)

registerIfNecessary ::
  T.Text ->
  Opacity ->
  Bool ->
  [(Ident, Comp)] ->
  [(Ident, Comp)] ->
  Comp ->
  IO ()
registerIfNecessary name isReducible isNoetic xts1 xts2 e = do
  compDefEnv <- readIORef compDefEnvRef
  unless (name `Map.member` compDefEnv) $ do
    e' <- linearize (xts2 ++ xts1) e
    (envVarName, envVar) <- newValueVarLocalWith "env"
    let args = map fst xts1 ++ [envVarName]
    body <- reduceComp $ CompSigmaElim False (map fst xts2) envVar e'
    insDefEnv (wrapWithQuote name) isReducible args body
    when isNoetic $ do
      bodyNoetic <- reduceComp $ CompSigmaElim True (map fst xts2) envVar e'
      insDefEnv (wrapWithQuote $ name <> ";noetic") isReducible args bodyNoetic

callClosure :: Comp -> [(Ident, Comp, Value)] -> IO Comp
callClosure e zexes = do
  let (zs, es', xs) = unzip3 zexes
  ((closureVarName, closureVar), typeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames
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
      let t = (IntMap.!) tenv (asInt x)
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
    _ :< TermConst _ ->
      []
    _ :< TermInt _ _ ->
      []
    _ :< TermFloat _ _ ->
      []
    _ :< TermEnum _ ->
      []
    _ :< TermEnumIntro _ ->
      []
    _ :< TermEnumElim (e, t) les -> do
      let xs0 = chainOf tenv t
      let xs1 = chainOf tenv e
      let es = map snd les
      let xs2 = concatMap (chainOf tenv) es
      xs0 ++ xs1 ++ xs2
    _ :< TermDerangement _ es ->
      concatMap (chainOf tenv) es
    _ :< TermMatch mSubject (e, _) patList -> do
      let xs1 = concatMap (chainOf tenv) (maybeToList mSubject)
      let xs2 = chainOf tenv e
      let xs3 = concatMap (\((_, _, xts), body) -> chainOf' tenv xts [body]) patList
      xs1 ++ xs2 ++ xs3
    _ :< TermIgnore e ->
      chainOf tenv e

chainOf' :: TypeEnv -> [BinderF Term] -> [Term] -> [BinderF Term]
chainOf' tenv binder es =
  case binder of
    [] ->
      concatMap (chainOf tenv) es
    (_, x, t) : xts -> do
      let xs1 = chainOf tenv t
      let xs2 = chainOf' (IntMap.insert (asInt x) t tenv) xts es
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
      insTypeEnv rest $ IntMap.insert (asInt x) t tenv

forgetHint :: EnumCase -> CompEnumCase
forgetHint (_ :< enumCase) =
  case enumCase of
    EnumCaseLabel label ->
      () :< EnumCaseLabel label
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
