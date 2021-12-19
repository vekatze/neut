module Clarify
  ( clarify,
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
    insDefEnv',
    wrapWithQuote,
  )
import Control.Monad (forM, forM_, unless, when, (>=>))
import Data.Basic
  ( EnumCase (EnumCaseInt),
    Hint,
    Ident,
    LamKind (..),
    Opacity (OpacityTranslucent, OpacityTransparent),
    asInt,
    asText',
    fromLamKind,
    isTransparent,
    newHint,
  )
import Data.Comp
  ( Comp (..),
    Primitive (PrimitiveDerangement, PrimitivePrimOp),
    Value (..),
    varComp,
  )
import Data.Global
  ( constructorEnv,
    defEnv,
    getCurrentFilePath,
    isMain,
    newCount,
    newIdentFromText,
    newValueVarLocalWith,
    p',
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (readIORef)
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
import qualified Data.Set as S
import Data.Stmt (Stmt (..))
import Data.Term
  ( IdentPlus,
    Pattern,
    Term
      ( TermCase,
        TermConst,
        TermDerangement,
        TermEnum,
        TermEnumElim,
        TermEnumIntro,
        TermFloat,
        TermIgnore,
        TermInt,
        TermPi,
        TermPiElim,
        TermPiIntro,
        TermTau,
        TermVar,
        TermVarGlobal
      ),
    TermPlus,
    TypeEnv,
    lowTypeToType,
  )
import qualified Data.Text as T
import Path (toFilePath)
import Reduce.Comp (reduceComp, substComp)

clarify :: ([[Stmt]], [Stmt]) -> IO ([(T.Text, Comp)], Maybe Comp)
clarify (ss, mainDefList) = do
  clarifyHeader ss
  mainDefList' <- mapM clarifyDef mainDefList
  register mainDefList' -- for inlining
  mainDefList'' <- forM mainDefList' $ \(name, e) -> do
    e' <- reduceComp e
    return (name, e')
  flag <- readIORef isMain
  if not flag
    then return (mainDefList'', Nothing)
    else do
      path <- toFilePath <$> getCurrentFilePath
      let m = newHint 1 1 path
      _ <- immediateS4
      _ <- returnClosureS4
      ensureMain m
      let mainTerm = CompPiElimDownElim (ValueVarGlobal "this.main") []
      return (mainDefList'', Just mainTerm)

ensureMain :: Hint -> IO ()
ensureMain m = do
  denv <- readIORef defEnv
  case Map.lookup "this.main" denv of
    Nothing -> do
      p' $ Map.keys denv
      raiseError m "`main` is missing"
    Just _ ->
      return ()

clarifyHeader :: [[Stmt]] -> IO ()
clarifyHeader ss =
  case ss of
    [] ->
      return ()
    defList : rest -> do
      mapM clarifyDef defList >>= register'
      clarifyHeader rest

register :: [(T.Text, Comp)] -> IO ()
register defList =
  forM_ defList $ \(x, e) -> insDefEnv x True [] e

register' :: [(T.Text, Comp)] -> IO ()
register' defList =
  forM_ defList $ \(x, _) -> insDefEnv' x True []

clarifyDef :: Stmt -> IO (T.Text, Comp)
clarifyDef (StmtDef _ _ x _ e) = do
  e' <- clarifyTerm IntMap.empty e >>= reduceComp
  return (x, e')

clarifyTerm :: TypeEnv -> TermPlus -> IO Comp
clarifyTerm tenv term =
  case term of
    (_, TermTau) ->
      returnImmediateS4
    (_, TermVar x) -> do
      return $ CompUpIntro $ ValueVarLocal x
    (_, TermVarGlobal x) ->
      return $ CompPiElimDownElim (ValueVarGlobal x) []
    (_, TermPi {}) ->
      returnClosureS4
    (m, TermPiIntro opacity kind mxts e) -> do
      e' <- clarifyTerm (insTypeEnv (catMaybes [fromLamKind kind] ++ mxts) tenv) e
      let fvs = nubFreeVariables $ chainOf tenv term
      case (opacity, kind) of
        (OpacityTranslucent, LamKindFix (_, x, _))
          | not (S.member x (varComp e')) ->
            returnClosure tenv True LamKindNormal fvs m mxts e'
        _ ->
          returnClosure tenv (isTransparent opacity) kind fvs m mxts e'
    (_, TermPiElim e es) -> do
      es' <- mapM (clarifyPlus tenv) es
      e' <- clarifyTerm tenv e
      callClosure e' es'
    (m, TermConst x) ->
      clarifyConst tenv m x
    (_, TermInt size l) ->
      return $ CompUpIntro (ValueInt size l)
    (_, TermFloat size l) ->
      return $ CompUpIntro (ValueFloat size l)
    (_, TermEnum _) ->
      returnImmediateS4
    (_, TermEnumIntro l) ->
      return $ CompUpIntro $ ValueEnumIntro l
    (m, TermEnumElim (e, _) bs) -> do
      let (cs, es) = unzip bs
      let fvs = chainFromTermList tenv es
      es' <- (mapM (clarifyTerm tenv) >=> alignFreeVariables tenv m fvs) es
      (y, e', yVar) <- clarifyPlus tenv e
      return $ bindLet [(y, e')] $ CompEnumElim yVar (zip (map snd cs) es')
    (_, TermDerangement expKind es) -> do
      case (expKind, es) of
        (DerangementNop, [e]) ->
          clarifyTerm tenv e
        _ -> do
          (xs, es', xsAsVars) <- unzip3 <$> mapM (clarifyPlus tenv) es
          return $ bindLet (zip xs es') $ CompPrimitive (PrimitiveDerangement expKind xsAsVars)
    (_, TermCase resultType mSubject (e, _) patList) -> do
      let fvs = chainFromTermList tenv $ map caseClauseToLambda patList
      resultArg <- clarifyPlus tenv resultType
      closure <- clarifyTerm tenv e
      ((closureVarName, closureVar), typeVarName, (envVarName, envVar), (tagVarName, tagVar)) <- newClosureNames
      branchList <- forM (zip patList [0 ..]) $ \(((mPat, constructorName, xts), body), i) -> do
        body' <- clarifyTerm (insTypeEnv xts tenv) body
        clauseClosure <- returnClosure tenv True LamKindNormal fvs mPat xts body'
        closureArgs <- constructClauseArguments clauseClosure i $ length patList
        let (argVarNameList, argList, argVarList) = unzip3 (resultArg : closureArgs)
        let constructorName' = constructorName <> ";cons"
        let consName = wrapWithQuote $ if isJust mSubject then constructorName' <> ";noetic" else constructorName'
        return
          ( EnumCaseInt i,
            bindLet
              (zip argVarNameList argList)
              ( CompPiElimDownElim
                  (ValueVarGlobal consName)
                  (argVarList ++ [envVar])
              )
          )
      return
        ( CompUpElim
            closureVarName
            closure
            ( CompSigmaElim
                (isJust mSubject)
                [typeVarName, envVarName, tagVarName]
                closureVar
                (CompEnumElim tagVar branchList)
            )
        )
    (_, TermIgnore e) -> do
      e' <- clarifyTerm tenv e
      return $ CompIgnore e'

newClosureNames :: IO ((Ident, Value), Ident, (Ident, Value), (Ident, Value))
newClosureNames = do
  closureVarInfo <- newValueVarLocalWith "closure"
  typeVarName <- newIdentFromText "exp"
  envVarInfo <- newValueVarLocalWith "env"
  lamVarInfo <- newValueVarLocalWith "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

caseClauseToLambda :: (Pattern, TermPlus) -> TermPlus
caseClauseToLambda pat =
  case pat of
    ((mPat, _, xts), body) ->
      (mPat, TermPiIntro OpacityTransparent LamKindNormal xts body)

constructClauseArguments :: Comp -> Int -> Int -> IO [(Ident, Comp, Value)]
constructClauseArguments cls clsIndex upperBound = do
  (innerClsVarName, innerClsVar) <- newValueVarLocalWith "var"
  constructClauseArguments' (innerClsVarName, cls, innerClsVar) clsIndex 0 upperBound

constructClauseArguments' :: (Ident, Comp, Value) -> Int -> Int -> Int -> IO [(Ident, Comp, Value)]
constructClauseArguments' clsInfo clsIndex cursor upperBound = do
  if cursor == upperBound
    then return []
    else do
      rest <- constructClauseArguments' clsInfo clsIndex (cursor + 1) upperBound
      if cursor == clsIndex
        then return $ clsInfo : rest
        else do
          (argVarName, argVar) <- newValueVarLocalWith "arg"
          fakeClosure <- makeFakeClosure
          return $ (argVarName, CompUpIntro fakeClosure, argVar) : rest

makeFakeClosure :: IO Value
makeFakeClosure = do
  imm <- immediateS4
  return $ ValueSigmaIntro [imm, ValueInt 64 0, ValueInt 64 0]

clarifyPlus :: TypeEnv -> TermPlus -> IO (Ident, Comp, Value)
clarifyPlus tenv e = do
  e' <- clarifyTerm tenv e
  (varName, var) <- newValueVarLocalWith "var"
  return (varName, e', var)

clarifyBinder :: TypeEnv -> [IdentPlus] -> IO [(Hint, Ident, Comp)]
clarifyBinder tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm tenv t
      xts' <- clarifyBinder (IntMap.insert (asInt x) t tenv) xts
      return $ (m, x, t') : xts'

chainFromTermList :: TypeEnv -> [TermPlus] -> [IdentPlus]
chainFromTermList tenv es =
  nubFreeVariables $ concatMap (chainOf tenv) es

alignFreeVariables :: TypeEnv -> Hint -> [IdentPlus] -> [Comp] -> IO [Comp]
alignFreeVariables tenv m fvs es = do
  es' <- mapM (returnClosure tenv True LamKindNormal fvs m []) es
  mapM (`callClosure` []) es'

nubFreeVariables :: [IdentPlus] -> [IdentPlus]
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
  returnClosure tenv True LamKindNormal [] m mxts $ CompPrimitive (PrimitivePrimOp op varList)

returnClosure ::
  TypeEnv ->
  Bool -> -- whether the closure is reducible
  LamKind IdentPlus -> -- the name of newly created closure
  [IdentPlus] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  Hint -> -- meta of lambda
  [IdentPlus] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
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
      let name = "thunk;" <> T.pack (show i)
      registerIfNecessary name isReducible False xts'' fvs'' e
      return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal (wrapWithQuote name)]
    LamKindCons _ consName -> do
      cenv <- readIORef constructorEnv
      case Map.lookup consName cenv of
        Just (_, constructorNumber) -> do
          let consName' = consName <> ";cons"
          registerIfNecessary consName' isReducible True xts'' fvs'' e
          return $ CompUpIntro $ ValueSigmaIntro [fvEnvSigma, fvEnv, ValueInt 64 (toInteger constructorNumber)]
        Nothing ->
          raiseCritical m $ "no such constructor is registered: `" <> consName <> "`"
    LamKindFix (_, name, _) -> do
      let name' = asText' name
      let cls = ValueSigmaIntro [fvEnvSigma, fvEnv, ValueVarGlobal (wrapWithQuote name')]
      e' <- substComp (IntMap.fromList [(asInt name, cls)]) IntMap.empty e
      registerIfNecessary name' False False xts'' fvs'' e'
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
  Bool ->
  Bool ->
  [(Ident, Comp)] ->
  [(Ident, Comp)] ->
  Comp ->
  IO ()
registerIfNecessary name isReducible isNoetic xts1 xts2 e = do
  denv <- readIORef defEnv
  unless (name `Map.member` denv) $ do
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

chainOf :: TypeEnv -> TermPlus -> [IdentPlus]
chainOf tenv term =
  case term of
    (_, TermTau) ->
      []
    (m, TermVar x) -> do
      let t = (IntMap.!) tenv (asInt x)
      let xts = chainOf tenv t
      xts ++ [(m, x, t)]
    (_, TermVarGlobal {}) ->
      []
    (_, TermPi {}) ->
      []
    (_, TermPiIntro _ kind xts e) ->
      chainOf' tenv (catMaybes [fromLamKind kind] ++ xts) [e]
    (_, TermPiElim e es) -> do
      let xs1 = chainOf tenv e
      let xs2 = concatMap (chainOf tenv) es
      xs1 ++ xs2
    (_, TermConst _) ->
      []
    (_, TermInt _ _) ->
      []
    (_, TermFloat _ _) ->
      []
    (_, TermEnum _) ->
      []
    (_, TermEnumIntro _) ->
      []
    (_, TermEnumElim (e, t) les) -> do
      let xs0 = chainOf tenv t
      let xs1 = chainOf tenv e
      let es = map snd les
      let xs2 = concatMap (chainOf tenv) es
      xs0 ++ xs1 ++ xs2
    (_, TermDerangement _ es) ->
      concatMap (chainOf tenv) es
    (_, TermCase _ mSubject (e, _) patList) -> do
      let xs1 = concatMap (chainOf tenv) (maybeToList mSubject)
      let xs2 = chainOf tenv e
      let xs3 = concatMap (\((_, _, xts), body) -> chainOf' tenv xts [body]) patList
      xs1 ++ xs2 ++ xs3
    (_, TermIgnore e) ->
      chainOf tenv e

chainOf' :: TypeEnv -> [IdentPlus] -> [TermPlus] -> [IdentPlus]
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

insTypeEnv :: [IdentPlus] -> TypeEnv -> TypeEnv
insTypeEnv xts tenv =
  case xts of
    [] ->
      tenv
    (_, x, t) : rest ->
      insTypeEnv rest $ IntMap.insert (asInt x) t tenv
