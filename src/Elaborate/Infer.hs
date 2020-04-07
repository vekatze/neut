{-# LANGUAGE OverloadedStrings #-}

module Elaborate.Infer
  ( infer
  , inferType
  , inferSigma
  -- , insLevelEQ
  -- , insLevelLE
  , insConstraintEnv
  -- , univInstWith
  -- , instantiate
  , insWeakTypeEnv
  , newTypeHoleInCtx
  , Context
  ) where

import Control.Monad.Except
import Control.Monad.State

-- import Data.Maybe (maybeToList)
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IntMap

-- import qualified Data.Set as S
import qualified Data.Text as T

import Data.Basic
import Data.Env
import Data.Term hiding (IdentifierPlus)
import Data.WeakTerm

type Context = [IdentifierPlus]

-- type Context = [(IdentifierPlus, UnivLevelPlus)]
-- type Context = [(Identifier, WeakTermPlus)]
-- Given a term and a context, return the type of the term, updating the
-- constraint environment. This is more or less the same process in ordinary
-- Hindley-Milner type inference algorithm. The difference is that, when we
-- create a type variable, the type variable may depend on terms.
-- For example, consider generating constraints from an application `e1 @ e2`.
-- In ordinary predicate logic, we generate a type variable `?M` and add a
-- constraint `<type-of-e1> == <type-of-e2> -> ?M`. In dependent situation, however,
-- we cannot take this approach, since the `?M` may depend on other terms defined
-- beforehand. If `?M` depends on other terms, we cannot define substitution for terms
-- that contain metavariables because we don't know whether a substitution {x := e}
-- affects the content of a metavariable.
-- To handle this situation, we define metavariables to be *closed*. To represent
-- dependence, we apply all the names defined beforehand to the metavariables.
-- In other words, when we generate a metavariable, we use `?M @ (x1, ..., xn)` as a
-- representation of the hole, where x1, ..., xn are the defined names, or the context.
-- With this design, we can handle dependence in a simple way. This design decision
-- is due to "Elaboration in Dependent Type Theory". There also exists an approach
-- that deals with this situation which uses so-called contextual modality.
-- Interested readers are referred to A. Abel and B. Pientka. "Higher-Order
-- Dynamic Pattern Unification for Dependent Types and Records". Typed Lambda
-- Calculi and Applications, 2011.
-- infer :: WeakTermPlus -> WithEnv (WeakTermPlus, WeakTermPlus, UnivLevelPlus)
infer :: WeakTermPlus -> WithEnv (WeakTermPlus, WeakTermPlus)
infer e = infer' [] e

-- inferType :: WeakTermPlus -> WithEnv (WeakTermPlus, UnivLevelPlus)
inferType :: WeakTermPlus -> WithEnv WeakTermPlus
inferType t = inferType' [] t

infer' :: Context -> WeakTermPlus -> WithEnv (WeakTermPlus, WeakTermPlus)
  -- -> WithEnv (WeakTermPlus, WeakTermPlus, UnivLevelPlus)
infer' _ (m, WeakTermTau) = return ((m, WeakTermTau), (m, WeakTermTau))
  -- ml0 <- newLevelLT m []
  -- ml1 <- newLevelLT m [ml0]
  -- ml2 <- newLevelLT m [ml1]
  -- return (asUniv ml0, asUniv ml1, ml2)
infer' _ (m, WeakTermUpsilon x) = inferSymbol m x
infer' ctx (m, WeakTermPi mName xts t) = do
  (xts', t') <- inferPi ctx xts t
  -- let (xts', mlPiArgs) = unzip xtls'
  -- ml0 <- newLevelLE m $ mlPiCod : mlPiArgs
  -- ml1 <- newLevelLT m [ml0]
  return ((m, WeakTermPi mName xts' t'), (m, WeakTermTau))
  -- return ((m, WeakTermPi mName xts' t'), (asUniv ml0), ml1)
infer' ctx (m, WeakTermPiIntro xts e) = do
  (xts', (e', t')) <- inferBinder ctx xts e
  -- let (xts', mlPiArgs) = unzip xtls'
  -- mlPi <- newLevelLE m $ mlPiCod : mlPiArgs
  return ((m, WeakTermPiIntro xts' e'), (m, weakTermPi xts' t'))
  -- (xtls', (e', t', mlPiCod)) <- inferBinder ctx xts e
  -- let (xts', mlPiArgs) = unzip xtls'
  -- mlPi <- newLevelLE m $ mlPiCod : mlPiArgs
  -- return ((m, WeakTermPiIntro xts' e'), (m, weakTermPi xts' t'), mlPi)
infer' ctx (m, WeakTermPiIntroNoReduce xts e) = do
  (xts', (e', t')) <- inferBinder ctx xts e
  -- let (xts', mlPiArgs) = unzip xtls'
  -- mlPi <- newLevelLE m $ mlPiCod : mlPiArgs
  return ((m, WeakTermPiIntroNoReduce xts' e'), (m, weakTermPi xts' t'))
infer' ctx (m, WeakTermPiIntroPlus ind (name, is, args1, args2) xts e) = do
  let args = args1 ++ args2
  args' <- inferSigma ctx args
  let args1' = take (length args1) args'
  let args2' = drop (length args1) args'
  (xts', (e', t')) <- inferBinder ctx xts e
  -- let (xts', mlPiArgs) = unzip xtls'
  -- mlPi <- newLevelLE m $ mlPiCod : mlPiArgs
  return
    ( (m, WeakTermPiIntroPlus ind (name, is, args1', args2') xts' e')
    , (m, WeakTermPi (Just $ asText ind) xts' t'))
infer' ctx (m, WeakTermPiElim e es) = do
  es' <- insertHoleIfNecessary e es
  etls <- mapM (infer' ctx) es'
  etl <- infer' ctx e
  inferPiElim ctx m etl etls
infer' ctx (m, WeakTermIter (mx, x, t) xts e) = do
  t' <- inferType' ctx t
  insWeakTypeEnv x t'
  -- Note that we cannot extend context with x. The type of e cannot be dependent on `x`.
  -- Otherwise the type of `mu x. e` might have `x` as free variable, which is unsound.
  (xts', (e', tCod)) <- inferBinder ctx xts e
  -- let (xts', mlPiArgs) = unzip xtls'
  -- mlPi <- newLevelLE m $ mlPiCod : mlPiArgs
  let piType = (m, weakTermPi xts' tCod)
  insConstraintEnv piType t'
  -- insLevelEQ mlPi ml'
  return ((m, WeakTermIter (mx, x, t') xts' e'), piType)
infer' ctx (m, WeakTermZeta x) = do
  zenv <- gets zetaEnv
  case IntMap.lookup (asInt x) zenv of
    Just hole -> return hole
    Nothing -> do
      (app, higherApp) <- newHoleInCtx ctx m
      modify
        (\env -> env {zetaEnv = IntMap.insert (asInt x) (app, higherApp) zenv})
      return (app, higherApp)
infer' _ (m, WeakTermConst x@(I (s, _)))
  -- i64, f16, u8, etc.
  | Just _ <- asLowTypeMaybe s
    -- ml0 <- newLevelLE m []
    -- ml1 <- newLevelLT m [ml0]
    -- i64とかf16とかは定義に展開されないのでunivParamsをもつ必要がない、はず
   = do return ((m, WeakTermConst x), (m, WeakTermTau))
  | Just op <- asUnaryOpMaybe s = inferExternal m x (unaryOpToType m op)
  | Just op <- asBinaryOpMaybe s = inferExternal m x (binaryOpToType m op)
  | Just lt <- asArrayAccessMaybe s = inferExternal m x (arrayAccessToType m lt)
  | otherwise = inferSymbol m x
infer' _ (m, WeakTermInt t i) = do
  t' <- inferType' [] t -- ctx == [] since t' should be i64, i8, etc. (i.e. t must be closed)
  return ((m, WeakTermInt t' i), t')
  -- (t', UnivLevelPlus (_, l)) <- inferType' [] t -- ctx == [] since t' should be i64, i8, etc. (i.e. t must be closed)
  -- return ((m, WeakTermInt t' i), t', UnivLevelPlus (m, l))
infer' _ (m, WeakTermFloat16 f)
  -- ml <- newLevelLE m []
 = do
  (_, f16) <- lookupConstantPlus m "f16"
  return ((m, WeakTermFloat16 f), (m, f16))
infer' _ (m, WeakTermFloat32 f)
  -- ml <- newLevelLE m []
 = do
  (_, f32) <- lookupConstantPlus m "f32"
  return ((m, WeakTermFloat32 f), (m, f32))
infer' _ (m, WeakTermFloat64 f)
  -- ml <- newLevelLE m []
 = do
  (_, f64) <- lookupConstantPlus m "f64"
  return ((m, WeakTermFloat64 f), (m, f64))
infer' _ (m, WeakTermFloat t f) = do
  t' <- inferType' [] t -- t must be closed
  return ((m, WeakTermFloat t' f), t')
  -- (t', UnivLevelPlus (_, l)) <- inferType' [] t -- t must be closed
  -- return ((m, WeakTermFloat t' f), t', UnivLevelPlus (m, l))
infer' _ (m, WeakTermEnum name)
  -- ml0 <- newLevelLE m []
  -- ml1 <- newLevelLT m [ml0]
 = do
  return ((m, WeakTermEnum name), (m, WeakTermTau))
infer' _ (m, WeakTermEnumIntro v)
  -- ml <- newLevelLE m []
 = do
  case v of
    EnumValueIntS size _ -> do
      let t = (m, WeakTermEnum (EnumTypeIntS size))
      return ((m, WeakTermEnumIntro v), t)
    EnumValueIntU size _ -> do
      let t = (m, WeakTermEnum (EnumTypeIntU size))
      return ((m, WeakTermEnumIntro v), t)
    EnumValueLabel l -> do
      k <- lookupKind m l
      let t = (m, WeakTermEnum $ EnumTypeLabel k)
      return ((m, WeakTermEnumIntro v), t)
infer' ctx (m, WeakTermEnumElim (e, t) ces) = do
  tEnum <- inferType' ctx t
  (e', t') <- infer' ctx e
  insConstraintEnv tEnum t'
  -- insLevelEQ mlEnum ml'
  if null ces
    then do
      h <- newTypeHoleInCtx ctx m
      -- ml <- newLevelLE m []
      return ((m, WeakTermEnumElim (e', t') []), h) -- ex falso quodlibet
    else do
      let (cs, es) = unzip ces
      (cs', tcs) <- unzip <$> mapM (inferWeakCase ctx) cs
      forM_ (zip (repeat t') tcs) $ uncurry insConstraintEnv
      (es', ts) <- unzip <$> mapM (infer' ctx) es
      constrainList $ ts
      -- constrainList $ map asUniv mls
      return ((m, WeakTermEnumElim (e', t') $ zip cs' es'), head ts)
infer' ctx (m, WeakTermArray dom k) = do
  (dom', tDom) <- infer' ctx dom
  let tDom' = (m, WeakTermEnum (EnumTypeIntU 64))
  insConstraintEnv tDom tDom'
  -- ml0 <- newLevelLE m [mlDom]
  -- ml1 <- newLevelLT m [ml0]
  return ((m, WeakTermArray dom' k), (m, WeakTermTau))
infer' ctx (m, WeakTermArrayIntro k es) = do
  tCod <- inferKind m k
  (es', ts) <- unzip <$> mapM (infer' ctx) es
  forM_ (zip ts (repeat tCod)) $ uncurry insConstraintEnv
  -- constrainList $ map asUniv mls
  let len = toInteger $ length es
  let dom = (m, WeakTermEnumIntro (EnumValueIntU 64 len))
  let t = (m, WeakTermArray dom k)
  -- ml <- newLevelLE m mls
  return ((m, WeakTermArrayIntro k es'), t)
infer' ctx (m, WeakTermArrayElim k xts e1 e2) = do
  (e1', t1) <- infer' ctx e1
  (xts', (e2', t2)) <- inferBinder ctx xts e2
  -- let (xts', mls) = unzip xtls'
  -- forM_ mls $ \mlArrArg -> insLevelLE mlArrArg mlArr
  let len = toInteger $ length xts
  let dom = (m, WeakTermEnumIntro (EnumValueIntU 64 len))
  insConstraintEnv t1 (fst e1', WeakTermArray dom k)
  let ts = map (\(_, _, t) -> t) xts'
  tCod <- inferKind m k
  forM_ (zip ts (repeat tCod)) $ uncurry insConstraintEnv
  return ((m, WeakTermArrayElim k xts' e1' e2'), t2)
infer' _ (m, WeakTermStruct ts)
  -- ml0 <- newLevelLE m []
  -- ml1 <- newLevelLT m [ml0]
 = do
  return ((m, WeakTermStruct ts), (m, WeakTermTau))
infer' ctx (m, WeakTermStructIntro eks) = do
  let (es, ks) = unzip eks
  ts <- mapM (inferKind m) ks
  let structType = (m, WeakTermStruct ks)
  (es', ts') <- unzip <$> mapM (infer' ctx) es
  forM_ (zip ts' ts) $ uncurry insConstraintEnv
  -- ml <- newLevelLE m mls
  return ((m, WeakTermStructIntro $ zip es' ks), structType)
infer' ctx (m, WeakTermStructElim xks e1 e2) = do
  (e1', t1) <- infer' ctx e1
  let (ms, xs, ks) = unzip3 xks
  ts <- mapM (inferKind m) ks
  -- ls <- mapM (const newCount) ts
  -- let mls = map UnivLevelPlus $ zip (repeat m) ls
  -- forM_ mls $ \mlStructArg -> insLevelLE mlStructArg mlStruct
  let structType = (fst e1', WeakTermStruct ks)
  insConstraintEnv t1 structType
  forM_ (zip xs ts) $ uncurry insWeakTypeEnv
  (e2', t2) <- infer' (ctx ++ zip3 ms xs ts) e2
  return ((m, WeakTermStructElim xks e1' e2'), t2)
infer' ctx (m, WeakTermCase indName e cxtes) = do
  (e', t') <- infer' ctx e
  resultType <- newTypeHoleInCtx ctx m
  -- resultLevel <- newLevelLE m []
  if null cxtes
    then do
      return ((m, WeakTermCase indName e' []), resultType) -- ex falso quodlibet
    else do
      (name@(I (nameStr, _)), indInfo) <- getIndInfo $ map (fst . fst) cxtes
      (indType, argHoleList) <- constructIndType m ctx name
      -- indType = a @ (HOLE, ..., HOLE)
      insConstraintEnv indType t'
      cxtes' <-
        forM (zip indInfo cxtes) $ \(is, (((mc, c), patArgs), body)) -> do
          let usedHoleList = map (\i -> argHoleList !! i) is
          let var = (mc {metaIsExplicit = True}, WeakTermUpsilon c)
          patArgs' <- inferPatArgs ctx patArgs
          let items =
                map (\(mx, x, tx) -> ((mx, WeakTermUpsilon x), tx)) patArgs'
          etl <- infer' ctx var
          _ <- inferPiElim ctx m etl (usedHoleList ++ items)
          -- insLevelEQ ml' appLevel
          (body', bodyType) <- infer' (ctx ++ patArgs') body
          insConstraintEnv resultType bodyType
          -- insLevelEQ resultLevel bodyLevel
          xts <- mapM (toIdentPlus mc) usedHoleList
          return (((mc, c), xts ++ patArgs'), body')
      return ((m, WeakTermCase nameStr e' cxtes'), resultType)

toIdentPlus :: Meta -> (WeakTermPlus, WeakTermPlus) -> WithEnv IdentifierPlus
toIdentPlus m (_, t) = do
  x <- newNameWith' "pat"
  return (m, x, t)

applyArgs ::
     Meta
  -> [IdentifierPlus]
  -> [(WeakTermPlus, WeakTermPlus)]
  -> WithEnv ([IdentifierPlus], [IdentifierPlus])
applyArgs _ xts [] = return (xts, [])
applyArgs m ((mx, x, t):xts) ((hole, higherHole):ets) = do
  insConstraintEnv t higherHole
  let xts' = substWeakTermPlus' [(x, hole)] xts
  (xts'', tmp) <- applyArgs m xts' ets
  return (xts'', tmp ++ [(mx, x, higherHole)])
applyArgs m [] _ = raiseCritical m $ "invalid argument passed to applyArgs"

-- indの名前から定義をlookupして、それにholeを適切に適用したものを返す。あとholeも（型情報つきで）返す。
constructIndType ::
     Meta
  -> Context
  -> Identifier
  -> WithEnv (WeakTermPlus, [(WeakTermPlus, WeakTermPlus)])
constructIndType m ctx i = do
  cenv <- gets cacheEnv
  case IntMap.lookup (asInt i) cenv of
    Just (Left e) -> do
      case weaken e of
        e'@(me, WeakTermPiIntro xts _) -> do
          holeList <- mapM (const $ newHoleInCtx ctx me) xts
          _ <- applyArgs m xts holeList
          let es = map (\(h, _) -> h) holeList
          return ((me, WeakTermPiElim e' es), holeList)
        e' ->
          raiseCritical m $
          "the definition of inductive type must be of the form `(lambda (xts) (...))`, but is:\n" <>
          toText e'
    _ -> raiseCritical m $ "no such inductive type defined: " <> asText i

getIndInfo :: [(Meta, Identifier)] -> WithEnv (Identifier, [[Int]])
getIndInfo cs = do
  (indNameList, usedPosList) <- unzip <$> mapM getIndInfo' cs
  checkIntegrity indNameList
  return (snd $ head indNameList, usedPosList)

getIndInfo' :: (Meta, Identifier) -> WithEnv ((Meta, Identifier), [Int])
getIndInfo' (m, c@(I (_, j))) = do
  cienv <- gets consToInd
  caenv <- gets consToArgs
  case (IntMap.lookup j cienv, IntMap.lookup j caenv) of
    (Just i, Just is) -> return ((m, i), is)
    _ -> raiseError m $ "no such constructor defined: " <> asText c

checkIntegrity :: [(Meta, Identifier)] -> WithEnv ()
checkIntegrity [] = return ()
checkIntegrity (mi:is) = checkIntegrity' mi is

checkIntegrity' :: (Meta, Identifier) -> [(Meta, Identifier)] -> WithEnv ()
checkIntegrity' _ [] = return ()
checkIntegrity' i (j:js) =
  if snd i == snd j
    then checkIntegrity' i js
    else raiseError (supMeta (fst i) (fst j)) "foo"

inferPatArgs :: Context -> [IdentifierPlus] -> WithEnv [IdentifierPlus]
inferPatArgs _ [] = return []
inferPatArgs ctx ((mx, x, t):xts) = do
  t' <- inferType' ctx t
  insWeakTypeEnv x t'
  xts' <- inferPatArgs (ctx ++ [(mx, x, t')]) xts
  return $ (mx, x, t') : xts'

insertHoleIfNecessary ::
     WeakTermPlus -> [WeakTermPlus] -> WithEnv [WeakTermPlus]
insertHoleIfNecessary (m, WeakTermUpsilon x) es
  | not (metaIsExplicit m) = insertHoleIfNecessary' m x es
insertHoleIfNecessary (m, WeakTermConst x) es
  | not (metaIsExplicit m) = insertHoleIfNecessary' m x es
insertHoleIfNecessary _ es = return es

insertHoleIfNecessary' ::
     Meta -> Identifier -> [WeakTermPlus] -> WithEnv [WeakTermPlus]
insertHoleIfNecessary' m x es = do
  ienv <- gets impEnv
  case IntMap.lookup (asInt x) ienv of
    Nothing -> return es
    Just is -> do
      tl <- lookupTypeEnv m x
      case tl of
        (_, TermPi _ xts _) -> supplyHole' m 0 (length xts) is es
        t ->
          raiseCritical m $
          "the type of `" <>
          asText x <> "` must be a Pi-type, but is:\n" <> toText (weaken t)

supplyHole' ::
     Meta -> Int -> Int -> [Int] -> [WeakTermPlus] -> WithEnv [WeakTermPlus]
supplyHole' m idx len is es
  | idx < len = do
    if idx `elem` is
      then do
        xts' <- supplyHole' m (idx + 1) len is es
        h <- newHole m
        return $ h : xts'
      else supplyHole' m (idx + 1) len is es
  | otherwise = return es

inferExternal ::
     Meta
  -> Identifier
  -> WithEnv TermPlus
  -> WithEnv (WeakTermPlus, WeakTermPlus)
inferExternal m x comp = do
  t <- comp
  -- l <- newCount
  -- (_, t', l') <- instantiate m t l
  return ((m, WeakTermConst x), weaken t)

inferSymbol :: Meta -> Identifier -> WithEnv (WeakTermPlus, WeakTermPlus)
inferSymbol m x = do
  mt <- lookupTypeEnvMaybe x
  case mt of
    Nothing -> do
      (_, t) <- lookupWeakTypeEnv m x
      return ((m, WeakTermUpsilon x), (m, t))
      -- ((_, t), UnivLevelPlus (_, l)) <- lookupWeakTypeEnv m x
      -- return ((m, WeakTermUpsilon x), (m, t), UnivLevelPlus (m, l)) -- infer時点で型環境に登録されていない <=> xは変数
    Just t
      -- (up, t', l') <- instantiate m t l
     -> return ((m, WeakTermConst x), weaken t)

-- instantiate ::
--      Meta
--   -> TermPlus
--   -> Int
--   -> WithEnv (UnivParams, WeakTermPlus, UnivLevelPlus)
-- instantiate m t l = do
--   (up, (_, t'), l') <- univInst (weaken t) l
--   return (up, (m, t'), UnivLevelPlus (m, l'))
inferType' :: Context -> WeakTermPlus -> WithEnv WeakTermPlus
inferType' ctx t = do
  (t', u) <- infer' ctx t
  -- ml <- newLevelLE (fst t') []
  insConstraintEnv u (metaOf t, WeakTermTau)
  -- insLevelLT ml l
  return t'

inferKind :: Meta -> ArrayKind -> WithEnv WeakTermPlus
inferKind m (ArrayKindIntS i) = return (m, WeakTermEnum (EnumTypeIntS i))
inferKind m (ArrayKindIntU i) = return (m, WeakTermEnum (EnumTypeIntU i))
inferKind m (ArrayKindFloat size) = do
  (_, t) <- lookupConstantPlus m $ "f" <> T.pack (show (sizeAsInt size))
  return (m, t)
inferKind m _ = raiseCritical m "inferKind for void-pointer"

inferPi ::
     Context
  -> [IdentifierPlus]
  -> WeakTermPlus
  -> WithEnv ([IdentifierPlus], WeakTermPlus)
inferPi ctx [] cod = do
  (cod', mlPiCod) <- inferType' ctx cod
  return ([], (cod', mlPiCod))
inferPi ctx ((mx, x, t):xts) cod = do
  t' <- inferType' ctx t
  insWeakTypeEnv x t'
  (xtls', tlCod) <- inferPi (ctx ++ [(mx, x, t')]) xts cod
  return ((mx, x, t') : xtls', tlCod)

inferSigma :: Context -> [IdentifierPlus] -> WithEnv [IdentifierPlus]
inferSigma _ [] = return []
inferSigma ctx ((mx, x, t):xts) = do
  t' <- inferType' ctx t
  insWeakTypeEnv x t'
  xts' <- inferSigma (ctx ++ [(mx, x, t')]) xts
  return $ (mx, x, t') : xts'

inferBinder ::
     Context
  -> [IdentifierPlus]
  -> WeakTermPlus
  -> WithEnv ([IdentifierPlus], (WeakTermPlus, WeakTermPlus))
inferBinder ctx [] e = do
  etl' <- infer' ctx e
  return ([], etl')
inferBinder ctx ((mx, x, t):xts) e = do
  t' <- inferType' ctx t
  insWeakTypeEnv x t'
  (xts', etl') <- inferBinder (ctx ++ [(mx, x, t')]) xts e
  return ((mx, x, t') : xts', etl')

inferPiElim ::
     Context
  -> Meta
  -> (WeakTermPlus, WeakTermPlus)
  -> [(WeakTermPlus, WeakTermPlus)]
  -> WithEnv (WeakTermPlus, WeakTermPlus)
inferPiElim ctx m (e, t) ets = do
  let (es, ts) = unzip ets
  case t of
    (_, WeakTermPi _ xts cod)
      | length xts == length ets -> do
        let xs = map (\(_, x, _) -> x) xts
        let ts'' = map (\(_, _, tx) -> substWeakTermPlus (zip xs es) tx) xts
        forM_ (zip ts'' ts) $ uncurry insConstraintEnv
        let cod' = substWeakTermPlus (zip xs es) cod
        return ((m, WeakTermPiElim e es), cod')
    _ -> do
      ys <- mapM (const $ newNameWith' "arg") es
      yts <- newTypeHoleListInCtx ctx $ zip ys (map fst es)
      let ts'' = map (\(_, _, ty) -> substWeakTermPlus (zip ys es) ty) yts
      cod <- newTypeHoleInCtx (ctx ++ yts) m
      insConstraintEnv t (fst e, weakTermPi yts cod)
      forM_ (zip ts ts'') $ uncurry insConstraintEnv
      return ((m, WeakTermPiElim e es), substWeakTermPlus (zip ys es) cod)

-- In a context (x1 : A1, ..., xn : An), this function creates metavariables
--   ?M  : Pi (x1 : A1, ..., xn : An). ?Mt @ (x1, ..., xn)
--   ?Mt : Pi (x1 : A1, ..., xn : An). Univ
-- and return ?M @ (x1, ..., xn) : ?Mt @ (x1, ..., xn).
-- Note that we can't just set `?M : Pi (x1 : A1, ..., xn : An). Univ` since
-- WeakTermZeta might be used as an ordinary term, that is, a term which is not a type.
-- {} newHoleInCtx {}
newHoleInCtx :: Context -> Meta -> WithEnv (WeakTermPlus, WeakTermPlus)
newHoleInCtx ctx m = do
  higherHole <- newHole m
  let varSeq = map (\(_, x, _) -> (m, WeakTermUpsilon x)) ctx
  let higherApp = (m, WeakTermPiElim higherHole varSeq)
  hole <- newHole m
  let app = (m, WeakTermPiElim hole varSeq)
  -- ml <- newLevelLE m []
  return (app, higherApp)

-- In a context (x1 : A1, ..., xn : An), this function creates a metavariable
--   ?M  : Pi (x1 : A1, ..., xn : An). Univ{i}
-- and return ?M @ (x1, ..., xn) : Univ{i}.
newTypeHoleInCtx :: Context -> Meta -> WithEnv WeakTermPlus
newTypeHoleInCtx ctx m = do
  let varSeq = map (\(_, x, _) -> (m, WeakTermUpsilon x)) ctx
  hole <- newHole m
  return (m, WeakTermPiElim hole varSeq)

-- In context ctx == [x1, ..., xn], `newTypeHoleListInCtx ctx [y1, ..., ym]` generates
-- the following list:
--
--   [(y1,   ?M1   @ (x1, ..., xn)),
--    (y2,   ?M2   @ (x1, ..., xn, y1),
--    ...,
--    (y{m}, ?M{m} @ (x1, ..., xn, y1, ..., y{m-1}))]
--
-- inserting type information `yi : ?Mi @ (x1, ..., xn, y1, ..., y{i-1})
newTypeHoleListInCtx ::
     Context -> [(Identifier, Meta)] -> WithEnv [IdentifierPlus]
newTypeHoleListInCtx _ [] = return []
newTypeHoleListInCtx ctx ((x, m):rest) = do
  t <- newTypeHoleInCtx ctx m
  -- ml <- newLevelLE m []
  insWeakTypeEnv x t
  ts <- newTypeHoleListInCtx (ctx ++ [(m, x, t)]) rest
  return $ (m, x, t) : ts

inferWeakCase :: Context -> WeakCasePlus -> WithEnv (WeakCasePlus, WeakTermPlus)
inferWeakCase _ l@(m, WeakCaseLabel name) = do
  k <- lookupKind m name
  return (l, (m, WeakTermEnum $ EnumTypeLabel k))
inferWeakCase _ l@(m, WeakCaseIntS size _) =
  return (l, (m, WeakTermEnum (EnumTypeIntS size)))
inferWeakCase _ l@(m, WeakCaseIntU size _) =
  return (l, (m, WeakTermEnum (EnumTypeIntU size)))
inferWeakCase ctx (m, WeakCaseInt t a) = do
  t' <- inferType' ctx t
  return ((m, WeakCaseInt t' a), t')
inferWeakCase ctx (m, WeakCaseDefault) = do
  h <- newTypeHoleInCtx ctx m
  return ((m, WeakCaseDefault), h)

constrainList :: [WeakTermPlus] -> WithEnv ()
constrainList [] = return ()
constrainList [_] = return ()
constrainList (t1:t2:ts) = do
  insConstraintEnv t1 t2
  constrainList $ t2 : ts

insConstraintEnv :: WeakTermPlus -> WeakTermPlus -> WithEnv ()
insConstraintEnv t1 t2 =
  modify (\e -> e {constraintEnv = (t1, t2) : constraintEnv e})

insWeakTypeEnv :: Identifier -> WeakTermPlus -> WithEnv ()
insWeakTypeEnv (I (_, i)) t =
  modify (\e -> e {weakTypeEnv = IntMap.insert i t (weakTypeEnv e)})

lookupWeakTypeEnv :: Meta -> Identifier -> WithEnv WeakTermPlus
lookupWeakTypeEnv m s = do
  mt <- lookupWeakTypeEnvMaybe s
  case mt of
    Just t -> return t
    Nothing ->
      raiseCritical m $
      asText' s <> " is not found in the weak type environment."

lookupWeakTypeEnvMaybe :: Identifier -> WithEnv (Maybe WeakTermPlus)
lookupWeakTypeEnvMaybe (I (_, s)) = do
  mt <- gets (IntMap.lookup s . weakTypeEnv)
  case mt of
    Nothing -> return Nothing
    Just t -> return $ Just t

lookupKind :: Meta -> T.Text -> WithEnv T.Text
lookupKind m name = do
  renv <- gets revEnumEnv
  case Map.lookup name renv of
    Nothing -> raiseError m $ "no such enum-intro is defined: " <> name
    Just (j, _) -> return j
-- newLevelLE :: Meta -> [UnivLevelPlus] -> WithEnv UnivLevelPlus
-- newLevelLE m mls = do
--   l <- newCount
--   let ml = UnivLevelPlus (m, l)
--   forM_ mls $ \ml' -> insLevelLE ml' ml
--   return ml
-- newLevelLT :: Meta -> [UnivLevelPlus] -> WithEnv UnivLevelPlus
-- newLevelLT m mls = do
--   l <- newCount
--   let ml = UnivLevelPlus (m, l)
--   forM_ mls $ \ml' -> insLevelLT ml' ml
--   return ml
-- insLevelLE :: UnivLevelPlus -> UnivLevelPlus -> WithEnv ()
-- insLevelLE ml1 ml2 =
--   modify (\env -> env {levelEnv = (ml1, (0, ml2)) : levelEnv env})
-- insLevelLT :: UnivLevelPlus -> UnivLevelPlus -> WithEnv ()
-- insLevelLT ml1 ml2 =
--   modify (\env -> env {levelEnv = (ml1, (1, ml2)) : levelEnv env})
-- insLevelEQ :: UnivLevelPlus -> UnivLevelPlus -> WithEnv ()
-- insLevelEQ (UnivLevelPlus (_, l1)) (UnivLevelPlus (_, l2)) = do
--   modify (\env -> env {equalityEnv = (l1, l2) : equalityEnv env})
-- univInst ::
--      WeakTermPlus -> UnivLevel -> WithEnv (UnivParams, WeakTermPlus, UnivLevel)
-- univInst e l = do
--   modify (\env -> env {univRenameEnv = IntMap.empty})
--   e' <- univInst' e
--   l' <- levelInst l
--   up <- gets univRenameEnv
--   return (up, e', l')
-- univInstWith :: IntMap.IntMap UnivLevel -> WeakTermPlus -> WithEnv WeakTermPlus
-- univInstWith univParams e = do
--   modify (\env -> env {univRenameEnv = univParams})
--   univInst' e
-- univInst' :: WeakTermPlus -> WithEnv WeakTermPlus
-- univInst' (m, WeakTermTau l) = do
--   l' <- levelInst l
--   return (m, WeakTermTau l')
-- univInst' (m, WeakTermUpsilon x) = return (m, WeakTermUpsilon x)
-- univInst' (m, WeakTermPi mName xts t) = do
--   xts' <- univInstArgs xts
--   t' <- univInst' t
--   return (m, WeakTermPi mName xts' t')
-- univInst' (m, WeakTermPiIntro xts e) = do
--   xts' <- univInstArgs xts
--   e' <- univInst' e
--   return (m, WeakTermPiIntro xts' e')
-- univInst' (m, WeakTermPiIntroNoReduce xts e) = do
--   xts' <- univInstArgs xts
--   e' <- univInst' e
--   return (m, WeakTermPiIntroNoReduce xts' e')
-- univInst' (m, WeakTermPiIntroPlus ind (name, is, args1, args2) xts e) = do
--   args1' <- univInstArgs args1
--   args2' <- univInstArgs args2
--   xts' <- univInstArgs xts
--   e' <- univInst' e
--   return (m, WeakTermPiIntroPlus ind (name, is, args1', args2') xts' e')
-- univInst' (m, WeakTermPiElim e es) = do
--   e' <- univInst' e
--   es' <- mapM univInst' es
--   return (m, WeakTermPiElim e' es')
-- univInst' (m, WeakTermIter (mx, x, t) xts e) = do
--   t' <- univInst' t
--   xts' <- univInstArgs xts
--   e' <- univInst' e
--   return (m, WeakTermIter (mx, x, t') xts' e')
-- univInst' (m, WeakTermConst x up) = do
--   up' <- mapM levelInst up
--   return (m, WeakTermConst x up')
-- univInst' (m, WeakTermZeta x) = return (m, WeakTermZeta x)
-- univInst' (m, WeakTermInt t a) = do
--   t' <- univInst' t
--   return (m, WeakTermInt t' a)
-- univInst' (m, WeakTermFloat16 a) = return (m, WeakTermFloat16 a)
-- univInst' (m, WeakTermFloat32 a) = return (m, WeakTermFloat32 a)
-- univInst' (m, WeakTermFloat64 a) = return (m, WeakTermFloat64 a)
-- univInst' (m, WeakTermFloat t a) = do
--   t' <- univInst' t
--   return (m, WeakTermFloat t' a)
-- univInst' (m, WeakTermEnum x) = return (m, WeakTermEnum x)
-- univInst' (m, WeakTermEnumIntro l) = return (m, WeakTermEnumIntro l)
-- univInst' (m, WeakTermEnumElim (e, t) les) = do
--   t' <- univInst' t
--   e' <- univInst' e
--   let (ls, es) = unzip les
--   es' <- mapM univInst' es
--   return (m, WeakTermEnumElim (e', t') (zip ls es'))
-- univInst' (m, WeakTermArray dom k) = do
--   dom' <- univInst' dom
--   return (m, WeakTermArray dom' k)
-- univInst' (m, WeakTermArrayIntro k es) = do
--   es' <- mapM univInst' es
--   return (m, WeakTermArrayIntro k es')
-- univInst' (m, WeakTermArrayElim k xts d e) = do
--   xts' <- univInstArgs xts
--   d' <- univInst' d
--   e' <- univInst' e
--   return (m, WeakTermArrayElim k xts' d' e')
-- univInst' (m, WeakTermStruct ks) = return (m, WeakTermStruct ks)
-- univInst' (m, WeakTermStructIntro ets) = do
--   let (es, ks) = unzip ets
--   es' <- mapM univInst' es
--   return (m, WeakTermStructIntro (zip es' ks))
-- univInst' (m, WeakTermStructElim xts d e) = do
--   d' <- univInst' d
--   e' <- univInst' e
--   return (m, WeakTermStructElim xts d' e')
-- univInst' (m, WeakTermCase indName e cxtes) = do
--   e' <- univInst' e
--   cxtes' <-
--     flip mapM cxtes $ \((c, xts), body) -> do
--       xts' <- univInstArgs xts
--       body' <- univInst' body
--       return ((c, xts'), body')
--   return (m, WeakTermCase indName e' cxtes')
-- univInstArgs :: [IdentifierPlus] -> WithEnv [IdentifierPlus]
-- univInstArgs xts = do
--   let (ms, xs, ts) = unzip3 xts
--   ts' <- mapM univInst' ts
--   return $ zip3 ms xs ts'
-- levelInst :: UnivLevel -> WithEnv UnivLevel
-- levelInst l = do
--   urenv <- gets univRenameEnv
--   case IntMap.lookup l urenv of
--     Just l' -> return l'
--     Nothing -> do
--       l' <- newCount
--       modify (\env -> env {univRenameEnv = IntMap.insert l l' urenv})
--       uienv <- gets univInstEnv
--       let s = S.fromList [l, l']
--       modify (\env -> env {univInstEnv = IntMap.insertWith S.union l s uienv})
--       return l'
