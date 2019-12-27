module Elaborate.Infer
  ( infer
  , readWeakMetaType
  , typeOf
  , univ
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (catMaybes)
import Prelude hiding (pi)

import Data.Basic
import Data.Env
import Data.PreTerm
import Data.WeakTerm

type Context = [(Identifier, PreTermPlus)]

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
-- infer e ~> {type-annotated e}
infer :: Context -> WeakTermPlus -> WithEnv PreTermPlus
infer _ (m, WeakTermTau) = return (PreMetaTerminal (toLoc m), PreTermTau)
infer _ (m, WeakTermTheta x)
  | Just i <- asEnumNatNumConstant x = do
    t <- toIsEnumType $ fromInteger i
    retPreTerm t (toLoc m) $ PreTermTheta x
  | otherwise = do
    mt <- lookupTypeEnvMaybe x
    case mt of
      Just t -> retPreTerm t (toLoc m) $ PreTermTheta x
      Nothing -> do
        h <- newHoleInCtx []
        insTypeEnv x h
        retPreTerm h (toLoc m) $ PreTermTheta x
infer _ (m, WeakTermUpsilon x) = do
  t <- lookupTypeEnv x
  retPreTerm t (toLoc m) $ PreTermUpsilon x
infer ctx (m, WeakTermPi xts t) = do
  (xts', t') <- inferPi ctx xts t
  retPreTerm univ (toLoc m) $ PreTermPi xts' t'
infer ctx (m, WeakTermPiIntro xts e) = do
  (xts', e') <- inferPiIntro ctx xts e
  let piType = (newMetaTerminal, PreTermPi xts' (typeOf e'))
  retPreTerm piType (toLoc m) $ PreTermPiIntro xts' e'
infer ctx (m, WeakTermPiElim e es) = do
  e' <- infer ctx e
  -- -- xts == [(x1, e1, t1), ..., (xn, en, tn)] with xi : ti and ei : ti
  (xs, es', ts) <- unzip3 <$> inferList ctx es
  let xts = zip xs ts
  -- cod = ?M @ ctx @ (x1, ..., xn)
  cod <- newHoleInCtx (ctx ++ xts)
  let tPi = (newMetaTerminal, PreTermPi xts cod)
  insConstraintEnv tPi (typeOf e')
  -- cod' = ?M @ ctx @ (e1, ..., en)
  let cod' = substPreTermPlus (zip xs es') cod
  retPreTerm cod' (toLoc m) $ PreTermPiElim e' es'
infer ctx (m, WeakTermMu (x, t) e) = do
  t' <- inferType ctx t
  insTypeEnv x t'
  e' <- infer (ctx ++ [(x, t')]) e
  insConstraintEnv t' (typeOf e')
  retPreTerm t' (toLoc m) $ PreTermMu (x, t') e'
infer ctx (m, WeakTermZeta x) = do
  mt <- lookupTypeEnvMaybe x
  case mt of
    Just t -> retPreTerm t (toLoc m) $ PreTermZeta x
    Nothing -> do
      h <- newHoleInCtx ctx
      insTypeEnv x h
      retPreTerm h (toLoc m) $ PreTermZeta x
infer _ (m, WeakTermIntS size i) = do
  let t = (newMetaTerminal, PreTermTheta $ "i" ++ show size)
  retPreTerm t (toLoc m) $ PreTermIntS size i
infer _ (m, WeakTermIntU size i) = do
  let t = (newMetaTerminal, PreTermTheta $ "u" ++ show size)
  retPreTerm t (toLoc m) $ PreTermIntU size i
infer _ (m, WeakTermInt i) = do
  h <- newHoleInCtx []
  retPreTerm h (toLoc m) $ PreTermInt i
infer _ (m, WeakTermFloat16 f) = do
  let t = (newMetaTerminal, PreTermTheta "f16")
  retPreTerm t (toLoc m) $ PreTermFloat16 f
infer _ (m, WeakTermFloat32 f) = do
  let t = (newMetaTerminal, PreTermTheta "f32")
  retPreTerm t (toLoc m) $ PreTermFloat32 f
infer _ (m, WeakTermFloat64 f) = do
  let t = (newMetaTerminal, PreTermTheta "f64")
  retPreTerm t (toLoc m) $ PreTermFloat64 f
infer _ (m, WeakTermFloat f) = do
  h <- newHoleInCtx []
  retPreTerm h (toLoc m) $ PreTermFloat f
infer _ (m, WeakTermEnum name) = retPreTerm univ (toLoc m) $ PreTermEnum name
infer _ (m, WeakTermEnumIntro labelOrNum) = do
  case labelOrNum of
    EnumValueLabel l -> do
      k <- lookupKind l
      let t = (newMetaTerminal, PreTermEnum $ EnumTypeLabel k)
      retPreTerm t (toLoc m) $ PreTermEnumIntro labelOrNum
    EnumValueNatNum i _ -> do
      let t = (newMetaTerminal, PreTermEnum $ EnumTypeNatNum i)
      retPreTerm t (toLoc m) $ PreTermEnumIntro labelOrNum
infer ctx (m, WeakTermEnumElim e les) = do
  e' <- infer ctx e
  if null les
    then do
      h <- newHoleInCtx ctx
      retPreTerm h (toLoc m) $ PreTermEnumElim e' [] -- ex falso quodlibet
    else do
      let (ls, es) = unzip les
      tls <- mapM inferCase ls
      constrainList $ (typeOf e') : catMaybes tls
      es' <- mapM (infer ctx) es
      constrainList $ map typeOf es'
      retPreTerm (typeOf $ head es') (toLoc m) $ PreTermEnumElim e' $ zip ls es'
infer ctx (m, WeakTermArray k dom cod) = do
  dom' <- inferType ctx dom
  cod' <- inferType ctx cod
  retPreTerm univ (toLoc m) $ PreTermArray k dom' cod'
infer ctx (m, WeakTermArrayIntro k les) = do
  let tCod = inferKind k
  let (ls, es) = unzip les
  tls <- mapM (inferCase . CaseValue) ls
  constrainList $ catMaybes tls
  es' <- mapM (infer ctx) es
  let tDom = determineDomType es'
  let t = (newMetaTerminal, PreTermArray k tDom tCod)
  retPreTerm t (toLoc m) $ PreTermArrayIntro k $ zip ls es'
infer ctx (m, WeakTermArrayElim kind e1 e2) = do
  let tCod = inferKind kind
  e1' <- infer ctx e1
  e2' <- infer ctx e2
  let tArr = typeOf e1'
  insConstraintEnv tArr (newMetaTerminal, PreTermArray kind (typeOf e2') tCod)
  retPreTerm tCod (toLoc m) $ PreTermArrayElim kind e1' e2'

inferType :: Context -> WeakTermPlus -> WithEnv PreTermPlus
inferType ctx t = do
  t' <- infer ctx t
  insConstraintEnv (typeOf t') univ
  return t'

inferKind :: ArrayKind -> PreTermPlus
inferKind (ArrayKindIntS i) = (newMetaTerminal, PreTermTheta $ "i" ++ show i)
inferKind (ArrayKindIntU i) = (newMetaTerminal, PreTermTheta $ "u" ++ show i)
inferKind (ArrayKindFloat size) =
  (newMetaTerminal, PreTermTheta $ "f" ++ show (sizeAsInt size))

-- inferPiIntro ::
--      Context -> WeakMeta -> Context -> WeakTermPlus -> WithEnv WeakTermPlus
-- inferPiIntro ctx meta xts e = inferPiIntro' ctx meta xts xts e
-- inferPiIntro' ::
--      Context
--   -> WeakMeta
--   -> [(Identifier, WeakTermPlus)]
--   -> Context
--   -> WeakTermPlus
--   -> WithEnv WeakTermPlus
-- inferPiIntro' ctx meta [] zts e = do
--   cod <- infer ctx e
--   undefined
--   -- returnAfterUpdate meta (newMetaTerminal, WeakTermPi zts cod)
-- inferPiIntro' ctx meta ((x, t):xts) zts e = do
--   t' <- inferType ctx t
--   insTypeEnv x t'
--   inferPiIntro' (ctx ++ [(x, t')]) meta xts zts e
inferPi ::
     Context
  -> [(Identifier, WeakTermPlus)]
  -> WeakTermPlus
  -> WithEnv ([(Identifier, PreTermPlus)], PreTermPlus)
inferPi ctx [] cod = do
  cod' <- inferType ctx cod
  return ([], cod')
inferPi ctx ((x, t):xts) cod = do
  t' <- inferType ctx t
  insTypeEnv x t'
  (xts', cod') <- inferPi (ctx ++ [(x, t')]) xts cod
  return ((x, t') : xts', cod')

inferPiIntro ::
     Context
  -> [(Identifier, WeakTermPlus)]
  -> WeakTermPlus
  -> WithEnv ([(Identifier, PreTermPlus)], PreTermPlus)
inferPiIntro ctx [] cod = do
  cod' <- infer ctx cod
  return ([], cod')
inferPiIntro ctx ((x, t):xts) cod = do
  t' <- inferType ctx t
  insTypeEnv x t'
  (xts', cod') <- inferPiIntro (ctx ++ [(x, t')]) xts cod
  return ((x, t') : xts', cod')

newHoleInCtx :: Context -> WithEnv PreTermPlus
newHoleInCtx ctx = do
  higherHole <- newHoleOfType (newMetaTerminal, PreTermPi ctx univ)
  varSeq <- mapM (uncurry toVar) ctx
  let app = (newMetaTerminal, PreTermPiElim higherHole varSeq)
  hole <- newHoleOfType (newMetaTerminal, PreTermPi ctx app)
  return (PreMetaNonTerminal app Nothing, PreTermPiElim hole varSeq)
  -- wrapWithType app (WeakTermPiElim hole varSeq)
  -- higherHole <- newHoleOfType (newMetaTerminal, WeakTermPi ctx univ)
  -- varSeq <- mapM (uncurry toVar) ctx
  -- let app = (newMetaTerminal, WeakTermPiElim higherHole varSeq)
  -- hole <- newHoleOfType (newMetaTerminal, WeakTermPi ctx app)
  -- wrapWithType app (WeakTermPiElim hole varSeq)

inferCase :: Case -> WithEnv (Maybe PreTermPlus)
inferCase (CaseValue (EnumValueLabel name)) = do
  ienv <- gets enumEnv
  k <- lookupKind' name ienv
  return $ Just (newMetaTerminal, PreTermEnum $ EnumTypeLabel k)
inferCase (CaseValue (EnumValueNatNum i _)) =
  return $ Just (newMetaTerminal, PreTermEnum $ EnumTypeNatNum i)
inferCase _ = return Nothing

-- inferList ctx [e1, ..., en]
-- ~> [(x1, t1), ..., (xn, tn)] with xi : ti, ei : ti
inferList ::
     Context
  -> [WeakTermPlus]
  -> WithEnv [(Identifier, PreTermPlus, PreTermPlus)]
inferList _ [] = return []
inferList ctx (e:es) = do
  e' <- infer ctx e
  x <- newNameWith "hole"
  -- _ <- inferType ctx t
  insTypeEnv x (typeOf e')
  xets <- inferList (ctx ++ [(x, (typeOf e'))]) es
  return $ (x, e', typeOf e') : xets

constrainList :: [PreTermPlus] -> WithEnv ()
constrainList [] = return ()
constrainList [_] = return ()
constrainList (t1:t2:ts) = do
  insConstraintEnv t1 t2
  constrainList $ t2 : ts

toVar :: Identifier -> PreTermPlus -> WithEnv PreTermPlus
toVar x t = do
  insTypeEnv x t
  return (PreMetaNonTerminal t Nothing, PreTermUpsilon x)

retPreTerm :: PreTermPlus -> Maybe Loc -> PreTerm -> WithEnv PreTermPlus
retPreTerm t ml e = return (PreMetaNonTerminal t ml, e)

-- returnAfterUpdate :: WeakMeta -> WeakTermPlus -> WithEnv WeakTermPlus
-- returnAfterUpdate m t = do
--   typeOrRef <- readWeakMetaType m
--   case typeOrRef of
--     Just t' -> insConstraintEnv t t'
--     Left r -> writeWeakTermRef r t
--   return t
univ :: PreTermPlus
univ = (PreMetaTerminal Nothing, PreTermTau)

-- wrapWithType :: WeakTermPlus -> WeakTerm -> WithEnv WeakTermPlus
-- wrapWithType t e = do
--   m <- newMetaOfType t
--   return (m, e)
readWeakMetaType :: WeakMeta -> WithEnv (Maybe PreTermPlus)
readWeakMetaType (WeakMetaTerminal _) = return $ Just univ
readWeakMetaType (WeakMetaNonTerminal _) = return Nothing

-- readWeakMetaType (WeakMetaTerminal _) = return $ Right univ
-- readWeakMetaType (WeakMetaNonTerminal r@(WeakTermRef ref) _) = do
--   mt <- liftIO $ readIORef ref
--   -- ここで仮にjustだったとしても、nonTerminalのなかにnonterminal nothingが入っている可能性がある、ということ？
--   case mt of
--     Just t -> return $ Right t
--     Nothing -> return $ Left r
typeOf :: PreTermPlus -> PreTermPlus
typeOf (PreMetaTerminal _, _) = univ
typeOf (PreMetaNonTerminal t _, _) = t

-- is-enum n{i}
toIsEnumType :: Int -> WithEnv PreTermPlus
toIsEnumType i = undefined
  -- piType <- univToUniv
  -- piMeta <- newMetaOfType piType
  -- return
  --   ( newMetaTerminal
  --   , WeakTermPiElim
  --       (piMeta, WeakTermTheta "is-enum")
  --       [(newMetaTerminal, WeakTermEnum $ EnumTypeNatNum i)])

-- Univ -> Univ
univToUniv :: WithEnv PreTermPlus
univToUniv = undefined
  -- h <- newNameWith "hole"
  -- return (newMetaTerminal, WeakTermPi [(h, univ)] univ)

toLoc :: WeakMeta -> Maybe Loc
toLoc (WeakMetaTerminal ml) = ml
toLoc (WeakMetaNonTerminal ml) = ml

newMetaTerminal :: PreMeta
newMetaTerminal = PreMetaTerminal Nothing

newHoleOfType :: PreTermPlus -> WithEnv PreTermPlus
newHoleOfType t = do
  h <- newNameWith "hole"
  return (PreMetaNonTerminal t Nothing, PreTermZeta h)

determineDomType :: [PreTermPlus] -> PreTermPlus
determineDomType es =
  if null es
    then typeOf (head es)
    else (newMetaTerminal, PreTermTheta "bottom")
