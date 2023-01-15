module Scene.Parse.Discern
  ( discernStmtList,
    Context,
  )
where

import qualified Context.Alias as Alias
import qualified Context.CodataDefinition as CodataDefinition
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Throw as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.List
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Entity.Arity as A
import Entity.Binder
import qualified Entity.DecisionTree as DT
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.FilePos
import qualified Entity.GlobalName as GN
import Entity.Hint
import Entity.Ident
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import qualified Entity.LocalLocator as LL
import qualified Entity.Log as L
import qualified Entity.Magic as M
import qualified Entity.Noema as N
import Entity.NominalEnv
import qualified Entity.Pattern as PAT
import qualified Entity.Pattern.Fallback as PATF
import qualified Entity.Pattern.Specialize as PATS
import qualified Entity.RawPattern as RP
import qualified Entity.RawTerm as RT
import Entity.Stmt
import qualified Entity.UnresolvedName as UN
import qualified Entity.Vector as V
import qualified Entity.WeakPrim as WP
import qualified Entity.WeakPrimValue as WPV
import qualified Entity.WeakTerm as WT

class
  ( Throw.Context m,
    Gensym.Context m,
    Global.Context m,
    Locator.Context m,
    Alias.Context m,
    PATF.Context m,
    PATS.Context m,
    CodataDefinition.Context m
  ) =>
  Context m

discernStmtList :: Context m => [RawStmt] -> m [WeakStmt]
discernStmtList stmtList =
  case stmtList of
    [] ->
      return []
    RawStmtDefine stmtKind m functionName impArgNum xts codType e : rest -> do
      stmtKind' <- discernStmtKind stmtKind
      (xts', nenv) <- discernBinder empty xts
      codType' <- discern nenv codType
      e' <- discern nenv e
      rest' <- discernStmtList rest
      return $ WeakStmtDefine stmtKind' m functionName impArgNum xts' codType' e' : rest'
    RawStmtSection section innerStmtList : rest -> do
      Locator.withSection section $ do
        innerStmtList' <- discernStmtList innerStmtList
        rest' <- discernStmtList rest
        return $ innerStmtList' ++ rest'

discernStmtKind :: Context m => StmtKindF RT.RawTerm -> m (StmtKindF WT.WeakTerm)
discernStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      return $ Normal opacity
    Data dataName dataArgs consInfoList -> do
      (dataArgs', nenv) <- discernBinder empty dataArgs
      let (consNameList, consArgsList, discriminantList) = unzip3 consInfoList
      consArgsList' <- map fst <$> mapM (discernBinder nenv) consArgsList
      return $ Data dataName dataArgs' $ zip3 consNameList consArgsList' discriminantList
    DataIntro dataName dataArgs consArgs discriminant -> do
      (dataArgs', nenv) <- discernBinder empty dataArgs
      (consArgs', _) <- discernBinder nenv consArgs
      return $ DataIntro dataName dataArgs' consArgs' discriminant

-- Alpha-convert all the variables so that different variables have different names.
discern :: Context m => NominalEnv -> RT.RawTerm -> m WT.WeakTerm
discern nenv term =
  case term of
    m :< RT.Tau ->
      return $ m :< WT.Tau
    m :< RT.Var (I (s, _)) -> do
      case lookup s nenv of
        Just (_, name) ->
          return $ m :< WT.Var name
        Nothing -> do
          (dd, gn) <- resolveName m s
          interpretGlobalName m dd gn
    m :< RT.VarGlobal globalLocator localLocator -> do
      sgl <- Alias.resolveAlias m globalLocator
      let dd = DD.new sgl localLocator
      gn <- interpretDefiniteDescription m dd
      interpretGlobalName m dd gn
    m :< RT.Pi xts t -> do
      (xts', t') <- discernBinderWithBody nenv xts t
      return $ m :< WT.Pi xts' t'
    m :< RT.PiIntro kind xts e -> do
      case kind of
        LK.Fix xt -> do
          (xt', xts', e') <- discernBinderWithBody' nenv xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        LK.Normal opacity -> do
          (xts', e') <- discernBinderWithBody nenv xts e
          return $ m :< WT.PiIntro (LK.Normal opacity) xts' e'
    m :< RT.PiElim e es -> do
      es' <- mapM (discern nenv) es
      e' <- discern nenv e
      return $ m :< WT.PiElim e' es'
    m :< RT.Data name es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WT.Data name es'
    m :< RT.DataIntro dataName consName disc dataArgs consArgs -> do
      dataArgs' <- mapM (discern nenv) dataArgs
      consArgs' <- mapM (discern nenv) consArgs
      return $ m :< WT.DataIntro dataName consName disc dataArgs' consArgs'
    m :< RT.DataElim isNoetic es patternMatrix -> do
      os <- mapM (const $ Gensym.newIdentFromText "match") es -- os: occurrences
      es' <- mapM (discern nenv >=> castFromNoema' nenv isNoetic) es
      ts <- mapM (const $ Gensym.newHole m (asHoleArgs nenv)) es'
      patternMatrix' <- discernPatternMatrix nenv patternMatrix
      ensurePatternMatrixSanity patternMatrix'
      decisionTree <- compilePatternMatrix nenv isNoetic m (V.fromList os) patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es' ts) decisionTree
    m :< RT.Noema t -> do
      t' <- discern nenv t
      return $ m :< WT.Noema t'
    m :< RT.Let mxt mys e1 e2 -> do
      discernLet nenv m mxt mys e1 e2
    m :< RT.Prim prim -> do
      prim' <- mapM (discern nenv) prim
      return $ m :< WT.Prim prim'
    m :< RT.Hole k ->
      return $ m :< WT.Hole k (asHoleArgs nenv)
    m :< RT.Magic der -> do
      der' <- traverse (discern nenv) der
      return $ m :< WT.Magic der'
    m :< RT.New name kvs -> do
      dd <- fst <$> resolveName m name
      let (_, ks, vs) = unzip3 kvs
      ks' <- mapM (resolveField m) ks
      ensureFieldLinearity m ks' S.empty S.empty
      -- when (length ks /= length (nub ks)) $
      --   Throw.raiseError m "duplicate key"
      -- ks' <- mapM (resolveName m >=> return . fst) ks
      ((constructor, arity), keyList) <- CodataDefinition.lookup m dd
      vs' <- mapM (discern nenv) vs
      args <- reorderArgs m keyList $ Map.fromList $ zip ks' vs'
      return $ m :< WT.PiElim (m :< WT.VarGlobal constructor arity) args

ensureFieldLinearity ::
  Context m =>
  Hint ->
  [DD.DefiniteDescription] ->
  S.Set DD.DefiniteDescription ->
  S.Set DD.DefiniteDescription ->
  m ()
ensureFieldLinearity m ks found nonLinear =
  case ks of
    [] ->
      if S.null nonLinear
        then return ()
        else
          Throw.raiseError m $
            "the following fields are defined more than once:\n"
              <> T.intercalate "\n" (map (\k -> "- " <> DD.reify k) (S.toList nonLinear))
    k : rest -> do
      if S.member k found
        then ensureFieldLinearity m rest found (S.insert k nonLinear)
        else ensureFieldLinearity m rest (S.insert k found) nonLinear

resolveField :: Context m => Hint -> T.Text -> m DD.DefiniteDescription
resolveField m name = do
  localLocator <- LL.reflect m name
  candList <- Locator.getPossibleReferents localLocator
  candList' <- mapM Global.lookup candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      Throw.raiseError m $ "undefined field: " <> name
    [(field, _)] ->
      return field
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . DD.reify . fst) foundNameList
      Throw.raiseError m $
        "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

reorderArgs :: Context m => Hint -> [DD.DefiniteDescription] -> Map.HashMap DD.DefiniteDescription a -> m [a]
reorderArgs m keyList kvs =
  case keyList of
    []
      | Map.null kvs ->
          return []
      | otherwise -> do
          let ks = map fst $ Map.toList kvs
          Throw.raiseError m $ "the following fields are redundant: " <> showKeyList ks
    key : keyRest
      | Just v <- Map.lookup key kvs -> do
          vs <- reorderArgs m keyRest (Map.delete key kvs)
          return $ v : vs
      | otherwise ->
          Throw.raiseError m $ "the field `" <> DD.reify key <> "` is missing"

showKeyList :: [DD.DefiniteDescription] -> T.Text
showKeyList ks =
  T.intercalate "\n" $ map (\k -> "- " <> DD.reify k) ks

discernLet ::
  Context m =>
  NominalEnv ->
  Hint ->
  BinderF RT.RawTerm ->
  [(Hint, Ident)] ->
  RT.RawTerm ->
  RT.RawTerm ->
  m WT.WeakTerm
discernLet nenv m mxt mys e1 e2 = do
  let (ms, ys) = unzip mys
  ysActual <- zipWithM (\my y -> discern nenv (my :< RT.Var y)) ms ys
  ysLocal <- mapM Gensym.newIdentFromIdent ys
  ysCont <- mapM Gensym.newIdentFromIdent ys
  let nenvLocal = zipWith (\my yLocal -> (Ident.toText yLocal, (my, yLocal))) ms ysLocal ++ nenv
  let nenvCont = zipWith (\my yCont -> (Ident.toText yCont, (my, yCont))) ms ysCont ++ nenv
  e1' <- discern nenvLocal e1
  (mxt', _, e2') <- discernBinderWithBody' nenvCont mxt [] e2
  e2'' <- attachSuffix nenv (zip ysCont ysLocal) e2'
  let opacity = if null mys then WT.Transparent else WT.Noetic
  attachPrefix nenv (zip ysLocal ysActual) $ m :< WT.Let opacity mxt' e1' e2''

attachPrefix :: Context m => NominalEnv -> [(Ident, WT.WeakTerm)] -> WT.WeakTerm -> m WT.WeakTerm
attachPrefix nenv binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (y, e) : rest -> do
      e' <- castToNoema nenv e
      cont' <- attachPrefix nenv rest cont
      h <- Gensym.newHole m (asHoleArgs nenv)
      return $ m :< WT.Let WT.Opaque (m, y, h) e' cont'

attachSuffix :: Context m => NominalEnv -> [(Ident, Ident)] -> WT.WeakTerm -> m WT.WeakTerm
attachSuffix nenv binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (yCont, yLocal) : rest -> do
      yLocal' <- castFromNoema nenv (m :< WT.Var yLocal)
      cont' <- attachSuffix nenv rest cont
      h <- Gensym.newHole m (asHoleArgs nenv)
      return $ m :< WT.Let WT.Opaque (m, yCont, h) yLocal' cont'

castToNoema :: Context m => NominalEnv -> WT.WeakTerm -> m WT.WeakTerm
castToNoema nenv e@(m :< _) = do
  t <- Gensym.newHole m (asHoleArgs nenv)
  let tNoema = m :< WT.Noema t
  return $ m :< WT.Magic (M.Cast t tNoema e)

castFromNoema :: Context m => NominalEnv -> WT.WeakTerm -> m WT.WeakTerm
castFromNoema nenv e@(m :< _) = do
  t <- Gensym.newHole m (asHoleArgs nenv)
  let tNoema = m :< WT.Noema t
  return $ m :< WT.Magic (M.Cast tNoema t e)

castToNoema' :: Context m => NominalEnv -> N.IsNoetic -> WT.WeakTerm -> m WT.WeakTerm
castToNoema' nenv isNoetic e =
  if isNoetic
    then castToNoema nenv e
    else return e

castFromNoema' :: Context m => NominalEnv -> N.IsNoetic -> WT.WeakTerm -> m WT.WeakTerm
castFromNoema' nenv isNoetic e =
  if isNoetic
    then castFromNoema nenv e
    else return e

discernBinder ::
  Context m =>
  NominalEnv ->
  [BinderF RT.RawTerm] ->
  m ([BinderF WT.WeakTerm], NominalEnv)
discernBinder nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x, t) : xts -> do
      t' <- discern nenv t
      x' <- Gensym.newIdentFromIdent x
      (xts', nenv') <- discernBinder ((Ident.toText x, (mx, x')) : nenv) xts
      return ((mx, x', t') : xts', nenv')

discernBinderWithBody ::
  Context m =>
  NominalEnv ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody nenv binder e = do
  (binder', nenv') <- discernBinder nenv binder
  e' <- discern nenv' e
  return (binder', e')

discernBinderWithBody' ::
  Context m =>
  NominalEnv ->
  BinderF RT.RawTerm ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm ->
  m (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody' nenv (mx, x, t) binder e = do
  t' <- discern nenv t
  x' <- Gensym.newIdentFromIdent x
  (binder', e') <- discernBinderWithBody ((Ident.toText x, (mx, x')) : nenv) binder e
  return ((mx, x', t'), binder', e')

resolveName :: Context m => Hint -> T.Text -> m (DD.DefiniteDescription, GN.GlobalName)
resolveName m name = do
  localLocator <- LL.reflect m name
  candList <- Locator.getPossibleReferents localLocator
  candList' <- mapM Global.lookup candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      Throw.raiseError m $ "undefined variable: " <> name
    [pair] ->
      return pair
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . DD.reify . fst) foundNameList
      Throw.raiseError m $
        "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

interpretGlobalName :: Context m => Hint -> DD.DefiniteDescription -> GN.GlobalName -> m WT.WeakTerm
interpretGlobalName m dd gn =
  case gn of
    GN.TopLevelFunc arity ->
      return $ m :< WT.VarGlobal dd arity
    GN.Data arity _ ->
      return $ m :< WT.VarGlobal dd arity
    GN.DataIntro dataArity consArity _ ->
      return $ m :< WT.VarGlobal dd (A.fromInt $ fromInteger (A.reify dataArity + A.reify consArity))
    GN.PrimType primNum ->
      return $ m :< WT.Prim (WP.Type primNum)
    GN.PrimOp primOp ->
      return $ m :< WT.Prim (WP.Value (WPV.Op primOp))

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo

interpretDefiniteDescription :: Context m => Hint -> DD.DefiniteDescription -> m GN.GlobalName
interpretDefiniteDescription m dd = do
  mgn <- Global.lookup dd
  case mgn of
    Just gn ->
      return gn
    Nothing ->
      Throw.raiseError m $ "undefined constant: " <> DD.reify dd

resolveConstructor ::
  Context m =>
  Hint ->
  RP.RawConsName ->
  m (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant)
resolveConstructor m cons = do
  case cons of
    RP.UnresolvedName (UN.UnresolvedName consName') -> do
      (dd, gn) <- resolveName m consName'
      resolveConstructor' m dd gn
    RP.LocatorPair globalLocator localLocator -> do
      sgl <- Alias.resolveAlias m globalLocator
      let dd = DD.new sgl localLocator
      gn <- interpretDefiniteDescription m dd
      resolveConstructor' m dd gn
    RP.DefiniteDescription dd -> do
      gn <- interpretDefiniteDescription m dd
      resolveConstructor' m dd gn

resolveConstructor' ::
  Context m =>
  Hint ->
  DD.DefiniteDescription ->
  GN.GlobalName ->
  m (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant)
resolveConstructor' m dd gn =
  case gn of
    GN.DataIntro dataArity consArity disc ->
      return (dd, dataArity, consArity, disc)
    _ ->
      Throw.raiseError m $ DD.reify dd <> " is not a constructor"

discernPatternMatrix ::
  Context m =>
  NominalEnv ->
  RP.RawPatternMatrix RT.RawTerm ->
  m (PAT.PatternMatrix ([Ident], WT.WeakTerm))
discernPatternMatrix nenv patternMatrix =
  case RP.unconsRow patternMatrix of
    Nothing ->
      return $ PAT.new []
    Just (row, rows) -> do
      row' <- discernPatternRow nenv row
      rows' <- discernPatternMatrix nenv rows
      return $ PAT.consRow row' rows'

discernPatternRow ::
  Context m =>
  NominalEnv ->
  RP.RawPatternRow RT.RawTerm ->
  m (PAT.PatternRow ([Ident], WT.WeakTerm))
discernPatternRow nenv (patList, body) = do
  let vars = RP.rowVars patList
  ensurePatternVariableLinearity vars
  vars' <- mapM (Gensym.newIdentFromIdent . snd) vars
  let nenv' = zipWith (\(mv, var) var' -> (Ident.toText var, (mv, var'))) vars vars'
  patList' <- mapM (discernPattern nenv') patList
  body' <- discern (nenv' ++ nenv) body
  return (patList', ([], body'))

ensurePatternVariableLinearity :: Context m => [(Hint, Ident)] -> m ()
ensurePatternVariableLinearity vars = do
  let linearityErrors = getNonLinearOccurrences vars S.empty []
  unless (null linearityErrors) $ Throw.throw $ L.MakeError linearityErrors

getNonLinearOccurrences :: [(Hint, Ident)] -> S.Set T.Text -> [(Hint, T.Text)] -> [L.Log]
getNonLinearOccurrences vars found nonLinear =
  case vars of
    [] -> do
      let nonLinearVars = reverse $ nubBy (\x y -> snd x == snd y) nonLinear
      flip map nonLinearVars $ \(m, x) ->
        L.logError (fromHint m) $
          "the pattern variable `"
            <> x
            <> "` is used non-linearly"
    (m, v) : rest
      | S.member (Ident.toText v) found ->
          getNonLinearOccurrences rest found ((m, Ident.toText v) : nonLinear)
      | otherwise ->
          getNonLinearOccurrences rest (S.insert (Ident.toText v) found) nonLinear

discernPattern ::
  Context m =>
  NominalEnv ->
  (Hint, RP.RawPattern) ->
  m (Hint, PAT.Pattern)
discernPattern nenv (m, pat) =
  case pat of
    RP.Var x ->
      case lookup (Ident.toText x) nenv of
        Nothing ->
          return (m, PAT.Var x)
        Just (_, x') ->
          return (m, PAT.Var x')
    RP.Cons cons args -> do
      (consName, dataArity, consArity, disc) <- resolveConstructor m cons
      args' <- mapM (discernPattern nenv) args
      return (m, PAT.Cons consName disc dataArity consArity args')

ensurePatternMatrixSanity :: Context m => PAT.PatternMatrix a -> m ()
ensurePatternMatrixSanity mat =
  case PAT.unconsRow mat of
    Nothing ->
      return ()
    Just (row, rest) -> do
      ensurePatternRowSanity row
      ensurePatternMatrixSanity rest

ensurePatternRowSanity :: Context m => PAT.PatternRow a -> m ()
ensurePatternRowSanity (patternVector, _) = do
  mapM_ ensurePatternSanity $ V.toList patternVector

ensurePatternSanity :: Context m => (Hint, PAT.Pattern) -> m ()
ensurePatternSanity (m, pat) =
  case pat of
    PAT.Var {} ->
      return ()
    PAT.WildcardVar {} ->
      return ()
    PAT.Cons cons _ _ consArity args -> do
      let argNum = length args
      when (argNum /= fromInteger (A.reify consArity)) $
        Throw.raiseError m $
          "the constructor `"
            <> DD.reify cons
            <> "` expects "
            <> T.pack (show (A.reify consArity))
            <> " arguments, but found "
            <> T.pack (show argNum)
            <> "."

-- This translation is based on:
--   https://dl.acm.org/doi/10.1145/1411304.1411311
compilePatternMatrix ::
  Context m =>
  NominalEnv ->
  N.IsNoetic ->
  Hint ->
  V.Vector Ident ->
  PAT.PatternMatrix ([Ident], WT.WeakTerm) ->
  m (DT.DecisionTree WT.WeakTerm)
compilePatternMatrix nenv isNoetic m occurrences mat =
  case PAT.unconsRow mat of
    Nothing ->
      return DT.Unreachable
    Just (row, _) ->
      case PAT.getClauseBody row of
        Right (usedVars, (freedVars, body)) -> do
          cursorVars <- mapM (alignLetBody nenv isNoetic m) (V.toList occurrences)
          DT.Leaf freedVars <$> bindLet nenv m (zip usedVars cursorVars) body
        Left i ->
          if i > 0
            then do
              occurrences' <- V.swap m i occurrences
              mat' <- PAT.swapColumn m i mat
              compilePatternMatrix nenv isNoetic m occurrences' mat'
            else do
              let headConstructors = PAT.getHeadConstructors mat
              let cursor = V.head occurrences
              clauseList <- forM headConstructors $ \(cons, disc, dataArity, consArity, _) -> do
                dataHoles <- mapM (const $ Gensym.newHole m (asHoleArgs nenv)) [1 .. A.reify dataArity]
                dataTypeHoles <- mapM (const $ Gensym.newHole m (asHoleArgs nenv)) [1 .. A.reify dataArity]
                consVars <- mapM (const $ Gensym.newIdentFromText "cvar") [1 .. A.reify consArity]
                (consArgs', nenv') <- alignConsArgs nenv $ map (m,) consVars
                let occurrences' = V.fromList consVars <> V.tail occurrences
                specialMatrix <- PATS.specialize nenv cursor (cons, consArity) mat
                specialDecisionTree <- compilePatternMatrix nenv' isNoetic m occurrences' specialMatrix
                return (DT.Cons cons disc (zip dataHoles dataTypeHoles) consArgs' specialDecisionTree)
              fallbackMatrix <- PATF.getFallbackMatrix nenv cursor mat
              fallbackClause <- compilePatternMatrix nenv isNoetic m (V.tail occurrences) fallbackMatrix
              t <- Gensym.newHole m (asHoleArgs nenv)
              return $ DT.Switch (cursor, t) (fallbackClause, clauseList)

alignLetBody :: Context m => NominalEnv -> N.IsNoetic -> Hint -> Ident -> m WT.WeakTerm
alignLetBody nenv isNoetic m x =
  castToNoema' nenv isNoetic $ m :< WT.Var x

alignConsArgs ::
  Context m =>
  NominalEnv ->
  [(Hint, Ident)] ->
  m ([BinderF WT.WeakTerm], NominalEnv)
alignConsArgs nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x) : xts -> do
      t <- Gensym.newPreHole mx
      t' <- discern nenv t
      (xts', nenv') <- alignConsArgs ((Ident.toText x, (mx, x)) : nenv) xts
      return ((mx, x, t') : xts', nenv')

bindLet ::
  Context m =>
  NominalEnv ->
  Hint ->
  [(Maybe Ident, WT.WeakTerm)] ->
  WT.WeakTerm ->
  m WT.WeakTerm
bindLet nenv m binder cont =
  case binder of
    [] ->
      return cont
    (Nothing, _) : xes -> do
      bindLet nenv m xes cont
    (Just from, to) : xes -> do
      h <- Gensym.newHole m (asHoleArgs nenv)
      cont' <- bindLet nenv m xes cont
      return $ m :< WT.Let WT.Transparent (m, from, h) to cont'

-- bindLet ::
--   Context m =>
--   NominalEnv ->
--   Hint ->
--   [(Maybe Ident, Ident)] ->
--   WT.WeakTerm ->
--   m WT.WeakTerm
-- bindLet nenv m binder cont =
--   case binder of
--     [] ->
--       return cont
--     (Nothing, _) : xes -> do
--       bindLet nenv m xes cont
--     (Just from, to) : xes -> do
--       h <- Gensym.newHole m (asHoleArgs nenv)
--       cont' <- bindLet nenv m xes cont
--       return $ m :< WT.Let O.Transparent (m, from, h) (m :< WT.Var to) cont'
