module Scene.Parse.Discern (discernStmtList) where

import Context.Alias qualified as Alias
import Context.App
import Context.CodataDefinition qualified as CodataDefinition
import Context.Gensym qualified as Gensym
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Throw qualified as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.List
import Data.Maybe qualified as Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Arity qualified as A
import Entity.Binder
import Entity.Const qualified as C
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.FilePos
import Entity.GlobalLocator qualified as GL
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.LocalLocator qualified as LL
import Entity.Log qualified as L
import Entity.Magic qualified as M
import Entity.Mutability
import Entity.Noema qualified as N
import Entity.NominalEnv
import Entity.Pattern qualified as PAT
import Entity.PrimNumSize qualified as PNS
import Entity.PrimOp qualified as PO
import Entity.PrimType qualified as PT
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Stmt
import Entity.UnresolvedName qualified as UN
import Entity.Vector qualified as V
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Fallback qualified as PATF
import Scene.Parse.Discern.Noema
import Scene.Parse.Discern.Specialize qualified as PATS

discernStmtList :: [RawStmt] -> App [WeakStmt]
discernStmtList stmtList =
  case stmtList of
    [] ->
      return []
    RawStmtDefine isConstLike stmtKind m functionName impArgNum xts codType e : rest -> do
      (xts', nenv) <- discernBinder empty xts
      codType' <- discern nenv codType
      stmtKind' <- discernStmtKind stmtKind
      e' <- discern nenv e
      rest' <- discernStmtList rest
      return $ WeakStmtDefine isConstLike stmtKind' m functionName impArgNum xts' codType' e' : rest'
    RawStmtDefineResource m name discarder copier : rest -> do
      discarder' <- discern empty discarder
      copier' <- discern empty copier
      rest' <- discernStmtList rest
      return $ WeakStmtDefineResource m name discarder' copier' : rest'

discernStmtKind :: StmtKindF RT.RawTerm -> App (StmtKindF WT.WeakTerm)
discernStmtKind stmtKind =
  case stmtKind of
    Normal opacity ->
      return $ Normal opacity
    Data dataName dataArgs consInfoList -> do
      (dataArgs', nenv) <- discernBinder empty dataArgs
      let (consNameList, isConstLikeList, consArgsList, discriminantList) = unzip4 consInfoList
      consArgsList' <- map fst <$> mapM (discernBinder nenv) consArgsList
      return $ Data dataName dataArgs' $ zip4 consNameList isConstLikeList consArgsList' discriminantList
    DataIntro dataName dataArgs consArgs discriminant -> do
      (dataArgs', nenv) <- discernBinder empty dataArgs
      (consArgs', _) <- discernBinder nenv consArgs
      return $ DataIntro dataName dataArgs' consArgs' discriminant

discern :: NominalEnv -> RT.RawTerm -> App WT.WeakTerm
discern nenv term =
  case term of
    m :< RT.Tau ->
      return $ m :< WT.Tau
    m :< RT.Var (I (s, _)) -> do
      case lookup s nenv of
        Just (_, name) ->
          return $ m :< WT.Var name
        Nothing -> do
          resolveName m s >>= uncurry (interpretGlobalName m)
    m :< RT.VarGlobal globalLocator localLocator -> do
      discernGlobal m globalLocator localLocator
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
      es' <- mapM (discern nenv >=> castFromNoemaIfNecessary nenv isNoetic) es
      ts <- mapM (const $ Gensym.newHole m (asHoleArgs nenv)) es'
      patternMatrix' <- discernPatternMatrix nenv patternMatrix
      ensurePatternMatrixSanity patternMatrix'
      decisionTree <- compilePatternMatrix nenv isNoetic m (V.fromList os) patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es' ts) decisionTree
    m :< RT.Noema t -> do
      t' <- discern nenv t
      return $ m :< WT.Noema t'
    m :< RT.Embody e -> do
      e' <- discern nenv e
      let doNotCare = m :< WT.Tau -- discarded at Infer
      return $ m :< WT.Embody doNotCare e'
    m :< RT.Cell t -> do
      t' <- discern nenv t
      return $ m :< WT.Cell t'
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
      ((constructor, numOfDataArgs, numOfFields), keyList) <- CodataDefinition.lookup m dd
      vs' <- mapM (discern nenv) vs
      args <- reorderArgs m keyList $ Map.fromList $ zip ks' vs'
      return $ m :< WT.PiElim (m :< WT.VarGlobal constructor (A.add numOfDataArgs numOfFields)) args

ensureFieldLinearity ::
  Hint ->
  [DD.DefiniteDescription] ->
  S.Set DD.DefiniteDescription ->
  S.Set DD.DefiniteDescription ->
  App ()
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

resolveField :: Hint -> T.Text -> App DD.DefiniteDescription
resolveField m name = do
  localLocator <- Throw.liftEither $ LL.reflect m name
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

reorderArgs :: Hint -> [DD.DefiniteDescription] -> Map.HashMap DD.DefiniteDescription a -> App [a]
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
  NominalEnv ->
  Hint ->
  BinderF RT.RawTerm ->
  [(Mutability, Hint, Ident)] ->
  RT.RawTerm ->
  RT.RawTerm ->
  App WT.WeakTerm
discernLet nenv m mxt mys e1 e2 = do
  let (mutabilityList, ms, ys) = unzip3 mys
  ysActual <- zipWithM (\my y -> discern nenv (my :< RT.Var y)) ms ys
  ysLocal <- mapM Gensym.newIdentFromIdent ys
  ysCont <- mapM Gensym.newIdentFromIdent ys
  let nenvLocal = zipWith (\my yLocal -> (Ident.toText yLocal, (my, yLocal))) ms ysLocal ++ nenv
  let nenvCont = zipWith (\my yCont -> (Ident.toText yCont, (my, yCont))) ms ysCont ++ nenv
  e1' <- discern nenvLocal e1
  (mxt', _, e2') <- discernBinderWithBody' nenvCont mxt [] e2
  e2'' <- attachSuffix nenv (zip3 mutabilityList ysCont ysLocal) e2'
  let opacity = if null mys then WT.Transparent else WT.Noetic
  attachPrefix nenv (zip3 mutabilityList ysLocal ysActual) (m :< WT.Let opacity mxt' e1' e2'')

attachPrefix :: NominalEnv -> [(Mutability, Ident, WT.WeakTerm)] -> WT.WeakTerm -> App WT.WeakTerm
attachPrefix nenv binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (mutability, y, e) : rest -> do
      e' <- castToNoema nenv mutability e
      cont' <- attachPrefix nenv rest cont
      h <- Gensym.newHole m (asHoleArgs nenv)
      return $ m :< WT.Let WT.Opaque (m, y, h) e' cont'

attachSuffix :: NominalEnv -> [(Mutability, Ident, Ident)] -> WT.WeakTerm -> App WT.WeakTerm
attachSuffix nenv binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (mutability, yCont, yLocal) : rest -> do
      yLocal' <- castFromNoema nenv mutability (m :< WT.Var yLocal)
      cont' <- attachSuffix nenv rest cont
      h <- Gensym.newHole m (asHoleArgs nenv)
      return $ m :< WT.Let WT.Opaque (m, yCont, h) yLocal' cont'

discernBinder ::
  NominalEnv ->
  [BinderF RT.RawTerm] ->
  App ([BinderF WT.WeakTerm], NominalEnv)
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
  NominalEnv ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody nenv binder e = do
  (binder', nenv') <- discernBinder nenv binder
  e' <- discern nenv' e
  return (binder', e')

discernBinderWithBody' ::
  NominalEnv ->
  BinderF RT.RawTerm ->
  [BinderF RT.RawTerm] ->
  RT.RawTerm ->
  App (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody' nenv (mx, x, t) binder e = do
  t' <- discern nenv t
  x' <- Gensym.newIdentFromIdent x
  (binder', e') <- discernBinderWithBody ((Ident.toText x, (mx, x')) : nenv) binder e
  return ((mx, x', t'), binder', e')

resolveName :: Hint -> T.Text -> App (DD.DefiniteDescription, GN.GlobalName)
resolveName m name = do
  nameOrErr <- resolveNameOrErr m name
  case nameOrErr of
    Left err ->
      Throw.raiseError m err
    Right pair ->
      return pair

resolveNameOrErr :: Hint -> T.Text -> App (Either T.Text (DD.DefiniteDescription, GN.GlobalName))
resolveNameOrErr m name = do
  localLocator <- Throw.liftEither $ LL.reflect m name
  candList <- Locator.getPossibleReferents localLocator
  candList' <- mapM Global.lookup candList
  let foundNameList = Maybe.mapMaybe candFilter $ zip candList candList'
  case foundNameList of
    [] ->
      return $ Left $ "undefined variable: " <> name
    [pair] ->
      return $ Right pair
    _ -> do
      let candInfo = T.concat $ map (("\n- " <>) . DD.reify . fst) foundNameList
      return $ Left $ "this `" <> name <> "` is ambiguous since it could refer to:" <> candInfo

interpretGlobalName :: Hint -> DD.DefiniteDescription -> GN.GlobalName -> App WT.WeakTerm
interpretGlobalName m dd gn =
  case gn of
    GN.TopLevelFunc arity isConstLike ->
      if isConstLike
        then return $ m :< WT.PiElim (m :< WT.VarGlobal dd arity) []
        else return $ m :< WT.VarGlobal dd arity
    GN.Data arity _ isConstLike ->
      if isConstLike
        then return $ m :< WT.PiElim (m :< WT.VarGlobal dd arity) []
        else return $ m :< WT.VarGlobal dd arity
    GN.DataIntro dataArity consArity _ isConstLike -> do
      let e = m :< WT.VarGlobal dd (A.fromInt $ fromInteger (A.reify dataArity + A.reify consArity))
      if isConstLike
        then return $ m :< WT.PiElim e []
        else return e
    GN.PrimType primNum ->
      return $ m :< WT.Prim (WP.Type primNum)
    GN.PrimOp primOp ->
      case primOp of
        PO.PrimCmpOp {} ->
          castFromIntToBool $ m :< WT.Prim (WP.Value (WPV.Op primOp)) -- i1 to bool
        _ ->
          return $ m :< WT.Prim (WP.Value (WPV.Op primOp))
    GN.Resource ->
      return $ m :< WT.ResourceType dd

candFilter :: (a, Maybe b) -> Maybe (a, b)
candFilter (from, mTo) =
  fmap (from,) mTo

interpretDefiniteDescription :: Hint -> DD.DefiniteDescription -> App GN.GlobalName
interpretDefiniteDescription m dd = do
  mgn <- Global.lookup dd
  case mgn of
    Just gn ->
      return gn
    Nothing ->
      Throw.raiseError m $ "undefined constant: " <> DD.reify dd

resolveConstructor ::
  Hint ->
  RP.RawConsName ->
  App (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant, IsConstLike)
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

resolveConstructorMaybe ::
  Hint ->
  T.Text ->
  App (Maybe (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant, IsConstLike))
resolveConstructorMaybe m name = do
  locatorOrErr <- resolveNameOrErr m name
  case locatorOrErr of
    Right (dd, GN.DataIntro dataArity consArity disc isConstLike) ->
      return $ Just (dd, dataArity, consArity, disc, isConstLike)
    _ ->
      return Nothing

resolveConstructor' ::
  Hint ->
  DD.DefiniteDescription ->
  GN.GlobalName ->
  App (DD.DefiniteDescription, A.Arity, A.Arity, D.Discriminant, IsConstLike)
resolveConstructor' m dd gn =
  case gn of
    GN.DataIntro dataArity consArity disc isConstLike ->
      return (dd, dataArity, consArity, disc, isConstLike)
    _ ->
      Throw.raiseError m $ DD.reify dd <> " is not a constructor"

discernPatternMatrix ::
  NominalEnv ->
  RP.RawPatternMatrix RT.RawTerm ->
  App (PAT.PatternMatrix ([Ident], WT.WeakTerm))
discernPatternMatrix nenv patternMatrix =
  case RP.unconsRow patternMatrix of
    Nothing ->
      return $ PAT.new []
    Just (row, rows) -> do
      row' <- discernPatternRow nenv row
      rows' <- discernPatternMatrix nenv rows
      return $ PAT.consRow row' rows'

discernPatternRow ::
  NominalEnv ->
  RP.RawPatternRow RT.RawTerm ->
  App (PAT.PatternRow ([Ident], WT.WeakTerm))
discernPatternRow nenv (patVec, body) = do
  let patList = V.toList patVec
  (patList', body') <- discernPatternRow' nenv patList [] body
  return (V.fromList patList', body')

discernPatternRow' ::
  NominalEnv ->
  [(Hint, RP.RawPattern)] ->
  NominalEnv ->
  RT.RawTerm ->
  App ([(Hint, PAT.Pattern)], ([Ident], WT.WeakTerm))
discernPatternRow' nenv patList newVarList body = do
  case patList of
    [] -> do
      ensureVariableLinearity newVarList
      body' <- discern (newVarList ++ nenv) body
      return ([], ([], body'))
    pat : rest -> do
      (pat', varsInPat) <- discernPattern pat
      (rest', body') <- discernPatternRow' nenv rest (varsInPat ++ newVarList) body
      return (pat' : rest', body')

ensureVariableLinearity :: NominalEnv -> App ()
ensureVariableLinearity vars = do
  let linearityErrors = getNonLinearOccurrences vars S.empty []
  unless (null linearityErrors) $ Throw.throw $ L.MakeError linearityErrors

getNonLinearOccurrences :: NominalEnv -> S.Set T.Text -> [(Hint, T.Text)] -> [L.Log]
getNonLinearOccurrences vars found nonLinear =
  case vars of
    [] -> do
      let nonLinearVars = reverse $ nubBy (\x y -> snd x == snd y) nonLinear
      flip map nonLinearVars $ \(m, x) ->
        L.logError (fromHint m) $
          "the pattern variable `"
            <> x
            <> "` is used non-linearly"
    (from, (m, _)) : rest
      | S.member from found ->
          getNonLinearOccurrences rest found ((m, from) : nonLinear)
      | otherwise ->
          getNonLinearOccurrences rest (S.insert from found) nonLinear

discernPattern ::
  (Hint, RP.RawPattern) ->
  App ((Hint, PAT.Pattern), NominalEnv)
discernPattern (m, pat) =
  case pat of
    RP.Var (I (x, _)) -> do
      mConsInfo <- resolveConstructorMaybe m x
      case mConsInfo of
        Nothing -> do
          x' <- Gensym.newIdentFromText x
          return ((m, PAT.Var x'), [(x, (m, x'))])
        Just (consName, dataArity, consArity, disc, isConstLike) -> do
          unless isConstLike $
            Throw.raiseError m $
              "the constructor `" <> DD.reify consName <> "` can't be used as a constant"
          return ((m, PAT.Cons consName disc dataArity consArity []), [])
    RP.Cons cons args -> do
      (consName, dataArity, consArity, disc, isConstLike) <- resolveConstructor m cons
      when isConstLike $
        Throw.raiseError m $
          "the constructor `" <> RP.showRawConsName cons <> "` can't have any arguments"
      (args', nenvList) <- mapAndUnzipM discernPattern args
      return ((m, PAT.Cons consName disc dataArity consArity args'), concat nenvList)

ensurePatternMatrixSanity :: PAT.PatternMatrix a -> App ()
ensurePatternMatrixSanity mat =
  case PAT.unconsRow mat of
    Nothing ->
      return ()
    Just (row, rest) -> do
      ensurePatternRowSanity row
      ensurePatternMatrixSanity rest

ensurePatternRowSanity :: PAT.PatternRow a -> App ()
ensurePatternRowSanity (patternVector, _) = do
  mapM_ ensurePatternSanity $ V.toList patternVector

ensurePatternSanity :: (Hint, PAT.Pattern) -> App ()
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
  NominalEnv ->
  N.IsNoetic ->
  Hint ->
  V.Vector Ident ->
  PAT.PatternMatrix ([Ident], WT.WeakTerm) ->
  App (DT.DecisionTree WT.WeakTerm)
compilePatternMatrix nenv isNoetic m occurrences mat =
  case PAT.unconsRow mat of
    Nothing ->
      return DT.Unreachable
    Just (row, _) ->
      case PAT.getClauseBody row of
        Right (usedVars, (freedVars, body)) -> do
          let occurrences' = map (\o -> m :< WT.Var o) $ V.toList occurrences
          cursorVars <- mapM (castToNoemaIfNecessary nenv isNoetic) occurrences'
          DT.Leaf freedVars <$> bindLet nenv (zip usedVars cursorVars) body
        Left (mCol, i) -> do
          if i > 0
            then do
              occurrences' <- Throw.liftEither $ V.swap mCol i occurrences
              mat' <- Throw.liftEither $ PAT.swapColumn mCol i mat
              compilePatternMatrix nenv isNoetic mCol occurrences' mat'
            else do
              let headConstructors = PAT.getHeadConstructors mat
              let cursor = V.head occurrences
              clauseList <- forM headConstructors $ \(mPat, (cons, disc, dataArity, consArity, args)) -> do
                dataHoles <- mapM (const $ Gensym.newHole mPat (asHoleArgs nenv)) [1 .. A.reify dataArity]
                dataTypeHoles <- mapM (const $ Gensym.newHole mPat (asHoleArgs nenv)) [1 .. A.reify dataArity]
                consVars <- mapM (const $ Gensym.newIdentFromText "cvar") [1 .. A.reify consArity]
                let ms = map fst args
                (consArgs', nenv') <- alignConsArgs nenv $ zip ms consVars
                let occurrences' = V.fromList consVars <> V.tail occurrences
                specialMatrix <- PATS.specialize isNoetic nenv cursor (cons, consArity) mat
                specialDecisionTree <- compilePatternMatrix nenv' isNoetic mPat occurrences' specialMatrix
                return (DT.Cons mPat cons disc (zip dataHoles dataTypeHoles) consArgs' specialDecisionTree)
              fallbackMatrix <- PATF.getFallbackMatrix isNoetic nenv cursor mat
              fallbackClause <- compilePatternMatrix nenv isNoetic mCol (V.tail occurrences) fallbackMatrix
              t <- Gensym.newHole mCol (asHoleArgs nenv)
              return $ DT.Switch (cursor, t) (fallbackClause, clauseList)

alignConsArgs ::
  NominalEnv ->
  [(Hint, Ident)] ->
  App ([BinderF WT.WeakTerm], NominalEnv)
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
  NominalEnv ->
  [(Maybe (Hint, Ident), WT.WeakTerm)] ->
  WT.WeakTerm ->
  App WT.WeakTerm
bindLet nenv binder cont =
  case binder of
    [] ->
      return cont
    (Nothing, _) : xes -> do
      bindLet nenv xes cont
    (Just (m, from), to) : xes -> do
      h <- Gensym.newHole m (asHoleArgs nenv)
      cont' <- bindLet nenv xes cont
      return $ m :< WT.Let WT.Transparent (m, from, h) to cont'

castFromIntToBool :: WT.WeakTerm -> App WT.WeakTerm
castFromIntToBool e@(m :< _) = do
  let i1 = m :< WT.Prim (WP.Type (PT.Int (PNS.IntSize 1)))
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m C.coreBool
  bool <- discernGlobal m gl ll
  t <- Gensym.newHole m []
  x1 <- Gensym.newIdentFromText "arg"
  x2 <- Gensym.newIdentFromText "arg"
  let cmpOpType cod = m :< WT.Pi [(m, x1, t), (m, x2, t)] cod
  return $ m :< WT.Magic (M.Cast (cmpOpType i1) (cmpOpType (m :< WT.PiElim bool [])) e)

discernGlobal :: Hint -> GL.GlobalLocator -> LL.LocalLocator -> App WT.WeakTerm
discernGlobal m gl ll = do
  sgl <- Alias.resolveAlias m gl
  let dd = DD.new sgl ll
  gn <- interpretDefiniteDescription m dd
  interpretGlobalName m dd gn
