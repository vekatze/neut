module Scene.Parse.Discern
  ( discernStmtList,
    discernNameArrow,
    resolveName,
    resolveLocator,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.CodataDefinition qualified as CodataDefinition
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Context.UnusedVariable qualified as UnusedVariable
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.List
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Annotation qualified as AN
import Entity.Arity qualified as A
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Error qualified as E
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Mutability
import Entity.NameArrow qualified as NA
import Entity.NominalEnv
import Entity.Pattern qualified as PAT
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Remark qualified as R
import Entity.Stmt
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Noema
import Scene.Parse.Discern.NominalEnv
import Scene.Parse.Discern.PatternMatrix
import Scene.Parse.Discern.Struct
import Scene.Parse.Discern.Symbol

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
      (consArgsList', nenvList) <- mapAndUnzipM (discernBinder nenv) consArgsList
      forM_ (concat nenvList) $ \(_, (_, newVar)) -> do
        UnusedVariable.delete newVar
      return $ Data dataName dataArgs' $ zip4 consNameList isConstLikeList consArgsList' discriminantList
    DataIntro dataName dataArgs consArgs discriminant -> do
      (dataArgs', nenv) <- discernBinder empty dataArgs
      (consArgs', nenv') <- discernBinder nenv consArgs
      forM_ nenv' $ \(_, (_, newVar)) -> do
        UnusedVariable.delete newVar
      return $ DataIntro dataName dataArgs' consArgs' discriminant

discern :: NominalEnv -> RT.RawTerm -> App WT.WeakTerm
discern nenv term =
  case term of
    m :< RT.Tau ->
      return $ m :< WT.Tau
    m :< RT.Var (I (s, _)) -> do
      case lookup s nenv of
        Just (_, name) -> do
          UnusedVariable.delete name
          return $ m :< WT.Var name
        Nothing -> do
          resolveName m s >>= uncurry (interpretGlobalName m)
    m :< RT.VarGlobal globalLocator localLocator -> do
      (dd, gn) <- resolveLocator m globalLocator localLocator
      interpretGlobalName m dd gn
    -- discernGlobal m globalLocator localLocator
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
    m :< RT.Data name consNameList es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WT.Data name consNameList es'
    m :< RT.DataIntro dataName consName consNameList disc dataArgs consArgs -> do
      dataArgs' <- mapM (discern nenv) dataArgs
      consArgs' <- mapM (discern nenv) consArgs
      return $ m :< WT.DataIntro dataName consName consNameList disc dataArgs' consArgs'
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
    m :< RT.Annotation remarkLevel annot e -> do
      e' <- discern nenv e
      case annot of
        AN.Type _ -> do
          let doNotCare = m :< WT.Tau -- discarded at Infer
          return $ m :< WT.Annotation remarkLevel (AN.Type doNotCare) e'

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
  let localAddition = zipWith (\my yLocal -> (Ident.toText yLocal, (my, yLocal))) ms ysLocal
  nenvLocal <- joinNominalEnv localAddition nenv
  let contAddition = zipWith (\my yCont -> (Ident.toText yCont, (my, yCont))) ms ysCont
  nenvCont <- joinNominalEnv contAddition nenv
  e1' <- discern nenvLocal e1
  (mxt', _, e2') <- discernBinderWithBody' nenvCont mxt [] e2
  e2'' <- attachSuffix nenv (zip3 mutabilityList ysCont ysLocal) e2'
  let opacity = if null mys then WT.Transparent else WT.Noetic
  attachPrefix nenv (zip3 mutabilityList ysLocal ysActual) (m :< WT.Let opacity mxt' e1' e2'')

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
      nenv' <- extendNominalEnv mx x' nenv
      (xts', nenv'') <- discernBinder nenv' xts
      return ((mx, x', t') : xts', nenv'')

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
discernBinderWithBody' nenv (mx, x, codType) binder e = do
  (binder'', nenv') <- discernBinder nenv binder
  codType' <- discern nenv' codType
  x' <- Gensym.newIdentFromIdent x
  nenv'' <- extendNominalEnv mx x' nenv'
  e' <- discern nenv'' e
  return ((mx, x', codType'), binder'', e')

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
      nenv' <- joinNominalEnv newVarList nenv
      body' <- discern nenv' body
      return ([], ([], body'))
    pat : rest -> do
      (pat', varsInPat) <- discernPattern pat
      (rest', body') <- discernPatternRow' nenv rest (varsInPat ++ newVarList) body
      return (pat' : rest', body')

ensureVariableLinearity :: NominalEnv -> App ()
ensureVariableLinearity vars = do
  let linearityErrors = getNonLinearOccurrences vars S.empty []
  unless (null linearityErrors) $ Throw.throw $ E.MakeError linearityErrors

getNonLinearOccurrences :: NominalEnv -> S.Set T.Text -> [(Hint, T.Text)] -> [R.Remark]
getNonLinearOccurrences vars found nonLinear =
  case vars of
    [] -> do
      let nonLinearVars = reverse $ nubBy (\x y -> snd x == snd y) nonLinear
      flip map nonLinearVars $ \(m, x) ->
        R.newRemark m R.Error $
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
      errOrLocator <- resolveNameOrErr m x
      case errOrLocator of
        Left _ -> do
          x' <- Gensym.newIdentFromText x
          return ((m, PAT.Var x'), [(x, (m, x'))])
        Right (dd, gn) -> do
          mCons <- resolveConstructorMaybe m dd gn
          case mCons of
            Nothing -> do
              x' <- Gensym.newIdentFromText x
              return ((m, PAT.Var x'), [(x, (m, x'))])
            Just (consName, dataArity, consArity, disc, isConstLike) -> do
              unless isConstLike $
                Throw.raiseError m $
                  "the constructor `" <> DD.reify consName <> "` can't be used as a constant"
              return ((m, PAT.Cons consName disc dataArity consArity []), [])
    RP.NullaryCons gl ll -> do
      sgl <- Alias.resolveAlias m gl
      let consName = DD.new sgl ll
      gn <- interpretDefiniteDescription m consName
      (_, consName', gn') <- resolveAlias m consName gn
      case gn' of
        GN.DataIntro dataArity consArity disc _ ->
          return ((m, PAT.Cons consName' disc dataArity consArity []), [])
        _ ->
          Throw.raiseCritical m $
            "the symbol `" <> DD.reify consName <> "` isn't defined as a constuctor\n" <> T.pack (show gn)
    RP.Cons cons args -> do
      (consName, dataArity, consArity, disc, isConstLike) <- resolveConstructor m cons
      when isConstLike $
        Throw.raiseError m $
          "the constructor `" <> RP.showRawConsName cons <> "` can't have any arguments"
      (args', nenvList) <- mapAndUnzipM discernPattern args
      return ((m, PAT.Cons consName disc dataArity consArity args'), concat nenvList)

discernNameArrow :: NA.RawNameArrow -> App [NA.NameArrow]
discernNameArrow clause = do
  case clause of
    NA.Function clauseInfo -> do
      nameArrow@(_, (m, consGN)) <- discernInnerNameArrow clauseInfo
      ensureNonConstructor m $ traceGlobalName consGN
      return [nameArrow]
    NA.Variant (from, (mOrig, origVarOrLocator)) consArrowList -> do
      (_, dataDD, dataGN) <- resolveVarOrLocator mOrig origVarOrLocator
      availableConsList <- getConsListByGlobalName mOrig dataGN
      consArrowList' <- mapM discernInnerNameArrow consArrowList
      suppliedConsList <- getSuppliedConsList availableConsList consArrowList'
      return $ (from, (mOrig, GN.AliasData dataDD suppliedConsList dataGN)) : consArrowList'

getSuppliedConsList :: [DD.DefiniteDescription] -> [NA.NameArrow] -> App [DD.DefiniteDescription]
getSuppliedConsList availableConsList consArrowList = do
  resolvedConsInfoList <- mapM traceInnerNameArrow consArrowList
  forM_ resolvedConsInfoList $ \(mCons, consDD, consGN) ->
    case (consDD `elem` availableConsList, consGN) of
      (False, _) ->
        Throw.raiseError mCons "specified cons doesn't belong to the variant type"
      (True, GN.DataIntro {}) ->
        return ()
      (True, _) ->
        Throw.raiseError mCons "not a cons"
  return $ map (\(_, consDD, _) -> consDD) resolvedConsInfoList

ensureNonConstructor :: Hint -> GN.GlobalName -> App ()
ensureNonConstructor m gn =
  case gn of
    GN.Data {} ->
      Throw.raiseError m "variant types can only be exported via `variant-type {...}`"
    GN.DataIntro {} ->
      Throw.raiseError m "constructors can only be exported via `variant-type {...}`"
    _ ->
      return ()

traceGlobalName :: GN.GlobalName -> GN.GlobalName
traceGlobalName gn =
  case gn of
    GN.Alias _ gn' ->
      traceGlobalName gn'
    GN.AliasData _ _ gn' ->
      traceGlobalName gn'
    _ ->
      gn

traceInnerNameArrow :: NA.NameArrow -> App (Hint, DD.DefiniteDescription, GN.GlobalName)
traceInnerNameArrow ((mAlias, aliasDD), (mOrig, origGN)) =
  case origGN of
    GN.Alias newDD gn' ->
      traceInnerNameArrow ((mAlias, newDD), (mOrig, gn'))
    _ ->
      return (mAlias, aliasDD, origGN)

discernInnerNameArrow :: NA.InnerRawNameArrow -> App NA.NameArrow
discernInnerNameArrow (dom, (m, varOrLocator)) = do
  (_, dd, gn) <- resolveVarOrLocator m varOrLocator
  return (dom, (m, GN.Alias dd gn))

getConsListByGlobalName :: Hint -> GN.GlobalName -> App [DD.DefiniteDescription]
getConsListByGlobalName m gn =
  case gn of
    GN.Data _ consList _ ->
      return consList
    GN.AliasData _ consList _ ->
      return consList
    _ ->
      Throw.raiseError m "this isn't a variant type"
