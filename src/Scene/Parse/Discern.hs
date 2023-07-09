module Scene.Parse.Discern (discernStmtList) where

import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.KeyArg qualified as KeyArg
import Context.NameDependence qualified as NameDependence
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Context.UnusedVariable qualified as UnusedVariable
import Context.Via qualified as Via
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Data.Char (isUpper)
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.List
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Annotation qualified as AN
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Error qualified as E
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Name
import Entity.NominalEnv
import Entity.Pattern qualified as PAT
import Entity.RawBinder
import Entity.RawIdent
import Entity.RawLamKind qualified as RLK
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Remark qualified as R
import Entity.Source qualified as Source
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Name
import Scene.Parse.Discern.Noema
import Scene.Parse.Discern.NominalEnv
import Scene.Parse.Discern.PatternMatrix
import Scene.Parse.Discern.Struct

discernStmtList :: [RawStmt] -> App [WeakStmt]
discernStmtList stmtList =
  discernStmtList' $ sortBy compareRawStmt stmtList

discernStmtList' :: [RawStmt] -> App [WeakStmt]
discernStmtList' stmtList =
  case stmtList of
    [] ->
      return []
    RawStmtDefine isConstLike stmtKind m functionName impArgNum xts codType e : rest -> do
      (xts', nenv) <- discernBinder empty xts
      codType' <- discern nenv codType
      stmtKind' <- discernStmtKind stmtKind
      e' <- discern nenv e
      rest' <- discernStmtList' rest
      return $ WeakStmtDefine isConstLike stmtKind' m functionName impArgNum xts' codType' e' : rest'
    RawStmtDefineResource m name discarder copier : rest -> do
      discarder' <- discern empty discarder
      copier' <- discern empty copier
      rest' <- discernStmtList' rest
      return $ WeakStmtDefineResource m name discarder' copier' : rest'
    RawStmtVia m consName consArgs : rest -> do
      forM_ consArgs $ \(consArgName, mViaName) -> do
        case mViaName of
          Nothing ->
            return ()
          Just viaName -> do
            (nameDep, (mDep, gnDep)) <- resolveName m viaName
            src <- Env.getCurrentSource
            let path = Source.sourceFilePath src
            NameDependence.add path (Map.singleton nameDep (mDep, gnDep))
            Via.union path $ Map.singleton consName (Map.singleton consArgName nameDep)
      discernStmtList' rest

compareRawStmt :: RawStmt -> RawStmt -> Ordering
compareRawStmt stmt1 stmt2 =
  case (stmt1, stmt2) of
    (RawStmtVia {}, RawStmtVia {}) ->
      EQ
    (RawStmtVia {}, _) ->
      LT
    (_, RawStmtVia {}) ->
      GT
    (_, _) ->
      EQ

discernStmtKind :: SK.RawStmtKind -> App (SK.StmtKind WT.WeakTerm)
discernStmtKind stmtKind =
  case stmtKind of
    SK.Normal opacity ->
      return $ SK.Normal opacity
    SK.Data dataName dataArgs consInfoList -> do
      (dataArgs', nenv) <- discernBinder empty dataArgs
      let (locList, consNameList, isConstLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      (consArgsList', nenvList) <- mapAndUnzipM (discernBinder nenv) consArgsList
      forM_ (concat nenvList) $ \(_, (_, newVar)) -> do
        UnusedVariable.delete newVar
      let consInfoList' = zip5 locList consNameList isConstLikeList consArgsList' discriminantList
      return $ SK.Data dataName dataArgs' consInfoList'
    SK.DataIntro dataName dataArgs consArgs discriminant -> do
      (dataArgs', nenv) <- discernBinder empty dataArgs
      (consArgs', nenv') <- discernBinder nenv consArgs
      forM_ nenv' $ \(_, (_, newVar)) -> do
        UnusedVariable.delete newVar
      return $ SK.DataIntro dataName dataArgs' consArgs' discriminant

discern :: NominalEnv -> RT.RawTerm -> App WT.WeakTerm
discern nenv term =
  case term of
    m :< RT.Tau ->
      return $ m :< WT.Tau
    m :< RT.Var name ->
      case name of
        Var s
          | Just (mDef, name') <- lookup s nenv -> do
              UnusedVariable.delete name'
              Tag.insert m (T.length s) mDef
              return $ m :< WT.Var name'
        _ -> do
          (dd, (_, gn)) <- resolveName m name
          interpretGlobalName m dd gn
    m :< RT.Pi xts t -> do
      (xts', t') <- discernBinderWithBody nenv xts t
      forM_ xts' $ \(_, x, _) -> UnusedVariable.delete x
      return $ m :< WT.Pi xts' t'
    m :< RT.PiIntro kind xts e -> do
      case kind of
        RLK.Fix xt -> do
          (xt', xts', e') <- discernBinderWithBody' nenv xt xts e
          return $ m :< WT.PiIntro (LK.Fix xt') xts' e'
        RLK.Normal opacity -> do
          (xts', e') <- discernBinderWithBody nenv xts e
          return $ m :< WT.PiIntro (LK.Normal opacity) xts' e'
    m :< RT.PiElim e es -> do
      es' <- mapM (discern nenv) es
      e' <- discern nenv e
      return $ m :< WT.PiElim e' es'
    m :< RT.PiElimByKey name kvs -> do
      (dd, _) <- resolveName m name
      let (_, ks, vs) = unzip3 kvs
      ensureFieldLinearity m ks S.empty S.empty
      (arity, keyList) <- KeyArg.lookup m dd
      vs' <- mapM (discern nenv) vs
      args <- reorderArgs m keyList $ Map.fromList $ zip ks vs'
      return $ m :< WT.PiElim (m :< WT.VarGlobal dd arity) args
    m :< RT.Data name consNameList es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WT.Data name consNameList es'
    m :< RT.DataIntro dataName consName consNameList disc dataArgs consArgs -> do
      dataArgs' <- mapM (discern nenv) dataArgs
      consArgs' <- mapM (discern nenv) consArgs
      return $ m :< WT.DataIntro dataName consName consNameList disc dataArgs' consArgs'
    m :< RT.DataElim isNoetic es patternMatrix -> do
      os <- mapM (const $ Gensym.newIdentFromText "match") es -- os: occurrences
      es' <- mapM (discern nenv >=> castFromNoemaIfNecessary isNoetic) es
      ts <- mapM (const $ Gensym.newHole m []) es'
      patternMatrix' <- discernPatternMatrix nenv patternMatrix
      ensurePatternMatrixSanity patternMatrix'
      decisionTree <- compilePatternMatrix nenv isNoetic m (V.fromList os) patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es' ts) decisionTree
    m :< RT.Noema t -> do
      t' <- discern nenv t
      return $ m :< WT.Noema t'
    m :< RT.Embody e -> do
      e' <- discern nenv e
      return $ m :< WT.Embody (doNotCare m) e'
    m :< RT.Let mxt mys e1 e2 -> do
      discernLet nenv m mxt mys e1 e2
    m :< RT.Prim prim -> do
      prim' <- mapM (discern nenv) prim
      return $ m :< WT.Prim prim'
    m :< RT.Hole k ->
      return $ m :< WT.Hole k []
    m :< RT.Magic der -> do
      der' <- traverse (discern nenv) der
      return $ m :< WT.Magic der'
    m :< RT.Annotation remarkLevel annot e -> do
      e' <- discern nenv e
      case annot of
        AN.Type _ ->
          return $ m :< WT.Annotation remarkLevel (AN.Type (doNotCare m)) e'
    m :< RT.Flow l t -> do
      flowDD <- fst <$> resolveName m (Locator l)
      t' <- discern nenv t
      return $ m :< WT.Flow flowDD t'
    m :< RT.FlowIntro flowL detachL e -> do
      flowDD <- fst <$> resolveName m (Locator flowL)
      detachDD <- fst <$> resolveName m (Locator detachL)
      e' <- discern nenv e
      return $ m :< WT.FlowIntro flowDD detachDD (e', doNotCare m)
    m :< RT.FlowElim flowL attachL e -> do
      flowDD <- fst <$> resolveName m (Locator flowL)
      attachDD <- fst <$> resolveName m (Locator attachL)
      e' <- discern nenv e
      return $ m :< WT.FlowElim flowDD attachDD (e', doNotCare m)

doNotCare :: Hint -> WT.WeakTerm
doNotCare m =
  m :< WT.Tau

discernLet ::
  NominalEnv ->
  Hint ->
  RawBinder RT.RawTerm ->
  [(Hint, RawIdent)] ->
  RT.RawTerm ->
  RT.RawTerm ->
  App WT.WeakTerm
discernLet nenv m mxt mys e1 e2 = do
  let (ms, ys) = unzip mys
  ysActual <- zipWithM (\my y -> discern nenv (my :< RT.Var (Var y))) ms ys
  ysLocal <- mapM Gensym.newIdentFromText ys
  ysCont <- mapM Gensym.newIdentFromText ys
  let localAddition = zipWith (\my yLocal -> (Ident.toText yLocal, (my, yLocal))) ms ysLocal
  nenvLocal <- joinNominalEnv localAddition nenv
  let contAddition = zipWith (\my yCont -> (Ident.toText yCont, (my, yCont))) ms ysCont
  nenvCont <- joinNominalEnv contAddition nenv
  e1' <- discern nenvLocal e1
  (mxt', _, e2') <- discernBinderWithBody' nenvCont mxt [] e2
  e2'' <- attachSuffix (zip ysCont ysLocal) e2'
  let opacity = if null mys then WT.Transparent else WT.Noetic
  attachPrefix (zip ysLocal ysActual) (m :< WT.Let opacity mxt' e1' e2'')

discernBinder ::
  NominalEnv ->
  [RawBinder RT.RawTerm] ->
  App ([BinderF WT.WeakTerm], NominalEnv)
discernBinder nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x, t) : xts -> do
      t' <- discern nenv t
      x' <- Gensym.newIdentFromText x
      nenv' <- extendNominalEnv mx x' nenv
      (xts', nenv'') <- discernBinder nenv' xts
      return ((mx, x', t') : xts', nenv'')

discernBinderWithBody ::
  NominalEnv ->
  [RawBinder RT.RawTerm] ->
  RT.RawTerm ->
  App ([BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody nenv binder e = do
  (binder', nenv') <- discernBinder nenv binder
  e' <- discern nenv' e
  return (binder', e')

discernBinderWithBody' ::
  NominalEnv ->
  RawBinder RT.RawTerm ->
  [RawBinder RT.RawTerm] ->
  RT.RawTerm ->
  App (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody' nenv (mx, x, codType) binder e = do
  (binder'', nenv') <- discernBinder nenv binder
  codType' <- discern nenv' codType
  x' <- Gensym.newIdentFromText x
  UnusedVariable.delete x'
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
      let nonLinearVars = reverse $ ListUtils.nubOrdOn snd nonLinear
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
    RP.Var name -> do
      case name of
        Var x -> do
          errOrLocator <- resolveNameOrError m $ Var x
          case errOrLocator of
            Left _ -> do
              case T.uncons x of
                Just (c, _)
                  | isUpper c -> do
                      Throw.raiseError m $ "no such constructor is defined: `" <> x <> "`"
                _ -> do
                  x' <- Gensym.newIdentFromText x
                  return ((m, PAT.Var x'), [(x, (m, x'))])
            Right (dd, (_, gn)) -> do
              mCons <- resolveConstructorMaybe dd gn
              case mCons of
                Nothing -> do
                  x' <- Gensym.newIdentFromText x
                  return ((m, PAT.Var x'), [(x, (m, x'))])
                Just (consName, dataArity, consArity, disc, isConstLike, _) -> do
                  unless isConstLike $
                    Throw.raiseError m $
                      "the constructor `" <> DD.reify consName <> "` can't be used as a constant"
                  return ((m, PAT.Cons consName disc dataArity consArity []), [])
        Locator l -> do
          (dd, gn) <- resolveName m $ Locator l
          case gn of
            (_, GN.NatZero) ->
              return ((m, PAT.NatZero), [])
            (_, GN.DataIntro dataArity consArity disc _) ->
              return ((m, PAT.Cons dd disc dataArity consArity []), [])
            _ ->
              Throw.raiseCritical m $
                "the symbol `" <> DD.reify dd <> "` isn't defined as a constuctor\n" <> T.pack (show gn)
        DefiniteDescription dd -> do
          (_, gn) <- resolveName m $ DefiniteDescription dd
          case gn of
            (_, GN.NatZero) ->
              return ((m, PAT.NatZero), [])
            (_, GN.DataIntro dataArity consArity disc _) ->
              return ((m, PAT.Cons dd disc dataArity consArity []), [])
            _ ->
              Throw.raiseCritical m $
                "the symbol `" <> DD.reify dd <> "` isn't defined as a constuctor\n" <> T.pack (show gn)
    RP.Cons cons mArgs -> do
      (consName, dataArity, consArity, disc, isConstLike, gn) <- resolveConstructor m cons
      when isConstLike $
        Throw.raiseError m $
          "the constructor `" <> showName cons <> "` can't have any arguments"
      case mArgs of
        Right args -> do
          (args', nenvList) <- mapAndUnzipM discernPattern args
          case gn of
            Just GN.NatZero -> do
              return ((m, PAT.NatZero), [])
            Just GN.NatSucc -> do
              case args' of
                [arg'] ->
                  return ((m, PAT.NatSucc arg'), concat nenvList)
                _ ->
                  -- (raises an arity mismatch error at Scene.Elaborate)
                  return ((m, PAT.Cons consName disc dataArity consArity args'), concat nenvList)
            _ -> do
              return ((m, PAT.Cons consName disc dataArity consArity args'), concat nenvList)
        Left mVar -> do
          vmap <- Via.lookup consName
          (_, keyList) <- KeyArg.lookup m consName
          patList <- mapM (keyToPattern vmap m) keyList
          (patList', nenvList) <- mapAndUnzipM discernPattern $ map (mVar,) patList
          forM_ (concat nenvList) $ \(_, (_, newVar)) -> do
            UnusedVariable.delete newVar
          return ((m, PAT.Cons consName disc dataArity consArity patList'), concat nenvList)
    RP.NatZero ->
      return ((m, PAT.NatZero), [])
    RP.NatSucc arg -> do
      (arg', nenv) <- discernPattern arg
      return ((m, PAT.NatSucc arg'), nenv)

keyToPattern :: Map.HashMap RawIdent DD.DefiniteDescription -> Hint -> RawIdent -> App RP.RawPattern
keyToPattern vmap m key =
  case Map.lookup key vmap of
    Just consName -> do
      return $ RP.Cons (DefiniteDescription consName) (Left m)
    Nothing ->
      return $ RP.Var $ Var key
