module Scene.Parse.Discern (discernStmtList) where

import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Global qualified as Global
import Context.KeyArg qualified as KeyArg
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Context.UnusedVariable qualified as UnusedVariable
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.List
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.Annotation qualified as AN
import Entity.Arch qualified as Arch
import Entity.ArgNum qualified as AN
import Entity.Attr.Lam qualified as AttrL
import Entity.Attr.VarGlobal qualified as AttrVG
import Entity.Binder
import Entity.BuildMode qualified as BM
import Entity.C
import Entity.Const
import Entity.Decl qualified as DE
import Entity.DefiniteDescription qualified as DD
import Entity.Error qualified as E
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Hint.Reify qualified as Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.Key
import Entity.LamKind qualified as LK
import Entity.Locator qualified as L
import Entity.Name
import Entity.Noema qualified as N
import Entity.NominalEnv
import Entity.OS qualified as OS
import Entity.Opacity qualified as O
import Entity.Pattern qualified as PAT
import Entity.Platform qualified as Platform
import Entity.RawBinder
import Entity.RawDecl qualified as RDE
import Entity.RawIdent hiding (isHole)
import Entity.RawPattern qualified as RP
import Entity.RawTerm qualified as RT
import Entity.Remark qualified as R
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Name
import Scene.Parse.Discern.Noema
import Scene.Parse.Discern.NominalEnv
import Scene.Parse.Discern.PatternMatrix
import Scene.Parse.Discern.Struct

discernStmtList :: [RawStmt] -> App [WeakStmt]
discernStmtList =
  mapM discernStmt

discernStmt :: RawStmt -> App WeakStmt
discernStmt stmt = do
  registerTopLevelName stmt
  case stmt of
    RawStmtDefine isConstLike stmtKind m functionName impArgs expArgs codType e -> do
      -- printNote' $ RT.pp e
      (impArgs', nenv) <- discernBinder empty impArgs
      (expArgs', nenv') <- discernBinder nenv expArgs
      codType' <- discern nenv' codType
      stmtKind' <- discernStmtKind stmtKind
      e' <- discern nenv' e
      Tag.insertDD m functionName m
      forM_ expArgs' Tag.insertBinder
      return $ WeakStmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' e'
    RawStmtDefineConst m dd t v -> do
      t' <- discern empty t
      v' <- discern empty v
      Tag.insertDD m dd m
      return $ WeakStmtDefineConst m dd t' v'
    RawStmtDeclare m declList -> do
      declList' <- mapM discernDecl declList
      return $ WeakStmtDeclare m declList'

discernDecl :: RDE.RawDecl -> App (DE.Decl WT.WeakTerm)
discernDecl decl = do
  (impArgs', nenv) <- discernBinder empty (map f $ RDE.impArgs decl)
  (expArgs', nenv') <- discernBinder nenv (map f $ RDE.expArgs decl)
  forM_ (impArgs' ++ expArgs') $ \(_, x, _) -> UnusedVariable.delete x
  cod' <- discern nenv' (RDE.cod decl)
  return $
    DE.Decl
      { loc = RDE.loc decl,
        name = RDE.name decl,
        isConstLike = RDE.isConstLike decl,
        impArgs = impArgs',
        expArgs = expArgs',
        cod = cod'
      }

registerTopLevelName :: RawStmt -> App ()
registerTopLevelName stmt =
  case stmt of
    RawStmtDefine isConstLike stmtKind m functionName impArgs expArgs _ _ -> do
      let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
      let expArgNames = map (\(_, x, _, _, _) -> x) expArgs
      Global.registerStmtDefine isConstLike m stmtKind functionName allArgNum expArgNames
    RawStmtDefineConst m dd _ _ -> do
      Global.registerStmtDefine True m (SK.Normal O.Clear) dd AN.zero []
    RawStmtDeclare _ declList -> do
      mapM_ Global.registerDecl declList

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
              unless (isHole name') $ do
                Tag.insert m (T.length s) mDef
              return $ m :< WT.Var name'
        _ -> do
          (dd, (_, gn)) <- resolveName m name
          interpretGlobalName m dd gn
    m :< RT.Pi (_, (impArgs, _)) (_, (expArgs, _)) _ t -> do
      (impArgs', nenv') <- discernBinder nenv $ map f impArgs
      (expArgs', nenv'') <- discernBinder nenv' $ map f expArgs
      t' <- discern nenv'' t
      forM_ (impArgs' ++ expArgs') $ \(_, x, _) -> UnusedVariable.delete x
      return $ m :< WT.Pi impArgs' expArgs' t'
    m :< RT.PiIntro (_, (impArgs, _)) (_, (expArgs, _)) _ e -> do
      lamID <- Gensym.newCount
      (impArgs', nenv') <- discernBinder nenv $ map f impArgs
      (expArgs', nenv'') <- discernBinder nenv' $ map f expArgs
      e' <- discern nenv'' e
      return $ m :< WT.PiIntro (AttrL.normal lamID) impArgs' expArgs' e'
    m :< RT.PiIntroFix _ ((mx, x), _, (_, (impArgs, _)), (_, (expArgs, _)), _, codType, e) -> do
      (impArgs', nenv') <- discernBinder nenv $ map f impArgs
      (expArgs', nenv'') <- discernBinder nenv' $ map f expArgs
      codType' <- discern nenv'' $ fst codType
      x' <- Gensym.newIdentFromText x
      nenv''' <- extendNominalEnv mx x' nenv''
      e' <- discern nenv''' e
      let mxt' = (mx, x', codType')
      Tag.insertBinder mxt'
      lamID <- Gensym.newCount
      return $ m :< WT.PiIntro (AttrL.Attr {lamKind = LK.Fix mxt', identity = lamID}) impArgs' expArgs' e'
    m :< RT.PiElim e _ es -> do
      es' <- mapM (discern nenv . fst) es
      e' <- discern nenv e
      return $ m :< WT.PiElim False e' es'
    m :< RT.PiElimByKey name kvs -> do
      (dd, _) <- resolveName m name
      let (_, ks, vs) = unzip3 kvs
      ensureFieldLinearity m ks S.empty S.empty
      (argNum, keyList) <- KeyArg.lookup m dd
      vs' <- mapM (discern nenv) vs
      args <- KeyArg.reorderArgs m keyList $ Map.fromList $ zip ks vs'
      let isConstLike = False
      return $ m :< WT.PiElim False (m :< WT.VarGlobal (AttrVG.Attr {..}) dd) args
    m :< RT.PiElimExact e -> do
      e' <- discern nenv e
      return $ m :< WT.PiElimExact e'
    m :< RT.Data name consNameList es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WT.Data name consNameList es'
    m :< RT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (discern nenv) dataArgs
      consArgs' <- mapM (discern nenv) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< RT.DataElim isNoetic es patternMatrix -> do
      let ms = map (\(me :< _) -> me) es
      os <- mapM (const $ Gensym.newIdentFromText "match") es -- os: occurrences
      es' <- mapM (discern nenv >=> castFromNoemaIfNecessary isNoetic) es
      ts <- mapM (const $ Gensym.newHole m []) es'
      patternMatrix' <- discernPatternMatrix nenv patternMatrix
      ensurePatternMatrixSanity patternMatrix'
      let os' = zip ms os
      decisionTree <- compilePatternMatrix nenv isNoetic (V.fromList os') patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es' ts) decisionTree
    m :< RT.Noema t -> do
      t' <- discern nenv t
      return $ m :< WT.Noema t'
    m :< RT.Embody e -> do
      e' <- discern nenv e
      return $ m :< WT.Embody (doNotCare m) e'
    m :< RT.Let letKind (mx, pat, c1, c2, (t, _)) mys e1@(m1 :< _) e2@(m2 :< _) -> do
      case letKind of
        RT.Try -> do
          eitherTypeInner <- locatorToVarGlobal mx coreExcept
          leftType <- Gensym.newPreHole m1
          let eitherType = m1 :< RT.piElim eitherTypeInner [leftType, t]
          e1' <- ascribe m1 eitherType e1
          err <- Gensym.newText
          exceptFail <- locatorToName m2 coreExceptFail
          exceptPass <- locatorToName m2 coreExceptPass
          exceptFailVar <- locatorToVarGlobal mx coreExceptFail
          let _m = blur m2
          discern nenv $
            m2
              :< RT.DataElim
                False
                [e1']
                ( RP.new
                    [ ( V.fromList [(_m, RP.Cons exceptFail (RP.Paren [(_m, RP.Var (Var err))]))],
                        m2 :< RT.piElim exceptFailVar [m2 :< RT.Var (Var err)]
                      ),
                      ( V.fromList [(_m, RP.Cons exceptPass (RP.Paren [(mx, pat)]))],
                        e2
                      )
                    ]
                )
        RT.Bind -> do
          Throw.raiseError m "`bind` can only be used inside `with`"
        RT.Plain -> do
          (x, modifier) <- getContinuationModifier (mx, pat)
          discernLet nenv m (mx, x, c1, c2, t) mys e1 (modifier False e2)
        RT.Noetic -> do
          (x, modifier) <- getContinuationModifier (mx, pat)
          discernLet nenv m (mx, x, c1, c2, t) mys e1 (modifier True e2)
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
    m :< RT.Resource dd discarder copier -> do
      resourceID <- Gensym.newCount
      discarder' <- discern nenv discarder
      copier' <- discern nenv copier
      return $ m :< WT.Resource dd resourceID discarder' copier'
    m :< RT.Use e xs cont -> do
      e' <- discern nenv e
      (xs', nenv') <- discernBinder nenv xs
      cont' <- discern nenv' cont
      return $ m :< WT.Use e' xs' cont'
    m :< RT.If ifCond ifBody elseIfList elseBody -> do
      boolTrue <- locatorToName (blur m) coreBoolTrue
      boolFalse <- locatorToName (blur m) coreBoolFalse
      discern nenv $ foldIf m boolTrue boolFalse ifCond ifBody elseIfList elseBody
    m :< RT.Seq e1 e2 -> do
      h <- Gensym.newTextForHole
      unit <- locatorToVarGlobal m coreUnit
      discern nenv $ m :< RT.Let RT.Plain (m, RP.Var (Var h), [], [], (unit, [])) [] e1 e2
    m :< RT.When whenCond whenBody -> do
      boolTrue <- locatorToName (blur m) coreBoolTrue
      boolFalse <- locatorToName (blur m) coreBoolFalse
      unitUnit <- locatorToVarGlobal m coreUnitUnit
      discern nenv $ foldIf m boolTrue boolFalse whenCond whenBody [] unitUnit
    m :< RT.ListIntro es -> do
      listNil <- locatorToVarGlobal m coreListNil
      listCons <- locatorToVarGlobal m coreListCons
      discern nenv $ foldListApp m listNil listCons es
    m :< RT.Admit -> do
      admit <- locatorToVarGlobal m coreSystemAdmit
      t <- Gensym.newPreHole (blur m)
      textType <- locatorToVarGlobal m coreText
      discern nenv $
        m
          :< RT.Annotation
            R.Warning
            (AN.Type ())
            ( m
                :< RT.piElim
                  admit
                  [t, m :< RT.Prim (WP.Value (WPV.StaticText textType ("admit: " <> T.pack (Hint.toString m) <> "\n")))]
            )
    m :< RT.Detach e -> do
      t <- Gensym.newPreHole (blur m)
      detachVar <- locatorToVarGlobal m coreThreadDetach
      discern nenv $ m :< RT.piElim detachVar [t, RT.lam m [] e]
    m :< RT.Attach e -> do
      t <- Gensym.newPreHole (blur m)
      attachVar <- locatorToVarGlobal m coreThreadAttach
      discern nenv $ m :< RT.piElim attachVar [t, e]
    m :< RT.Option t -> do
      exceptVar <- locatorToVarGlobal m coreExcept
      unit <- locatorToVarGlobal m coreUnit
      discern nenv $ m :< RT.piElim exceptVar [unit, t]
    m :< RT.Assert (mText, message) e@(mCond :< _) -> do
      assert <- locatorToVarGlobal m coreSystemAssert
      textType <- locatorToVarGlobal m coreText
      let fullMessage = T.pack (Hint.toString m) <> "\nassertion failure: " <> message <> "\n"
      discern nenv $
        m
          :< RT.piElim
            assert
            [mText :< RT.Prim (WP.Value (WPV.StaticText textType fullMessage)), RT.lam mCond [] e]
    m :< RT.Introspect key clauseList -> do
      value <- getIntrospectiveValue m key
      clause <- lookupIntrospectiveClause m value clauseList
      discern nenv clause
    m :< RT.With binder body -> do
      case body of
        mLet :< RT.Let letKind mxt@(mPat, pat, c1, c2, t) mys e1 e2 -> do
          let e1' = m :< RT.With binder e1
          let e2' = m :< RT.With binder e2
          case letKind of
            RT.Bind -> do
              (x, modifier) <- getContinuationModifier (mPat, pat)
              discern nenv $ m :< RT.piElim binder [e1', RT.lam m [(mPat, x, c1, c2, t)] (modifier False e2')]
            _ -> do
              discern nenv $ mLet :< RT.Let letKind mxt mys e1' e2'
        mSeq :< RT.Seq e1 e2 -> do
          let e1' = m :< RT.With binder e1
          let e2' = m :< RT.With binder e2
          discern nenv $ mSeq :< RT.Seq e1' e2'
        mUse :< RT.Use item vars cont -> do
          let cont' = m :< RT.With binder cont
          discern nenv $ mUse :< RT.Use item vars cont'
        _ ->
          discern nenv body

getContinuationModifier :: (Hint, RP.RawPattern) -> App (RawIdent, N.IsNoetic -> RT.RawTerm -> RT.RawTerm)
getContinuationModifier pat =
  case pat of
    (_, RP.Var (Var x))
      | not (isConsName x) ->
          return (x, \_ cont -> cont)
    _ -> do
      tmp <- Gensym.newTextForHole
      return
        ( tmp,
          \isNoetic cont@(mCont :< _) ->
            mCont :< RT.DataElim isNoetic [mCont :< RT.Var (Var tmp)] (RP.new [(V.fromList [pat], cont)])
        )

ascribe :: Hint -> RT.RawTerm -> RT.RawTerm -> App RT.RawTerm
ascribe m t e = do
  tmp <- Gensym.newTextForHole
  return $ bind (m, tmp, [], [], t) e (m :< RT.Var (Var tmp))

bind :: RawBinder RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind (m, x, c1, c2, t) e cont =
  m :< RT.Let RT.Plain (m, RP.Var (Var x), c1, c2, (t, [])) [] e cont

foldListApp :: Hint -> RT.RawTerm -> RT.RawTerm -> [RT.RawTerm] -> RT.RawTerm
foldListApp m listNil listCons es =
  case es of
    [] ->
      listNil
    e : rest ->
      m :< RT.piElim listCons [e, foldListApp m listNil listCons rest]

lookupIntrospectiveClause :: Hint -> T.Text -> [(Maybe T.Text, RT.RawTerm)] -> App RT.RawTerm
lookupIntrospectiveClause m value clauseList =
  case clauseList of
    [] ->
      Throw.raiseError m $ "this term doesn't support `" <> value <> "`."
    (Just key, clause) : rest
      | key == value ->
          return clause
      | otherwise ->
          lookupIntrospectiveClause m value rest
    (Nothing, clause) : _ ->
      return clause

getIntrospectiveValue :: Hint -> T.Text -> App T.Text
getIntrospectiveValue m key = do
  bm <- Env.getBuildMode
  case key of
    "platform" -> do
      return $ Platform.reify Platform.platform
    "arch" ->
      return $ Arch.reify (Platform.arch Platform.platform)
    "os" ->
      return $ OS.reify (Platform.os Platform.platform)
    "build-mode" ->
      return $ BM.reify bm
    _ ->
      Throw.raiseError m $ "no such introspective value is defined: " <> key

foldIf ::
  Hint ->
  Name ->
  Name ->
  RT.RawTerm ->
  RT.RawTerm ->
  [(RT.RawTerm, RT.RawTerm)] ->
  RT.RawTerm ->
  RT.RawTerm
foldIf m true false ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(blur m, RP.Var true)], ifBody),
                (V.fromList [(blur m, RP.Var false)], elseBody)
              ]
          )
    ((elseIfCond, elseIfBody) : rest) -> do
      let cont = foldIf m true false elseIfCond elseIfBody rest elseBody
      m
        :< RT.DataElim
          False
          [ifCond]
          ( RP.new
              [ (V.fromList [(blur m, RP.Var true)], ifBody),
                (V.fromList [(blur m, RP.Var false)], cont)
              ]
          )

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
  mys' <- mapM (\(my, y) -> discernIdent my nenv y) mys
  let (ms', ys') = unzip mys'
  let ysActual = zipWith (\my y -> my :< WT.Var y) ms' ys'
  ysLocal <- mapM Gensym.newIdentFromIdent ys'
  ysCont <- mapM Gensym.newIdentFromIdent ys'
  let localAddition = zipWith (\my yLocal -> (Ident.toText yLocal, (my, yLocal))) ms' ysLocal
  nenvLocal <- joinNominalEnv localAddition nenv
  let contAddition = zipWith (\my yCont -> (Ident.toText yCont, (my, yCont))) ms' ysCont
  nenvCont <- joinNominalEnv contAddition nenv
  e1' <- discern nenvLocal e1
  (mxt', _, e2') <- discernBinderWithBody' nenvCont mxt [] e2
  Tag.insertBinder mxt'
  e2'' <- attachSuffix (zip ysCont ysLocal) e2'
  let opacity = if null mys then WT.Clear else WT.Noetic
  attachPrefix (zip ysLocal (zip ms' ysActual)) (m :< WT.Let opacity mxt' e1' e2'')

discernIdent :: Hint -> NominalEnv -> RawIdent -> App (Hint, Ident)
discernIdent m nenv x =
  case lookup x nenv of
    Nothing ->
      Throw.raiseError m $ "undefined variable: " <> x
    Just ident@(_, x') -> do
      UnusedVariable.delete x'
      return ident

discernBinder ::
  NominalEnv ->
  [RawBinder RT.RawTerm] ->
  App ([BinderF WT.WeakTerm], NominalEnv)
discernBinder nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x, _, _, t) : xts -> do
      t' <- discern nenv t
      x' <- Gensym.newIdentFromText x
      nenv' <- extendNominalEnv mx x' nenv
      (xts', nenv'') <- discernBinder nenv' xts
      Tag.insertBinder (mx, x', t')
      return ((mx, x', t') : xts', nenv'')

discernBinderWithBody' ::
  NominalEnv ->
  RawBinder RT.RawTerm ->
  [RawBinder RT.RawTerm] ->
  RT.RawTerm ->
  App (BinderF WT.WeakTerm, [BinderF WT.WeakTerm], WT.WeakTerm)
discernBinderWithBody' nenv (mx, x, _, _, codType) binder e = do
  (binder'', nenv') <- discernBinder nenv binder
  codType' <- discern nenv' codType
  x' <- Gensym.newIdentFromText x
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
discernPattern (m, pat) = do
  case pat of
    RP.Var name -> do
      case name of
        Var x
          | isConsName x -> do
              (consDD, dataArgNum, consArgNum, disc, isConstLike, _) <- resolveConstructor m $ Var x
              unless isConstLike $
                Throw.raiseError m $
                  "the constructor `" <> DD.reify consDD <> "` can't be used as a constant"
              return ((m, PAT.Cons (PAT.ConsInfo {args = [], ..})), [])
          | otherwise -> do
              x' <- Gensym.newIdentFromText x
              return ((m, PAT.Var x'), [(x, (m, x'))])
        Locator l -> do
          (dd, gn) <- resolveName m $ Locator l
          case gn of
            (_, GN.DataIntro dataArgNum consArgNum disc isConstLike) -> do
              let consInfo =
                    PAT.ConsInfo
                      { consDD = dd,
                        isConstLike = isConstLike,
                        disc = disc,
                        dataArgNum = dataArgNum,
                        consArgNum = consArgNum,
                        args = []
                      }
              return ((m, PAT.Cons consInfo), [])
            _ ->
              Throw.raiseCritical m $
                "the symbol `" <> DD.reify dd <> "` isn't defined as a constuctor\n" <> T.pack (show gn)
    RP.Cons cons mArgs -> do
      (consName, dataArgNum, consArgNum, disc, isConstLike, _) <- resolveConstructor m cons
      when isConstLike $
        Throw.raiseError m $
          "the constructor `" <> showName cons <> "` can't have any arguments"
      case mArgs of
        RP.Paren args -> do
          (args', nenvList) <- mapAndUnzipM discernPattern args
          let consInfo =
                PAT.ConsInfo
                  { consDD = consName,
                    isConstLike = isConstLike,
                    disc = disc,
                    dataArgNum = dataArgNum,
                    consArgNum = consArgNum,
                    args = args'
                  }
          return ((m, PAT.Cons consInfo), concat nenvList)
        RP.Of mkvs -> do
          let (ks, mvs) = unzip mkvs
          ensureFieldLinearity m ks S.empty S.empty
          (_, keyList) <- KeyArg.lookup m consName
          defaultKeyMap <- constructDefaultKeyMap m keyList
          let specifiedKeyMap = Map.fromList $ zip ks mvs
          let keyMap = Map.union specifiedKeyMap defaultKeyMap
          reorderedArgs <- KeyArg.reorderArgs m keyList keyMap
          (patList', nenvList) <- mapAndUnzipM discernPattern reorderedArgs
          let consInfo =
                PAT.ConsInfo
                  { consDD = consName,
                    isConstLike = isConstLike,
                    disc = disc,
                    dataArgNum = dataArgNum,
                    consArgNum = consArgNum,
                    args = patList'
                  }
          return ((m, PAT.Cons consInfo), concat nenvList)
    RP.ListIntro patList -> do
      listNil <- Throw.liftEither $ DD.getLocatorPair m coreListNil
      listCons <- locatorToName m coreListCons
      discernPattern $ foldListAppPat m listNil listCons patList

foldListAppPat ::
  Hint ->
  L.Locator ->
  Name ->
  [(Hint, RP.RawPattern)] ->
  (Hint, RP.RawPattern)
foldListAppPat m listNil listCons es =
  case es of
    [] ->
      (m, RP.Var $ Locator listNil)
    e : rest -> do
      let rest' = foldListAppPat m listNil listCons rest
      (m, RP.Cons listCons (RP.Paren [e, rest']))

constructDefaultKeyMap :: Hint -> [Key] -> App (Map.HashMap Key (Hint, RP.RawPattern))
constructDefaultKeyMap m keyList = do
  names <- mapM (const Gensym.newTextForHole) keyList
  return $ Map.fromList $ zipWith (\k v -> (k, (m, RP.Var (Var v)))) keyList names

locatorToName :: Hint -> T.Text -> App Name
locatorToName m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair m text
  return $ Locator (gl, ll)

locatorToVarGlobal :: Hint -> T.Text -> App RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- Throw.liftEither $ DD.getLocatorPair (blur m) text
  return $ blur m :< RT.Var (Locator (gl, ll))

f :: RawBinder (a, C) -> RawBinder a
f (m, x, c1, c2, (t, _)) =
  (m, x, c1, c2, t)
