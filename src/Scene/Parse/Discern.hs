module Scene.Parse.Discern (discernStmtList) where

import Context.App
import Context.Decl qualified as Decl
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
import Entity.DeclarationName qualified as DN
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
import Entity.Magic qualified as M
import Entity.Name
import Entity.Noema qualified as N
import Entity.NominalEnv
import Entity.OS qualified as OS
import Entity.Opacity qualified as O
import Entity.Pattern qualified as PAT
import Entity.Platform qualified as Platform
import Entity.RawBinder
import Entity.RawIdent hiding (isHole)
import Entity.RawPattern qualified as RP
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.Remark qualified as R
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Entity.Syntax.Series qualified as SE
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Data
import Scene.Parse.Discern.Name
import Scene.Parse.Discern.Noema
import Scene.Parse.Discern.NominalEnv
import Scene.Parse.Discern.PatternMatrix
import Scene.Parse.Discern.Struct

discernStmtList :: [RawStmt] -> App [WeakStmt]
discernStmtList =
  fmap concat . mapM discernStmt

discernStmt :: RawStmt -> App [WeakStmt]
discernStmt stmt = do
  case stmt of
    RawStmtDefine _ stmtKind (RT.RawDef {decl, body}) -> do
      registerTopLevelName stmt
      let impArgs = RT.extractArgs $ RT.impArgs decl
      let expArgs = RT.extractArgs $ RT.expArgs decl
      let (_, codType) = RT.cod decl
      let m = RT.loc decl
      let (functionName, _) = RT.name decl
      let isConstLike = RT.isConstLike decl
      (impArgs', nenv) <- discernBinder empty impArgs
      (expArgs', nenv') <- discernBinder nenv expArgs
      codType' <- discern nenv' codType
      stmtKind' <- discernStmtKind stmtKind
      body' <- discern nenv' body
      Tag.insertDD m functionName m
      forM_ expArgs' Tag.insertBinder
      return [WeakStmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' body']
    RawStmtDefineConst _ m (dd, _) (_, (t, _)) (_, (v, _)) -> do
      registerTopLevelName stmt
      t' <- discern empty t
      v' <- discern empty v
      Tag.insertDD m dd m
      return [WeakStmtDefineConst m dd t' v']
    RawStmtDefineData _ m (dd, _) args consInfo -> do
      stmtList <- defineData m dd args $ SE.extract consInfo
      discernStmtList stmtList
    RawStmtDefineResource _ m (name, _) _ (_, discarder) (_, copier) -> do
      registerTopLevelName stmt
      t' <- discern empty $ m :< RT.Tau
      e' <- discern empty $ m :< RT.Resource name [] discarder copier
      Tag.insertDD m name m
      return [WeakStmtDefineConst m name t' e']
    RawStmtDeclare _ m declList -> do
      registerTopLevelName stmt
      declList' <- mapM discernDecl $ SE.extract declList
      return [WeakStmtDeclare m declList']

discernDecl :: RT.TopDefHeader -> App (DE.Decl WT.WeakTerm)
discernDecl decl = do
  let impArgs = RT.extractArgs $ RT.impArgs decl
  let expArgs = RT.extractArgs $ RT.expArgs decl
  (impArgs', nenv) <- discernBinder empty impArgs
  (expArgs', nenv') <- discernBinder nenv expArgs
  forM_ (impArgs' ++ expArgs') $ \(_, x, _) -> UnusedVariable.delete x
  cod' <- discern nenv' $ snd $ RT.cod decl
  return $
    DE.Decl
      { loc = RT.loc decl,
        name = fst $ RT.name decl,
        isConstLike = RT.isConstLike decl,
        impArgs = impArgs',
        expArgs = expArgs',
        cod = cod'
      }

registerTopLevelName :: RawStmt -> App ()
registerTopLevelName stmt =
  case stmt of
    RawStmtDefine _ stmtKind (RT.RawDef {decl}) -> do
      let impArgs = RT.extractArgs $ RT.impArgs decl
      let expArgs = RT.extractArgs $ RT.expArgs decl
      let m = RT.loc decl
      let (functionName, _) = RT.name decl
      let isConstLike = RT.isConstLike decl
      let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
      let expArgNames = map (\(_, x, _, _, _) -> x) expArgs
      Global.registerStmtDefine isConstLike m stmtKind functionName allArgNum expArgNames
    RawStmtDefineConst _ m (dd, _) _ _ -> do
      Global.registerStmtDefine True m (SK.Normal O.Clear) dd AN.zero []
    RawStmtDeclare _ _ declList -> do
      mapM_ Global.registerDecl $ SE.extract declList
    RawStmtDefineData _ m (dd, _) args consInfo -> do
      stmtList <- defineData m dd args $ SE.extract consInfo
      mapM_ registerTopLevelName stmtList
    RawStmtDefineResource _ m (name, _) _ _ _ -> do
      Global.registerStmtDefine True m (SK.Normal O.Clear) name AN.zero []

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
    m :< RT.Pi impArgs expArgs _ t -> do
      (impArgs', nenv') <- discernBinder nenv $ RT.extractArgs impArgs
      (expArgs', nenv'') <- discernBinder nenv' $ RT.extractArgs expArgs
      t' <- discern nenv'' t
      forM_ (impArgs' ++ expArgs') $ \(_, x, _) -> UnusedVariable.delete x
      return $ m :< WT.Pi impArgs' expArgs' t'
    m :< RT.PiIntro impArgs expArgs _ e -> do
      lamID <- Gensym.newCount
      (impArgs', nenv') <- discernBinder nenv $ RT.extractArgs impArgs
      (expArgs', nenv'') <- discernBinder nenv' $ RT.extractArgs expArgs
      e' <- discern nenv'' e
      return $ m :< WT.PiIntro (AttrL.normal lamID) impArgs' expArgs' e'
    m :< RT.PiIntroFix _ (RT.RawDef {decl, body}) -> do
      let impArgs = RT.extractArgs $ RT.impArgs decl
      let expArgs = RT.extractArgs $ RT.expArgs decl
      let mx = RT.loc decl
      let (x, _) = RT.name decl
      (impArgs', nenv') <- discernBinder nenv impArgs
      (expArgs', nenv'') <- discernBinder nenv' expArgs
      codType' <- discern nenv'' $ snd $ RT.cod decl
      x' <- Gensym.newIdentFromText x
      nenv''' <- extendNominalEnv mx x' nenv''
      body' <- discern nenv''' body
      let mxt' = (mx, x', codType')
      Tag.insertBinder mxt'
      lamID <- Gensym.newCount
      return $ m :< WT.PiIntro (AttrL.Attr {lamKind = LK.Fix mxt', identity = lamID}) impArgs' expArgs' body'
    m :< RT.PiElim e _ es -> do
      es' <- mapM (discern nenv) $ SE.extract es
      e' <- discern nenv e
      return $ m :< WT.PiElim False e' es'
    m :< RT.PiElimByKey name _ kvs -> do
      (dd, _) <- resolveName m name
      let (ks, vs) = unzip $ map (\(_, k, _, _, v) -> (k, v)) $ SE.extract kvs
      ensureFieldLinearity m ks S.empty S.empty
      (argNum, keyList) <- KeyArg.lookup m dd
      vs' <- mapM (discern nenv) vs
      args <- KeyArg.reorderArgs m keyList $ Map.fromList $ zip ks vs'
      let isConstLike = False
      return $ m :< WT.PiElim False (m :< WT.VarGlobal (AttrVG.Attr {..}) dd) args
    m :< RT.PiElimExact _ e -> do
      e' <- discern nenv e
      return $ m :< WT.PiElimExact e'
    m :< RT.Data name consNameList es -> do
      es' <- mapM (discern nenv) es
      return $ m :< WT.Data name consNameList es'
    m :< RT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (discern nenv) dataArgs
      consArgs' <- mapM (discern nenv) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< RT.DataElim _ isNoetic es patternMatrix -> do
      let es' = SE.extract es
      let ms = map (\(me :< _) -> me) es'
      os <- mapM (const $ Gensym.newIdentFromText "match") es' -- os: occurrences
      es'' <- mapM (discern nenv >=> castFromNoemaIfNecessary isNoetic) es'
      ts <- mapM (const $ Gensym.newHole m []) es''
      patternMatrix' <- discernPatternMatrix nenv $ SE.extract patternMatrix
      ensurePatternMatrixSanity patternMatrix'
      let os' = zip ms os
      decisionTree <- compilePatternMatrix nenv isNoetic (V.fromList os') patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es'' ts) decisionTree
    m :< RT.Noema t -> do
      t' <- discern nenv t
      return $ m :< WT.Noema t'
    m :< RT.Embody e -> do
      e' <- discern nenv e
      return $ m :< WT.Embody (doNotCare m) e'
    m :< RT.Let letKind _ (mx, pat, c1, c2, (t, _)) mys _ e1@(m1 :< _) _ e2@(m2 :< _) -> do
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
                []
                False
                (SE.fromList'' [e1'])
                ( SE.fromList
                    SE.Brace
                    SE.Hyphen
                    [ ( SE.fromList'' [(_m, RP.Cons exceptFail [] (RP.Paren (SE.fromList' [(_m, RP.Var (Var err))])))],
                        [],
                        m2 :< RT.piElim exceptFailVar [m2 :< RT.Var (Var err)]
                      ),
                      ( SE.fromList'' [(_m, RP.Cons exceptPass [] (RP.Paren (SE.fromList' [(mx, pat)])))],
                        [],
                        e2
                      )
                    ]
                )
        RT.Bind -> do
          Throw.raiseError m "`bind` can only be used inside `with`"
        RT.Plain -> do
          (x, modifier) <- getContinuationModifier (mx, pat)
          discernLet nenv m (mx, x, c1, c2, t) (SE.extract mys) e1 (modifier False e2)
        RT.Noetic -> do
          (x, modifier) <- getContinuationModifier (mx, pat)
          discernLet nenv m (mx, x, c1, c2, t) (SE.extract mys) e1 (modifier True e2)
    m :< RT.Prim prim -> do
      prim' <- mapM (discern nenv) prim
      return $ m :< WT.Prim prim'
    m :< RT.Hole k ->
      return $ m :< WT.Hole k []
    m :< RT.Magic _ magic -> do
      magic' <- discernMagic nenv m magic
      return $ m :< WT.Magic magic'
    m :< RT.Annotation remarkLevel annot e -> do
      e' <- discern nenv e
      case annot of
        AN.Type _ ->
          return $ m :< WT.Annotation remarkLevel (AN.Type (doNotCare m)) e'
    m :< RT.Resource dd _ (discarder, _) (copier, _) -> do
      resourceID <- Gensym.newCount
      discarder' <- discern nenv discarder
      copier' <- discern nenv copier
      return $ m :< WT.Resource dd resourceID discarder' copier'
    m :< RT.Use _ e _ xs _ cont -> do
      e' <- discern nenv e
      (xs', nenv') <- discernBinder nenv $ RT.extractArgs xs
      cont' <- discern nenv' cont
      return $ m :< WT.Use e' xs' cont'
    m :< RT.If (_, (ifCond, _), _, (ifBody, _), _) elseIfList _ _ (elseBody, _) -> do
      boolTrue <- locatorToName (blur m) coreBoolTrue
      boolFalse <- locatorToName (blur m) coreBoolFalse
      discern nenv $ foldIf m boolTrue boolFalse ifCond ifBody elseIfList elseBody
    m :< RT.Seq (e1, _) _ e2 -> do
      h <- Gensym.newTextForHole
      unit <- locatorToVarGlobal m coreUnit
      discern nenv $ bind (m, h, [], [], unit) e1 e2
    m :< RT.When _ (whenCond, _) _ (whenBody, _) -> do
      boolTrue <- locatorToName (blur m) coreBoolTrue
      boolFalse <- locatorToName (blur m) coreBoolFalse
      unitUnit <- locatorToVarGlobal m coreUnitUnit
      discern nenv $ foldIf m boolTrue boolFalse whenCond whenBody [] unitUnit
    m :< RT.ListIntro es -> do
      listNil <- locatorToVarGlobal m coreListNil
      listCons <- locatorToVarGlobal m coreListCons
      discern nenv $ foldListApp m listNil listCons $ SE.extract es
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
    m :< RT.Detach _ _ (e, _) -> do
      t <- Gensym.newPreHole (blur m)
      detachVar <- locatorToVarGlobal m coreThreadDetach
      discern nenv $ m :< RT.piElim detachVar [t, RT.lam m [] e]
    m :< RT.Attach _ _ (e, _) -> do
      t <- Gensym.newPreHole (blur m)
      attachVar <- locatorToVarGlobal m coreThreadAttach
      discern nenv $ m :< RT.piElim attachVar [t, e]
    m :< RT.Option _ t -> do
      exceptVar <- locatorToVarGlobal m coreExcept
      unit <- locatorToVarGlobal m coreUnit
      discern nenv $ m :< RT.piElim exceptVar [unit, t]
    m :< RT.Assert _ (mText, message) _ _ (e@(mCond :< _), _) -> do
      assert <- locatorToVarGlobal m coreSystemAssert
      textType <- locatorToVarGlobal m coreText
      let fullMessage = T.pack (Hint.toString m) <> "\nassertion failure: " <> message <> "\n"
      discern nenv $
        m
          :< RT.piElim
            assert
            [mText :< RT.Prim (WP.Value (WPV.StaticText textType fullMessage)), RT.lam mCond [] e]
    m :< RT.Introspect _ key _ clauseList -> do
      value <- getIntrospectiveValue m key
      clause <- lookupIntrospectiveClause m value $ SE.extract clauseList
      discern nenv clause
    m :< RT.With _ binder _ _ (body, _) -> do
      case body of
        mLet :< RT.Let letKind c1 mxt@(mPat, pat, c2, c3, (t, c)) mys c4 e1 c5 e2 -> do
          let e1' = m :< RT.With [] binder [] [] (e1, [])
          let e2' = m :< RT.With [] binder [] [] (e2, [])
          case letKind of
            RT.Bind -> do
              (x, modifier) <- getContinuationModifier (mPat, pat)
              discern nenv $ m :< RT.piElim binder [e1', RT.lam m [((mPat, x, c2, c3, t), c)] (modifier False e2')]
            _ -> do
              discern nenv $ mLet :< RT.Let letKind c1 mxt mys c4 e1' c5 e2'
        mSeq :< RT.Seq (e1, c1) c2 e2 -> do
          let e1' = m :< RT.With [] binder [] [] (e1, [])
          let e2' = m :< RT.With [] binder [] [] (e2, [])
          discern nenv $ mSeq :< RT.Seq (e1', c1) c2 e2'
        mUse :< RT.Use c1 item c2 vars c3 cont -> do
          let cont' = m :< RT.With [] binder [] [] (cont, [])
          discern nenv $ mUse :< RT.Use c1 item c2 vars c3 cont'
        _ ->
          discern nenv body
    _ :< RT.Brace _ (e, _) ->
      discern nenv e

discernMagic :: NominalEnv -> Hint -> RT.RawMagic -> App (M.Magic WT.WeakTerm)
discernMagic nenv m magic =
  case magic of
    RT.Cast _ (_, (from, _)) (_, (to, _)) (_, (e, _)) -> do
      from' <- discern nenv from
      to' <- discern nenv to
      e' <- discern nenv e
      return $ M.Cast from' to' e'
    RT.Store _ (_, (lt, _)) (_, (value, _)) (_, (pointer, _)) -> do
      value' <- discern nenv value
      pointer' <- discern nenv pointer
      return $ M.Store lt value' pointer'
    RT.Load _ (_, (lt, _)) (_, (pointer, _)) -> do
      pointer' <- discern nenv pointer
      return $ M.Load lt pointer'
    RT.External _ (_, (funcName, _)) args varArgs -> do
      (domList, cod) <- Decl.lookupDeclEnv m (DN.Ext funcName)
      args' <- mapM (discern nenv . fst . snd) args
      varArgs' <- forM varArgs $ \(_, ((arg, _), (lt, _))) -> do
        arg' <- discern nenv arg
        return (arg', lt)
      return $ M.External domList cod funcName args' varArgs'
    RT.Global _ _ (name, _) _ (lt, _) -> do
      return $ M.Global name lt

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
            mCont
              :< RT.DataElim
                []
                isNoetic
                (SE.fromList'' [mCont :< RT.Var (Var tmp)])
                (SE.fromList SE.Brace SE.Hyphen [(SE.fromList'' [pat], [], cont)])
        )

ascribe :: Hint -> RT.RawTerm -> RT.RawTerm -> App RT.RawTerm
ascribe m t e = do
  tmp <- Gensym.newTextForHole
  return $ bind (m, tmp, [], [], t) e (m :< RT.Var (Var tmp))

bind :: RawBinder RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind (m, x, c1, c2, t) e cont =
  m
    :< RT.Let
      RT.Plain
      []
      (m, RP.Var (Var x), c1, c2, (t, []))
      (SE.emptySeries' Nothing SE.Comma)
      []
      e
      []
      cont

foldListApp :: Hint -> RT.RawTerm -> RT.RawTerm -> [RT.RawTerm] -> RT.RawTerm
foldListApp m listNil listCons es =
  case es of
    [] ->
      listNil
    e : rest ->
      m :< RT.piElim listCons [e, foldListApp m listNil listCons rest]

lookupIntrospectiveClause :: Hint -> T.Text -> [(Maybe (T.Text, C), C, RT.RawTerm)] -> App RT.RawTerm
lookupIntrospectiveClause m value clauseList =
  case clauseList of
    [] ->
      Throw.raiseError m $ "this term doesn't support `" <> value <> "`."
    (Just (key, _), _, clause) : rest
      | key == value ->
          return clause
      | otherwise ->
          lookupIntrospectiveClause m value rest
    (Nothing, _, clause) : _ ->
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
  [RT.IfClause RT.RawTerm] ->
  RT.RawTerm ->
  RT.RawTerm
foldIf m true false ifCond ifBody elseIfList elseBody =
  case elseIfList of
    [] -> do
      m
        :< RT.DataElim
          []
          False
          (SE.fromList'' [ifCond])
          ( SE.fromList
              SE.Brace
              SE.Hyphen
              [ ( SE.fromList'' [(blur m, RP.Var true)],
                  [],
                  ifBody
                ),
                ( SE.fromList'' [(blur m, RP.Var false)],
                  [],
                  elseBody
                )
              ]
          )
    ((_, (elseIfCond, _), _, (elseIfBody, _), _) : rest) -> do
      let cont = foldIf m true false elseIfCond elseIfBody rest elseBody
      m
        :< RT.DataElim
          []
          False
          (SE.fromList'' [ifCond])
          ( SE.fromList
              SE.Brace
              SE.Hyphen
              [ (SE.fromList'' [(blur m, RP.Var true)], [], ifBody),
                (SE.fromList'' [(blur m, RP.Var false)], [], cont)
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
  [RP.RawPatternRow RT.RawTerm] ->
  App (PAT.PatternMatrix ([Ident], WT.WeakTerm))
discernPatternMatrix nenv patternMatrix =
  case uncons patternMatrix of
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
discernPatternRow nenv (patList, _, body) = do
  (patList', body') <- discernPatternRow' nenv (SE.extract patList) [] body
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
    RP.Cons cons _ mArgs -> do
      (consName, dataArgNum, consArgNum, disc, isConstLike, _) <- resolveConstructor m cons
      when isConstLike $
        Throw.raiseError m $
          "the constructor `" <> showName cons <> "` can't have any arguments"
      case mArgs of
        RP.Paren args -> do
          (args', nenvList) <- mapAndUnzipM discernPattern $ SE.extract args
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
          let (ks, mvcs) = unzip $ SE.extract mkvs
          let mvs = map (\(mv, _, v) -> (mv, v)) mvcs
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
      discernPattern $ foldListAppPat m listNil listCons $ SE.extract patList

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
    pat : rest -> do
      let rest' = foldListAppPat m listNil listCons rest
      (m, RP.Cons listCons [] (RP.Paren (SE.fromList' [pat, rest'])))

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
