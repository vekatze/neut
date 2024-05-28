module Scene.Parse.Discern (discernStmtList) where

import Codec.Binary.UTF8.String
import Context.App
import Context.Decl qualified as Decl
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.Global qualified as Global
import Context.KeyArg qualified as KeyArg
import Context.Locator qualified as Locator
import Context.SymLoc qualified as SymLoc
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Context.TopCandidate qualified as TopCandidate
import Context.UnusedVariable qualified as UnusedVariable
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as B
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.List
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Vector qualified as V
import Data.Word
import Entity.Annotation qualified as AN
import Entity.Arch qualified as Arch
import Entity.ArgNum qualified as AN
import Entity.Attr.Lam qualified as AttrL
import Entity.Attr.VarGlobal qualified as AttrVG
import Entity.BaseName qualified as BN
import Entity.Binder
import Entity.BuildMode qualified as BM
import Entity.C
import Entity.Const
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.Error qualified as E
import Entity.Geist qualified as G
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Hint.Reify qualified as Hint
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.Key
import Entity.LamKind qualified as LK
import Entity.Locator qualified as L
import Entity.LowType qualified as LT
import Entity.LowType.FromRawLowType qualified as LT
import Entity.Magic qualified as M
import Entity.Module
import Entity.Name
import Entity.Noema qualified as N
import Entity.NominalEnv
import Entity.OS qualified as OS
import Entity.Opacity qualified as O
import Entity.Pattern qualified as PAT
import Entity.Platform qualified as Platform
import Entity.RawBinder
import Entity.RawIdent hiding (isHole)
import Entity.RawLowType qualified as RLT
import Entity.RawPattern qualified as RP
import Entity.RawProgram
import Entity.RawTerm qualified as RT
import Entity.Remark qualified as R
import Entity.Stmt
import Entity.StmtKind qualified as SK
import Entity.Syntax.Series qualified as SE
import Entity.TopCandidate
import Entity.VarDefKind qualified as VDK
import Entity.WeakPrim qualified as WP
import Entity.WeakPrimValue qualified as WPV
import Entity.WeakTerm qualified as WT
import Path
import Path.IO (doesFileExist)
import Scene.Parse.Discern.Data
import Scene.Parse.Discern.Name
import Scene.Parse.Discern.Noema
import Scene.Parse.Discern.NominalEnv
import Scene.Parse.Discern.PatternMatrix
import Scene.Parse.Discern.Struct
import Scene.Parse.Foreign
import Scene.Parse.Util
import Text.Read qualified as R

discernStmtList :: Module -> [RawStmt] -> App [WeakStmt]
discernStmtList mo =
  fmap concat . mapM (discernStmt mo)

discernStmt :: Module -> RawStmt -> App [WeakStmt]
discernStmt mo stmt = do
  nameLifter <- Locator.getNameLifter
  case stmt of
    RawStmtDefine _ stmtKind (RT.RawDef {geist, body, endLoc}) -> do
      registerTopLevelName nameLifter stmt
      let impArgs = RT.extractArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let (_, codType) = RT.cod geist
      let m = RT.loc geist
      let functionName = nameLifter $ fst $ RT.name geist
      let isConstLike = RT.isConstLike geist
      (impArgs', nenv) <- discernBinder (emptyAxis mo) impArgs endLoc
      (expArgs', nenv') <- discernBinder nenv expArgs endLoc
      codType' <- discern nenv' codType
      stmtKind' <- discernStmtKind (emptyAxis mo) stmtKind
      body' <- discern nenv' body
      Tag.insertGlobalVar m functionName isConstLike m
      TopCandidate.insert $ TopCandidate {loc = metaLocation m, dd = functionName, kind = toCandidateKind stmtKind'}
      forM_ expArgs' Tag.insertBinder
      return [WeakStmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' body']
    RawStmtDefineConst _ m (name, _) (_, (t, _)) (_, (v, _)) -> do
      let dd = nameLifter name
      registerTopLevelName nameLifter stmt
      t' <- discern (emptyAxis mo) t
      v' <- discern (emptyAxis mo) v
      Tag.insertGlobalVar m dd True m
      TopCandidate.insert $ TopCandidate {loc = metaLocation m, dd = dd, kind = Constant}
      return [WeakStmtDefineConst m dd t' v']
    RawStmtDefineData _ m (dd, _) args consInfo loc -> do
      stmtList <- defineData m dd args (SE.extract consInfo) loc
      discernStmtList mo stmtList
    RawStmtDefineResource _ m (name, _) (_, discarder) (_, copier) _ -> do
      let dd = nameLifter name
      registerTopLevelName nameLifter stmt
      t' <- discern (emptyAxis mo) $ m :< RT.Tau
      e' <- discern (emptyAxis mo) $ m :< RT.Resource [] (discarder, []) (copier, [])
      Tag.insertGlobalVar m dd True m
      TopCandidate.insert $ TopCandidate {loc = metaLocation m, dd = dd, kind = Constant}
      return [WeakStmtDefineConst m dd t' e']
    RawStmtNominal _ m geistList -> do
      geistList' <- forM (SE.extract geistList) $ \(geist, endLoc) -> do
        Global.registerGeist geist
        discernGeist mo endLoc geist
      return [WeakStmtNominal m geistList']
    RawStmtForeign _ foreignList -> do
      foreign' <- interpretForeign foreignList
      activateForeign foreign'
      return [WeakStmtForeign foreign']

discernGeist :: Module -> Loc -> RT.TopGeist -> App (G.Geist WT.WeakTerm)
discernGeist mo endLoc geist = do
  nameLifter <- Locator.getNameLifter
  let impArgs = RT.extractArgs $ RT.impArgs geist
  let expArgs = RT.extractArgs $ RT.expArgs geist
  (impArgs', axis) <- discernBinder (emptyAxis mo) impArgs endLoc
  (expArgs', axis') <- discernBinder axis expArgs endLoc
  forM_ (impArgs' ++ expArgs') $ \(_, x, _) -> UnusedVariable.delete x
  cod' <- discern axis' $ snd $ RT.cod geist
  let m = RT.loc geist
  let dd = nameLifter $ fst $ RT.name geist
  let kind = if RT.isConstLike geist then Constant else Function
  TopCandidate.insert $ TopCandidate {loc = metaLocation m, dd = dd, kind = kind}
  return $
    G.Geist
      { loc = m,
        name = dd,
        isConstLike = RT.isConstLike geist,
        impArgs = impArgs',
        expArgs = expArgs',
        cod = cod'
      }

registerTopLevelName :: (BN.BaseName -> DD.DefiniteDescription) -> RawStmt -> App ()
registerTopLevelName nameLifter stmt =
  case stmt of
    RawStmtDefine _ stmtKind (RT.RawDef {geist}) -> do
      let impArgs = RT.extractArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let m = RT.loc geist
      let functionName = nameLifter $ fst $ RT.name geist
      let isConstLike = RT.isConstLike geist
      let allArgNum = AN.fromInt $ length $ impArgs ++ expArgs
      let expArgNames = map (\(_, x, _, _, _) -> x) expArgs
      stmtKind' <- liftStmtKind stmtKind
      Global.registerStmtDefine isConstLike m stmtKind' functionName allArgNum expArgNames
    RawStmtDefineConst _ m (name, _) _ _ -> do
      Global.registerStmtDefine True m (SK.Normal O.Clear) (nameLifter name) AN.zero []
    RawStmtNominal {} -> do
      return ()
    RawStmtDefineData _ m (dd, _) args consInfo loc -> do
      stmtList <- defineData m dd args (SE.extract consInfo) loc
      mapM_ (registerTopLevelName nameLifter) stmtList
    RawStmtDefineResource _ m (name, _) _ _ _ -> do
      Global.registerStmtDefine True m (SK.Normal O.Clear) (nameLifter name) AN.zero []
    RawStmtForeign {} ->
      return ()

liftStmtKind :: SK.RawStmtKind BN.BaseName -> App (SK.RawStmtKind DD.DefiniteDescription)
liftStmtKind stmtKind = do
  case stmtKind of
    SK.Normal opacity ->
      return $ SK.Normal opacity
    SK.Data dataName dataArgs consInfoList -> do
      nameLifter <- Locator.getNameLifter
      let (locList, consNameList, isConstLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      let consNameList' = map nameLifter consNameList
      let consInfoList' = zip5 locList consNameList' isConstLikeList consArgsList discriminantList
      return $ SK.Data (nameLifter dataName) dataArgs consInfoList'
    SK.DataIntro dataName dataArgs consArgs discriminant -> do
      nameLifter <- Locator.getNameLifter
      return $ SK.DataIntro (nameLifter dataName) dataArgs consArgs discriminant

discernStmtKind :: Axis -> SK.RawStmtKind BN.BaseName -> App (SK.StmtKind WT.WeakTerm)
discernStmtKind ax stmtKind =
  case stmtKind of
    SK.Normal opacity ->
      return $ SK.Normal opacity
    SK.Data dataName dataArgs consInfoList -> do
      nameLifter <- Locator.getNameLifter
      (dataArgs', axis) <- discernBinder' ax dataArgs
      let (locList, consNameList, isConstLikeList, consArgsList, discriminantList) = unzip5 consInfoList
      (consArgsList', axisList) <- mapAndUnzipM (discernBinder' axis) consArgsList
      forM_ (concatMap _nenv axisList) $ \(_, (_, newVar)) -> do
        UnusedVariable.delete newVar
      let consNameList' = map nameLifter consNameList
      let consInfoList' = zip5 locList consNameList' isConstLikeList consArgsList' discriminantList
      return $ SK.Data (nameLifter dataName) dataArgs' consInfoList'
    SK.DataIntro dataName dataArgs consArgs discriminant -> do
      nameLifter <- Locator.getNameLifter
      (dataArgs', axis) <- discernBinder' ax dataArgs
      (consArgs', axis') <- discernBinder' axis consArgs
      forM_ (_nenv axis') $ \(_, (_, newVar)) -> do
        UnusedVariable.delete newVar
      return $ SK.DataIntro (nameLifter dataName) dataArgs' consArgs' discriminant

toCandidateKind :: SK.StmtKind a -> CandidateKind
toCandidateKind stmtKind =
  case stmtKind of
    SK.Normal {} ->
      Function
    SK.Data {} ->
      Function
    SK.DataIntro {} ->
      Constructor

discern :: Axis -> RT.RawTerm -> App WT.WeakTerm
discern axis term =
  case term of
    m :< RT.Tau ->
      return $ m :< WT.Tau
    m :< RT.Var name ->
      case name of
        Var s
          | Just x <- R.readMaybe (T.unpack s) -> do
              h <- Gensym.newHole m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int h x)
          | Just x <- readIntBinaryMaybe s -> do
              h <- Gensym.newHole m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int h x)
          | Just x <- R.readMaybe (T.unpack s) -> do
              h <- Gensym.newHole m []
              return $ m :< WT.Prim (WP.Value $ WPV.Float h x)
          | Just (mDef, name') <- lookup s (_nenv axis) -> do
              UnusedVariable.delete name'
              Tag.insertLocalVar m name' mDef
              return $ m :< WT.Var name'
        _ -> do
          (dd, (_, gn)) <- resolveName m name
          interpretGlobalName m dd gn
    m :< RT.Pi impArgs expArgs _ t endLoc -> do
      (impArgs', axis') <- discernBinder axis (RT.extractArgs impArgs) endLoc
      (expArgs', axis'') <- discernBinder axis' (RT.extractArgs expArgs) endLoc
      t' <- discern axis'' t
      forM_ (impArgs' ++ expArgs') $ \(_, x, _) -> UnusedVariable.delete x
      return $ m :< WT.Pi impArgs' expArgs' t'
    m :< RT.PiIntro _ (RT.RawDef {geist, body, endLoc}) -> do
      lamID <- Gensym.newCount
      let impArgs = RT.extractArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      (impArgs', axis') <- discernBinder axis impArgs endLoc
      (expArgs', axis'') <- discernBinder axis' expArgs endLoc
      codType' <- discern axis'' $ snd $ RT.cod geist
      body' <- discern axis'' body
      return $ m :< WT.PiIntro (AttrL.normal lamID codType') impArgs' expArgs' body'
    m :< RT.PiIntroFix _ (RT.RawDef {geist, body, endLoc}) -> do
      let impArgs = RT.extractArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let mx = RT.loc geist
      let (x, _) = RT.name geist
      (impArgs', axis') <- discernBinder axis impArgs endLoc
      (expArgs', axis'') <- discernBinder axis' expArgs endLoc
      codType' <- discern axis'' $ snd $ RT.cod geist
      x' <- Gensym.newIdentFromText x
      axis''' <- extendAxis mx x' VDK.Normal axis''
      body' <- discern axis''' body
      let mxt' = (mx, x', codType')
      Tag.insertBinder mxt'
      lamID <- Gensym.newCount
      return $ m :< WT.PiIntro (AttrL.Attr {lamKind = LK.Fix mxt', identity = lamID}) impArgs' expArgs' body'
    m :< RT.PiElim e _ es -> do
      case e of
        _ :< RT.Var (Var c)
          | c == "new-cell",
            [arg] <- SE.extract es -> do
              newCellDD <- locatorToVarGlobal m coreCellNewCell
              e' <- discern axis $ m :< RT.piElim newCellDD [arg]
              return $ m :< WT.Actual e'
          | c == "new-channel",
            [] <- SE.extract es -> do
              newChannelDD <- locatorToVarGlobal m coreChannelNewChannel
              e' <- discern axis $ m :< RT.piElim newChannelDD []
              return $ m :< WT.Actual e'
        _ -> do
          es' <- mapM (discern axis) $ SE.extract es
          e' <- discern axis e
          return $ m :< WT.PiElim e' es'
    m :< RT.PiElimByKey name _ kvs -> do
      (dd, _) <- resolveName m name
      let (ks, vs) = unzip $ map (\(_, k, _, _, v) -> (k, v)) $ SE.extract kvs
      ensureFieldLinearity m ks S.empty S.empty
      (argNum, keyList) <- KeyArg.lookup m dd
      vs' <- mapM (discern axis) vs
      args <- KeyArg.reorderArgs m keyList $ Map.fromList $ zip ks vs'
      let isConstLike = False
      return $ m :< WT.PiElim (m :< WT.VarGlobal (AttrVG.Attr {..}) dd) args
    m :< RT.PiElimExact _ e -> do
      e' <- discern axis e
      return $ m :< WT.PiElimExact e'
    m :< RT.Data attr dataName es -> do
      nameLifter <- Locator.getNameLifter
      dataName' <- Locator.attachCurrentLocator dataName
      es' <- mapM (discern axis) es
      return $ m :< WT.Data (fmap nameLifter attr) dataName' es'
    m :< RT.DataIntro attr consName dataArgs consArgs -> do
      nameLifter <- Locator.getNameLifter
      dataArgs' <- mapM (discern axis) dataArgs
      consArgs' <- mapM (discern axis) consArgs
      return $ m :< WT.DataIntro (fmap nameLifter attr) (nameLifter consName) dataArgs' consArgs'
    m :< RT.DataElim _ isNoetic es patternMatrix -> do
      let es' = SE.extract es
      let ms = map (\(me :< _) -> me) es'
      os <- mapM (const $ Gensym.newIdentFromText "match") es' -- os: occurrences
      es'' <- mapM (discern axis >=> castFromNoemaIfNecessary isNoetic) es'
      ts <- mapM (const $ Gensym.newHole m []) es''
      patternMatrix' <- discernPatternMatrix axis $ SE.extract patternMatrix
      ensurePatternMatrixSanity patternMatrix'
      let os' = zip ms os
      decisionTree <- compilePatternMatrix (_nenv axis) isNoetic (V.fromList os') patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es'' ts) decisionTree
    m :< RT.Noema t -> do
      t' <- discern axis t
      return $ m :< WT.Noema t'
    m :< RT.Embody e -> do
      e' <- discern axis e
      return $ m :< WT.Embody (doNotCare m) e'
    m :< RT.Let letKind _ (mx, pat, c1, c2, t) _ mys _ e1 _ startLoc _ e2@(m2 :< _) endLoc -> do
      case letKind of
        RT.Try -> do
          let mx' = blur mx
          let m2' = blur m2
          eitherTypeInner <- locatorToVarGlobal mx' coreExcept
          leftType <- Gensym.newPreHole m2'
          let eitherType = m2' :< RT.piElim eitherTypeInner [leftType, t]
          e1' <- ascribe m2' eitherType e1
          err <- Gensym.newText
          exceptFail <- locatorToName m2' coreExceptFail
          exceptPass <- locatorToName m2' coreExceptPass
          exceptFailVar <- locatorToVarGlobal mx' coreExceptFail
          discern axis $
            m
              :< RT.DataElim
                []
                False
                (SE.fromList'' [e1'])
                ( SE.fromList
                    SE.Brace
                    SE.Bar
                    [ ( SE.fromList'' [(m2', RP.Cons exceptFail [] (RP.Paren (SE.fromList' [(m2', RP.Var (Var err))])))],
                        [],
                        m2' :< RT.piElim exceptFailVar [m2' :< RT.Var (Var err)],
                        fakeLoc
                      ),
                      ( SE.fromList'' [(m2', RP.Cons exceptPass [] (RP.Paren (SE.fromList' [(mx, pat)])))],
                        [],
                        e2,
                        endLoc
                      )
                    ]
                )
        RT.Bind -> do
          Throw.raiseError m "`bind` can only be used inside `with`"
        RT.Plain -> do
          (x, modifier) <- getContinuationModifier (mx, pat) endLoc
          discernLet axis m (mx, x, c1, c2, t) (SE.extract mys) e1 (modifier False e2) startLoc endLoc
        RT.Noetic -> do
          (x, modifier) <- getContinuationModifier (mx, pat) endLoc
          discernLet axis m (mx, x, c1, c2, t) (SE.extract mys) e1 (modifier True e2) startLoc endLoc
    m :< RT.StaticText s str -> do
      let strOrNone = R.readMaybe (T.unpack $ "\"" <> str <> "\"")
      s' <- discern axis s
      case strOrNone of
        Nothing ->
          Throw.raiseError m $ "couldn't interpret the following as a string: " <> str
        Just str' -> do
          return $ m :< WT.Prim (WP.Value $ WPV.StaticText s' str')
    m :< RT.Rune runeType str -> do
      let strOrNone = R.readMaybe (T.unpack $ "\"" <> str <> "\"")
      runeType' <- discern axis runeType
      case strOrNone of
        Just str'
          | Just (c, "") <- T.uncons str' -> do
              let runeValue = calculateRuneValue $ encode [c]
              return $ m :< WT.Prim (WP.Value $ WPV.Int runeType' runeValue)
        _ ->
          Throw.raiseError m $ "couldn't interpret the following as a rune: " <> str
    m :< RT.Hole k ->
      return $ m :< WT.Hole k []
    m :< RT.Magic _ magic -> do
      magic' <- discernMagic axis m magic
      return $ m :< WT.Magic magic'
    m :< RT.Annotation remarkLevel annot e -> do
      e' <- discern axis e
      case annot of
        AN.Type _ ->
          return $ m :< WT.Annotation remarkLevel (AN.Type (doNotCare m)) e'
    m :< RT.Resource _ (discarder, _) (copier, _) -> do
      resourceID <- Gensym.newCount
      discarder' <- discern axis discarder
      copier' <- discern axis copier
      return $ m :< WT.Resource resourceID discarder' copier'
    m :< RT.Use _ e _ xs _ cont endLoc -> do
      e' <- discern axis e
      (xs', axis') <- discernBinder axis (RT.extractArgs xs) endLoc
      cont' <- discern axis' cont
      return $ m :< WT.Use e' xs' cont'
    m :< RT.If ifClause elseIfClauseList (_, (elseBody, _)) -> do
      let (ifCond, ifBody) = RT.extractFromKeywordClause ifClause
      boolTrue <- locatorToName (blur m) coreBoolTrue
      boolFalse <- locatorToName (blur m) coreBoolFalse
      discern axis $ foldIf m boolTrue boolFalse ifCond ifBody elseIfClauseList elseBody
    m :< RT.Seq (e1, _) _ e2 -> do
      h <- Gensym.newTextForHole
      unit <- locatorToVarGlobal m coreUnit
      discern axis $ bind fakeLoc fakeLoc (m, h, [], [], unit) e1 e2
    m :< RT.When whenClause -> do
      let (whenCond, whenBody) = RT.extractFromKeywordClause whenClause
      boolTrue <- locatorToName (blur m) coreBoolTrue
      boolFalse <- locatorToName (blur m) coreBoolFalse
      unitUnit <- locatorToVarGlobal m coreUnitUnit
      discern axis $ foldIf m boolTrue boolFalse whenCond whenBody [] unitUnit
    m :< RT.ListIntro es -> do
      listNil <- locatorToVarGlobal m coreListNil
      listCons <- locatorToVarGlobal m coreListCons
      discern axis $ foldListApp m listNil listCons $ SE.extract es
    m :< RT.Admit -> do
      admit <- locatorToVarGlobal m coreSystemAdmit
      t <- Gensym.newPreHole (blur m)
      textType <- locatorToVarGlobal m coreText
      discern axis $
        m
          :< RT.Annotation
            R.Warning
            (AN.Type ())
            ( m
                :< RT.piElim
                  admit
                  [t, m :< RT.StaticText textType ("admit: " <> T.pack (Hint.toString m) <> "\n")]
            )
    m :< RT.Detach _ _ (e, _) -> do
      t <- Gensym.newPreHole (blur m)
      detachVar <- locatorToVarGlobal m coreThreadDetach
      cod <- Gensym.newPreHole (blur m)
      discern axis $ m :< RT.piElim detachVar [t, RT.lam fakeLoc m [] cod e]
    m :< RT.Attach _ _ (e, _) -> do
      t <- Gensym.newPreHole (blur m)
      attachVar <- locatorToVarGlobal m coreThreadAttach
      discern axis $ m :< RT.piElim attachVar [t, e]
    m :< RT.Option t -> do
      exceptVar <- locatorToVarGlobal m coreExcept
      unit <- locatorToVarGlobal m coreUnit
      discern axis $ m :< RT.piElim exceptVar [unit, t]
    m :< RT.Assert _ (mText, message) _ _ (e@(mCond :< _), _) -> do
      assert <- locatorToVarGlobal m coreSystemAssert
      textType <- locatorToVarGlobal m coreText
      let fullMessage = T.pack (Hint.toString m) <> "\nassertion failure: " <> message <> "\n"
      cod <- Gensym.newPreHole (blur m)
      discern axis $
        m
          :< RT.piElim
            assert
            [mText :< RT.StaticText textType fullMessage, RT.lam fakeLoc mCond [] cod e]
    m :< RT.Introspect _ key _ clauseList -> do
      value <- getIntrospectiveValue m key
      clause <- lookupIntrospectiveClause m value $ SE.extract clauseList
      discern axis clause
    m :< RT.IncludeText _ _ (path, _) -> do
      case parseRelFile (T.unpack path) of
        Just path' -> do
          let dir = getModuleRootDir $ currentModule axis
          let filePath = dir </> path'
          b <- doesFileExist filePath
          if b
            then do
              content <- liftIO $ fmap decodeUtf8 $ B.readFile $ toFilePath filePath
              textType <- locatorToVarGlobal m coreText >>= discern axis
              Tag.insertFileLoc m (T.length "include-text") (newSourceHint filePath)
              return $ m :< WT.Prim (WP.Value $ WPV.StaticText textType content)
            else do
              Throw.raiseError m $ "No such file exist: `" <> T.pack (toFilePath filePath) <> "`"
        Nothing ->
          Throw.raiseError m $ "couldn't parse the relative path: `" <> path <> "`"
    m :< RT.With withClause -> do
      let (binder, body) = RT.extractFromKeywordClause withClause
      case body of
        mLet :< RT.Let letKind c1 mxt@(mPat, pat, c2, c3, t) c mys c4 e1 c5 loc c6 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          case letKind of
            RT.Bind -> do
              (x, modifier) <- getContinuationModifier (mPat, pat) endLoc
              cod <- Gensym.newPreHole (blur m)
              discern axis $ m :< RT.piElim binder [e1', RT.lam loc m [((mPat, x, c2, c3, t), c)] cod (modifier False e2')]
            _ -> do
              discern axis $ mLet :< RT.Let letKind c1 mxt c mys c4 e1' c5 loc c6 e2' endLoc
        mSeq :< RT.Seq (e1, c1) c2 e2 -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern axis $ mSeq :< RT.Seq (e1', c1) c2 e2'
        mUse :< RT.Use c1 item c2 vars c3 cont endLoc -> do
          let cont' = m :< RT.With (([], (binder, [])), ([], (cont, [])))
          discern axis $ mUse :< RT.Use c1 item c2 vars c3 cont' endLoc
        _ ->
          discern axis body
    _ :< RT.Projection e (mProj, proj) loc -> do
      t <- Gensym.newPreHole (blur mProj)
      let args = (SE.fromList SE.Brace SE.Comma [(mProj, proj, [], [], t)], [])
      let var = mProj :< RT.Var (Var proj)
      discern axis $ mProj :< RT.Use [] e [] args [] var loc
    _ :< RT.Brace _ (e, _) ->
      discern axis e

discernRawLowType :: Hint -> RLT.RawLowType -> App LT.LowType
discernRawLowType m rlt = do
  dataSize <- Env.getDataSize m
  case LT.fromRawLowType dataSize rlt of
    Left err ->
      Throw.raiseError m err
    Right lt ->
      return lt

discernMagic :: Axis -> Hint -> RT.RawMagic -> App (M.Magic WT.WeakTerm)
discernMagic axis m magic =
  case magic of
    RT.Cast _ (_, (from, _)) (_, (to, _)) (_, (e, _)) -> do
      from' <- discern axis from
      to' <- discern axis to
      e' <- discern axis e
      return $ M.Cast from' to' e'
    RT.Store _ (_, (lt, _)) (_, (value, _)) (_, (pointer, _)) -> do
      lt' <- discernRawLowType m lt
      value' <- discern axis value
      pointer' <- discern axis pointer
      return $ M.Store lt' value' pointer'
    RT.Load _ (_, (lt, _)) (_, (pointer, _)) -> do
      lt' <- discernRawLowType m lt
      pointer' <- discern axis pointer
      return $ M.Load lt' pointer'
    RT.Alloca _ (_, (lt, _)) (_, (size, _)) -> do
      lt' <- discernRawLowType m lt
      size' <- discern axis size
      return $ M.Alloca lt' size'
    RT.External _ funcName _ args varArgsOrNone -> do
      (domList, cod) <- Decl.lookupDeclEnv m (DN.Ext funcName)
      args' <- mapM (discern axis) $ SE.extract args
      varArgs' <- case varArgsOrNone of
        Nothing ->
          return []
        Just (_, varArgs) ->
          forM (SE.extract varArgs) $ \(_, arg, _, _, lt) -> do
            arg' <- discern axis arg
            lt' <- discernRawLowType m lt
            return (arg', lt')
      return $ M.External domList cod funcName args' varArgs'
    RT.Global _ (_, (name, _)) (_, (lt, _)) -> do
      lt' <- discernRawLowType m lt
      return $ M.Global name lt'

getContinuationModifier :: (Hint, RP.RawPattern) -> Loc -> App (RawIdent, N.IsNoetic -> RT.RawTerm -> RT.RawTerm)
getContinuationModifier pat endLoc =
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
                (SE.fromList SE.Brace SE.Bar [(SE.fromList'' [pat], [], cont, endLoc)])
        )

ascribe :: Hint -> RT.RawTerm -> RT.RawTerm -> App RT.RawTerm
ascribe m t e = do
  tmp <- Gensym.newTextForHole
  return $ bind fakeLoc fakeLoc (m, tmp, [], [], t) e (m :< RT.Var (Var tmp))

bind :: Loc -> Loc -> RawBinder RT.RawTerm -> RT.RawTerm -> RT.RawTerm -> RT.RawTerm
bind loc endLoc (m, x, c1, c2, t) e cont =
  m
    :< RT.Let
      RT.Plain
      []
      (m, RP.Var (Var x), c1, c2, t)
      []
      (SE.emptySeries' Nothing SE.Comma)
      []
      e
      []
      loc
      []
      cont
      endLoc

foldListApp :: Hint -> RT.RawTerm -> RT.RawTerm -> [RT.RawTerm] -> RT.RawTerm
foldListApp m listNil listCons es =
  case es of
    [] ->
      listNil
    e : rest ->
      m :< RT.piElim listCons [e, foldListApp m listNil listCons rest]

lookupIntrospectiveClause :: Hint -> T.Text -> [(Maybe T.Text, C, RT.RawTerm)] -> App RT.RawTerm
lookupIntrospectiveClause m value clauseList =
  case clauseList of
    [] ->
      Throw.raiseError m $ "this term doesn't support `" <> value <> "`."
    (Just key, _, clause) : rest
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
  [RT.KeywordClause RT.RawTerm] ->
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
              SE.Bar
              [ ( SE.fromList'' [(blur m, RP.Var true)],
                  [],
                  ifBody,
                  fakeLoc
                ),
                ( SE.fromList'' [(blur m, RP.Var false)],
                  [],
                  elseBody,
                  fakeLoc
                )
              ]
          )
    elseIfClause : rest -> do
      let (elseIfCond, elseIfBody) = RT.extractFromKeywordClause elseIfClause
      let cont = foldIf m true false elseIfCond elseIfBody rest elseBody
      m
        :< RT.DataElim
          []
          False
          (SE.fromList'' [ifCond])
          ( SE.fromList
              SE.Brace
              SE.Bar
              [ (SE.fromList'' [(blur m, RP.Var true)], [], ifBody, fakeLoc),
                (SE.fromList'' [(blur m, RP.Var false)], [], cont, fakeLoc)
              ]
          )

doNotCare :: Hint -> WT.WeakTerm
doNotCare m =
  m :< WT.Tau

discernLet ::
  Axis ->
  Hint ->
  RawBinder RT.RawTerm ->
  [(Hint, RawIdent)] ->
  RT.RawTerm ->
  RT.RawTerm ->
  Loc ->
  Loc ->
  App WT.WeakTerm
discernLet axis m mxt mys e1 e2 startLoc endLoc = do
  mys' <- mapM (\(my, y) -> discernIdent my axis y) mys
  let (ms', ys') = unzip mys'
  let ysActual = zipWith (\my y -> my :< WT.Var y) ms' ys'
  ysLocal <- mapM Gensym.newIdentFromIdent ys'
  ysCont <- mapM Gensym.newIdentFromIdent ys'
  let localAddition = zipWith (\my yLocal -> (Ident.toText yLocal, (my, yLocal))) ms' ysLocal
  axisLocal <- extendAxisByNominalEnv VDK.Borrowed localAddition axis
  let contAddition = zipWith (\my yCont -> (Ident.toText yCont, (my, yCont))) ms' ysCont
  axisCont <- extendAxisByNominalEnv VDK.Relayed contAddition axis
  e1' <- discern axisLocal e1
  (mxt', e2') <- discernBinderWithBody' axisCont mxt e2 startLoc endLoc
  Tag.insertBinder mxt'
  e2'' <- attachSuffix (zip ysCont ysLocal) e2'
  let opacity = if null mys then WT.Clear else WT.Noetic
  attachPrefix (zip ysLocal (zip ms' ysActual)) (m :< WT.Let opacity mxt' e1' e2'')

discernIdent :: Hint -> Axis -> RawIdent -> App (Hint, Ident)
discernIdent m axis x =
  case lookup x (_nenv axis) of
    Nothing ->
      Throw.raiseError m $ "undefined variable: " <> x
    Just (_, x') -> do
      UnusedVariable.delete x'
      return (m, x')

discernBinder ::
  Axis ->
  [RawBinder RT.RawTerm] ->
  Loc ->
  App ([BinderF WT.WeakTerm], Axis)
discernBinder axis binder endLoc =
  case binder of
    [] -> do
      return ([], axis)
    (mx, x, _, _, t) : xts -> do
      t' <- discern axis t
      x' <- Gensym.newIdentFromText x
      axis' <- extendAxis mx x' VDK.Normal axis
      (xts', axis'') <- discernBinder axis' xts endLoc
      Tag.insertBinder (mx, x', t')
      SymLoc.insert x' (metaLocation mx) endLoc
      return ((mx, x', t') : xts', axis'')

discernBinder' ::
  Axis ->
  [RawBinder RT.RawTerm] ->
  App ([BinderF WT.WeakTerm], Axis)
discernBinder' axis binder =
  case binder of
    [] -> do
      return ([], axis)
    (mx, x, _, _, t) : xts -> do
      t' <- discern axis t
      x' <- Gensym.newIdentFromText x
      axis' <- extendAxis mx x' VDK.Normal axis
      (xts', axis'') <- discernBinder' axis' xts
      Tag.insertBinder (mx, x', t')
      return ((mx, x', t') : xts', axis'')

discernBinderWithBody' ::
  Axis ->
  RawBinder RT.RawTerm ->
  RT.RawTerm ->
  Loc ->
  Loc ->
  App (BinderF WT.WeakTerm, WT.WeakTerm)
discernBinderWithBody' axis (mx, x, _, _, codType) e startLoc endLoc = do
  codType' <- discern axis codType
  x' <- Gensym.newIdentFromText x
  axis'' <- extendAxis mx x' VDK.Normal axis
  e' <- discern axis'' e
  SymLoc.insert x' startLoc endLoc
  return ((mx, x', codType'), e')

discernPatternMatrix ::
  Axis ->
  [RP.RawPatternRow RT.RawTerm] ->
  App (PAT.PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
discernPatternMatrix axis patternMatrix =
  case uncons patternMatrix of
    Nothing ->
      return $ PAT.new []
    Just (row, rows) -> do
      row' <- discernPatternRow axis row
      rows' <- discernPatternMatrix axis rows
      return $ PAT.consRow row' rows'

discernPatternRow ::
  Axis ->
  RP.RawPatternRow RT.RawTerm ->
  App (PAT.PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
discernPatternRow axis (patList, _, body, _) = do
  (patList', body') <- discernPatternRow' axis (SE.extract patList) [] body
  return (V.fromList patList', body')

discernPatternRow' ::
  Axis ->
  [(Hint, RP.RawPattern)] ->
  NominalEnv ->
  RT.RawTerm ->
  App ([(Hint, PAT.Pattern)], ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
discernPatternRow' axis patList newVarList body = do
  case patList of
    [] -> do
      ensureVariableLinearity newVarList
      axis' <- extendAxisByNominalEnv VDK.Normal newVarList axis
      body' <- discern axis' body
      return ([], ([], [], body'))
    pat : rest -> do
      (pat', varsInPat) <- discernPattern pat
      (rest', body') <- discernPatternRow' axis rest (varsInPat ++ newVarList) body
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
          | Just i <- R.readMaybe (T.unpack x) -> do
              return ((m, PAT.LiteralInt i), [])
          | isConsName x -> do
              (consDD, dataArgNum, consArgNum, disc, isConstLike, _) <- resolveConstructor m $ Var x
              consDD' <- Locator.getReadableDD consDD
              unless isConstLike $
                Throw.raiseError m $
                  "the constructor `" <> consDD' <> "` can't be used as a constant"
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
            _ -> do
              dd' <- Locator.getReadableDD dd
              Throw.raiseError m $
                "the symbol `" <> dd' <> "` isn't defined as a constuctor"
    RP.Cons cons _ mArgs -> do
      (consName, dataArgNum, consArgNum, disc, isConstLike, _) <- resolveConstructor m cons
      when isConstLike $
        Throw.raiseError m $
          "the constructor `" <> showName cons <> "` can't have any arguments"
      case mArgs of
        RP.Paren args -> do
          (args', axisList) <- mapAndUnzipM discernPattern $ SE.extract args
          let consInfo =
                PAT.ConsInfo
                  { consDD = consName,
                    isConstLike = isConstLike,
                    disc = disc,
                    dataArgNum = dataArgNum,
                    consArgNum = consArgNum,
                    args = args'
                  }
          return ((m, PAT.Cons consInfo), concat axisList)
        RP.Of mkvs -> do
          let (ks, mvcs) = unzip $ SE.extract mkvs
          let mvs = map (\(mv, _, v) -> (mv, v)) mvcs
          ensureFieldLinearity m ks S.empty S.empty
          (_, keyList) <- KeyArg.lookup m consName
          defaultKeyMap <- constructDefaultKeyMap m keyList
          let specifiedKeyMap = Map.fromList $ zip ks mvs
          let keyMap = Map.union specifiedKeyMap defaultKeyMap
          reorderedArgs <- KeyArg.reorderArgs m keyList keyMap
          (patList', axisList) <- mapAndUnzipM discernPattern reorderedArgs
          let consInfo =
                PAT.ConsInfo
                  { consDD = consName,
                    isConstLike = isConstLike,
                    disc = disc,
                    dataArgNum = dataArgNum,
                    consArgNum = consArgNum,
                    args = patList'
                  }
          return ((m, PAT.Cons consInfo), concat axisList)
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

calculateRuneValue :: [Word8] -> Integer
calculateRuneValue =
  foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0
