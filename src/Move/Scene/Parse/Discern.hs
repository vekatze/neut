module Move.Scene.Parse.Discern (discernStmtList) where

import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.List qualified as List
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Move.Context.App
import Move.Context.Decl qualified as Decl
import Move.Context.EIO (EIO, raiseError, toApp)
import Move.Context.Env (getPlatform)
import Move.Context.Env qualified as Env
import Move.Context.Gensym qualified as Gensym
import Move.Context.Global qualified as Global
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Tag qualified as Tag
import Move.Context.Throw qualified as Throw
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Move.Context.UnusedVariable qualified as UnusedVariable
import Move.Scene.Parse.Discern.Data
import Move.Scene.Parse.Discern.Handle qualified as H
import Move.Scene.Parse.Discern.Name
import Move.Scene.Parse.Discern.Noema
import Move.Scene.Parse.Discern.PatternMatrix
import Move.Scene.Parse.Discern.Struct
import Move.Scene.Parse.Foreign
import Move.Scene.Parse.Util
import Rule.Annotation qualified as AN
import Rule.Arch qualified as Arch
import Rule.ArgNum qualified as AN
import Rule.Attr.Lam qualified as AttrL
import Rule.Attr.VarGlobal qualified as AttrVG
import Rule.BaseName qualified as BN
import Rule.Binder
import Rule.BuildMode qualified as BM
import Rule.C
import Rule.Const
import Rule.DefiniteDescription qualified as DD
import Rule.Error qualified as E
import Rule.ForeignCodType qualified as FCT
import Rule.Geist qualified as G
import Rule.GlobalName qualified as GN
import Rule.Hint
import Rule.Hint.Reify qualified as Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.Key
import Rule.LamKind qualified as LK
import Rule.Layer
import Rule.Literal qualified as LI
import Rule.Locator qualified as L
import Rule.Magic qualified as M
import Rule.Module
import Rule.Name
import Rule.NecessityVariant
import Rule.Noema qualified as N
import Rule.NominalEnv
import Rule.OS qualified as OS
import Rule.Opacity qualified as O
import Rule.Pattern qualified as PAT
import Rule.Platform qualified as Platform
import Rule.PrimType qualified as PT
import Rule.RawBinder
import Rule.RawIdent hiding (isHole)
import Rule.RawPattern qualified as RP
import Rule.RawProgram
import Rule.RawTerm qualified as RT
import Rule.Remark qualified as R
import Rule.Stmt
import Rule.StmtKind qualified as SK
import Rule.Syntax.Series qualified as SE
import Rule.Text.Util
import Rule.TopCandidate
import Rule.VarDefKind qualified as VDK
import Rule.WeakPrim qualified as WP
import Rule.WeakPrimValue qualified as WPV
import Rule.WeakTerm qualified as WT
import Rule.WeakTerm.FreeVars (freeVars)
import Text.Read qualified as R

discernStmtList :: Module -> [RawStmt] -> App [WeakStmt]
discernStmtList mo =
  fmap concat . mapM (discernStmt mo)

discernStmt :: Module -> RawStmt -> App [WeakStmt]
discernStmt mo stmt = do
  nameLifter <- Locator.new >>= liftIO . Locator.getNameLifter
  case stmt of
    RawStmtDefine _ stmtKind (RT.RawDef {geist, body, endLoc}) -> do
      registerTopLevelName nameLifter stmt
      let impArgs = RT.extractArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let (_, codType) = RT.cod geist
      let m = RT.loc geist
      let functionName = nameLifter $ fst $ RT.name geist
      let isConstLike = RT.isConstLike geist
      h <- H.new
      (impArgs', nenv) <- discernBinder h impArgs endLoc
      (expArgs', nenv') <- discernBinder nenv expArgs endLoc
      codType' <- discern nenv' codType
      stmtKind' <- discernStmtKind h stmtKind
      body' <- discern nenv' body
      Tag.insertGlobalVar m functionName isConstLike m
      TopCandidate.insert $ TopCandidate {loc = metaLocation m, dd = functionName, kind = toCandidateKind stmtKind'}
      forM_ expArgs' Tag.insertBinder
      return [WeakStmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' body']
    RawStmtDefineData _ m (dd, _) args consInfo loc -> do
      let stmtList = defineData m dd args (SE.extract consInfo) loc
      discernStmtList mo stmtList
    RawStmtDefineResource _ m (name, _) (_, discarder) (_, copier) _ -> do
      let dd = nameLifter name
      registerTopLevelName nameLifter stmt
      h <- H.new
      t' <- discern h $ m :< RT.Tau
      e' <- discern h $ m :< RT.Resource dd [] (discarder, []) (copier, [])
      Tag.insertGlobalVar m dd True m
      TopCandidate.insert $ TopCandidate {loc = metaLocation m, dd = dd, kind = Constant}
      return [WeakStmtDefine True (SK.Normal O.Clear) m dd [] [] t' e']
    RawStmtNominal _ m geistList -> do
      h <- Global.new
      geistList' <- forM (SE.extract geistList) $ \(geist, endLoc) -> do
        toApp $ Global.registerGeist h geist
        discernGeist endLoc geist
      return [WeakStmtNominal m geistList']
    RawStmtForeign _ foreignList -> do
      let foreignList' = SE.extract foreignList
      h <- H.new
      foreignList'' <- mapM (mapM (discern h)) foreignList'
      foreign' <- interpretForeign foreignList''
      return [WeakStmtForeign foreign']

discernGeist :: Loc -> RT.TopGeist -> App (G.Geist WT.WeakTerm)
discernGeist endLoc geist = do
  nameLifter <- Locator.new >>= liftIO . Locator.getNameLifter
  let impArgs = RT.extractArgs $ RT.impArgs geist
  let expArgs = RT.extractArgs $ RT.expArgs geist
  h <- H.new
  (impArgs', h') <- discernBinder h impArgs endLoc
  (expArgs', h'') <- discernBinder h' expArgs endLoc
  forM_ (impArgs' ++ expArgs') $ \(_, x, _) -> UnusedVariable.delete x
  cod' <- discern h'' $ snd $ RT.cod geist
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
registerTopLevelName nameLifter stmt = do
  h <- Global.new
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
      toApp $ Global.registerStmtDefine h isConstLike m stmtKind' functionName allArgNum expArgNames
    RawStmtNominal {} -> do
      return ()
    RawStmtDefineData _ m (dd, _) args consInfo loc -> do
      let stmtList = defineData m dd args (SE.extract consInfo) loc
      mapM_ (registerTopLevelName nameLifter) stmtList
    RawStmtDefineResource _ m (name, _) _ _ _ -> do
      toApp $ Global.registerStmtDefine h True m (SK.Normal O.Clear) (nameLifter name) AN.zero []
    RawStmtForeign {} ->
      return ()

liftStmtKind :: SK.RawStmtKind BN.BaseName -> App (SK.RawStmtKind DD.DefiniteDescription)
liftStmtKind stmtKind = do
  case stmtKind of
    SK.Normal opacity ->
      return $ SK.Normal opacity
    SK.Data dataName dataArgs consInfoList -> do
      nameLifter <- Locator.new >>= liftIO . Locator.getNameLifter
      let (locList, consNameList, isConstLikeList, consArgsList, discriminantList) = List.unzip5 consInfoList
      let consNameList' = map nameLifter consNameList
      let consInfoList' = List.zip5 locList consNameList' isConstLikeList consArgsList discriminantList
      return $ SK.Data (nameLifter dataName) dataArgs consInfoList'
    SK.DataIntro dataName dataArgs consArgs discriminant -> do
      nameLifter <- Locator.new >>= liftIO . Locator.getNameLifter
      return $ SK.DataIntro (nameLifter dataName) dataArgs consArgs discriminant

discernStmtKind :: H.Handle -> SK.RawStmtKind BN.BaseName -> App (SK.StmtKind WT.WeakTerm)
discernStmtKind ax stmtKind =
  case stmtKind of
    SK.Normal opacity ->
      return $ SK.Normal opacity
    SK.Data dataName dataArgs consInfoList -> do
      nameLifter <- Locator.new >>= liftIO . Locator.getNameLifter
      (dataArgs', h) <- discernBinder' ax dataArgs
      let (locList, consNameList, isConstLikeList, consArgsList, discriminantList) = List.unzip5 consInfoList
      (consArgsList', hList) <- mapAndUnzipM (discernBinder' h) consArgsList
      forM_ (concatMap H.nameEnv hList) $ \(_, (_, newVar, _)) -> do
        UnusedVariable.delete newVar
      let consNameList' = map nameLifter consNameList
      let consInfoList' = List.zip5 locList consNameList' isConstLikeList consArgsList' discriminantList
      return $ SK.Data (nameLifter dataName) dataArgs' consInfoList'
    SK.DataIntro dataName dataArgs consArgs discriminant -> do
      nameLifter <- Locator.new >>= liftIO . Locator.getNameLifter
      (dataArgs', h) <- discernBinder' ax dataArgs
      (consArgs', h') <- discernBinder' h consArgs
      forM_ (H.nameEnv h') $ \(_, (_, newVar, _)) -> do
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

discern :: H.Handle -> RT.RawTerm -> App WT.WeakTerm
discern h term =
  case term of
    m :< RT.Tau ->
      return $ m :< WT.Tau
    m :< RT.Var name ->
      case name of
        Var s
          | Just x <- readIntDecimalMaybe s -> do
              hole <- Gensym.newHole m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int hole x)
          | Just x <- readIntBinaryMaybe s -> do
              hole <- Gensym.newHole m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int hole x)
          | Just x <- readIntOctalMaybe s -> do
              hole <- Gensym.newHole m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int hole x)
          | Just x <- readIntHexadecimalMaybe s -> do
              hole <- Gensym.newHole m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int hole x)
          | Just x <- R.readMaybe (T.unpack s) -> do
              hole <- Gensym.newHole m []
              return $ m :< WT.Prim (WP.Value $ WPV.Float hole x)
          | Just (mDef, name', layer) <- lookup s (H.nameEnv h) -> do
              if layer == H.currentLayer h
                then do
                  UnusedVariable.delete name'
                  Tag.insertLocalVar m name' mDef
                  return $ m :< WT.Var name'
                else
                  toApp $ raiseLayerError m (H.currentLayer h) layer
        _ -> do
          (dd, (_, gn)) <- toApp $ resolveName h m name
          toApp $ interpretGlobalName h m dd gn
    m :< RT.Pi impArgs expArgs _ t endLoc -> do
      (impArgs', h') <- discernBinder h (RT.extractArgs impArgs) endLoc
      (expArgs', h'') <- discernBinder h' (RT.extractArgs expArgs) endLoc
      t' <- discern h'' t
      forM_ (impArgs' ++ expArgs') $ \(_, x, _) -> UnusedVariable.delete x
      return $ m :< WT.Pi impArgs' expArgs' t'
    m :< RT.PiIntro _ (RT.RawDef {geist, body, endLoc}) -> do
      lamID <- Gensym.newCount
      let impArgs = RT.extractArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      (impArgs', h') <- discernBinder h impArgs endLoc
      (expArgs', h'') <- discernBinder h' expArgs endLoc
      codType' <- discern h'' $ snd $ RT.cod geist
      body' <- discern h'' body
      ensureLayerClosedness m h'' body'
      return $ m :< WT.PiIntro (AttrL.normal lamID codType') impArgs' expArgs' body'
    m :< RT.PiIntroFix _ (RT.RawDef {geist, body, endLoc}) -> do
      let impArgs = RT.extractArgs $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let mx = RT.loc geist
      let (x, _) = RT.name geist
      (impArgs', h') <- discernBinder h impArgs endLoc
      (expArgs', h'') <- discernBinder h' expArgs endLoc
      codType' <- discern h'' $ snd $ RT.cod geist
      x' <- Gensym.newIdentFromText x
      h''' <- liftIO $ H.extend' h'' mx x' VDK.Normal
      body' <- discern h''' body
      let mxt' = (mx, x', codType')
      Tag.insertBinder mxt'
      lamID <- Gensym.newCount
      ensureLayerClosedness m h''' body'
      return $ m :< WT.PiIntro (AttrL.Attr {lamKind = LK.Fix mxt', identity = lamID}) impArgs' expArgs' body'
    m :< RT.PiElim e _ es -> do
      case e of
        _ :< RT.Var (Var c)
          | c == "new-cell",
            [arg] <- SE.extract es -> do
              newCellDD <- locatorToVarGlobal m coreCellNewCell
              e' <- discern h $ m :< RT.piElim newCellDD [arg]
              return $ m :< WT.Actual e'
          | c == "new-channel",
            [] <- SE.extract es -> do
              newChannelDD <- locatorToVarGlobal m coreChannelNewChannel
              e' <- discern h $ m :< RT.piElim newChannelDD []
              return $ m :< WT.Actual e'
        _ -> do
          e' <- discern h e
          es' <- mapM (discern h) $ SE.extract es
          return $ m :< WT.PiElim e' es'
    m :< RT.PiElimByKey name _ kvs -> do
      (dd, _) <- toApp $ resolveName h m name
      let (ks, vs) = unzip $ map (\(_, k, _, _, v) -> (k, v)) $ SE.extract kvs
      toApp $ ensureFieldLinearity m ks S.empty S.empty
      hKeyArg <- KeyArg.new
      (argNum, keyList) <- toApp $ KeyArg.lookup hKeyArg m dd
      vs' <- mapM (discern h) vs
      args <- toApp $ KeyArg.reorderArgs m keyList $ Map.fromList $ zip ks vs'
      let isConstLike = False
      return $ m :< WT.PiElim (m :< WT.VarGlobal (AttrVG.Attr {..}) dd) args
    m :< RT.PiElimExact _ e -> do
      e' <- discern h e
      return $ m :< WT.PiElimExact e'
    m :< RT.Data attr dataName es -> do
      hLoc <- Locator.new
      nameLifter <- liftIO $ Locator.getNameLifter hLoc
      dataName' <- liftIO $ Locator.attachCurrentLocator hLoc dataName
      es' <- mapM (discern h) es
      return $ m :< WT.Data (fmap nameLifter attr) dataName' es'
    m :< RT.DataIntro attr consName dataArgs consArgs -> do
      nameLifter <- Locator.new >>= liftIO . Locator.getNameLifter
      dataArgs' <- mapM (discern h) dataArgs
      consArgs' <- mapM (discern h) consArgs
      return $ m :< WT.DataIntro (fmap nameLifter attr) (nameLifter consName) dataArgs' consArgs'
    m :< RT.DataElim _ isNoetic es patternMatrix -> do
      let es' = SE.extract es
      let ms = map (\(me :< _) -> me) es'
      os <- mapM (const $ Gensym.newIdentFromText "match") es' -- os: occurrences
      es'' <- mapM (discern h >=> liftIO . castFromNoemaIfNecessary h isNoetic) es'
      ts <- mapM (const $ Gensym.newHole m []) es''
      patternMatrix' <- discernPatternMatrix h $ SE.extract patternMatrix
      toApp $ ensurePatternMatrixSanity h patternMatrix'
      let os' = zip ms os
      decisionTree <- toApp $ compilePatternMatrix h isNoetic (V.fromList os') patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es'' ts) decisionTree
    m :< RT.Box t -> do
      t' <- discern h t
      return $ m :< WT.Box t'
    m :< RT.BoxNoema t -> do
      t' <- discern h t
      return $ m :< WT.BoxNoema t'
    m :< RT.BoxIntro _ _ mxs (body, _) -> do
      xsOuter <- forM (SE.extract mxs) $ \(mx, x) -> discernIdent mx h x
      xets <- discernNoeticVarList True xsOuter
      let innerLayer = H.currentLayer h - 1
      let xsInner = map (\((mx, x, _), _) -> (mx, x)) xets
      let innerAddition = map (\(mx, x) -> (Ident.toText x, (mx, x, innerLayer))) xsInner
      hInner <- liftIO $ H.extendByNominalEnv (h {H.currentLayer = innerLayer}) VDK.Borrowed innerAddition
      body' <- discern hInner body
      return $ m :< WT.BoxIntro xets body'
    m :< RT.BoxIntroQuote _ _ (body, _) -> do
      body' <- discern h body
      return $ m :< WT.BoxIntroQuote body'
    m :< RT.BoxElim nv mustIgnoreRelayedVars _ (mx, pat, c1, c2, t) _ mys _ e1 _ startLoc _ e2 endLoc -> do
      tmp <- Gensym.newTextFromText "tmp"
      let mxt = (mx, tmp, c1, c2, t)
      let m' = blur m
      let patParam = (mx, pat, [], [], t)
      let e2' = m' :< RT.Let (RT.Plain False) [] patParam [] [] (m' :< RT.Var (Var tmp)) [] startLoc [] e2 endLoc
      -- inner
      ysOuter <- forM (SE.extract mys) $ \(my, y) -> discernIdent my h y
      yetsInner <- discernNoeticVarList True ysOuter
      let innerLayer = H.currentLayer h + layerOffset nv
      let ysInner = map (\((myUse, y, myDef :< _), _) -> (myDef, (myUse, y))) yetsInner
      let innerAddition = map (\(_, (myUse, y)) -> (Ident.toText y, (myUse, y, innerLayer))) ysInner
      hInner <- liftIO $ H.extendByNominalEnv (h {H.currentLayer = innerLayer}) VDK.Borrowed innerAddition
      e1' <- discern hInner e1
      -- cont
      yetsCont <- discernNoeticVarList False ysInner
      let ysCont = map (\((myUse, y, _), _) -> (myUse, y)) yetsCont
      let contAddition = map (\(myUse, y) -> (Ident.toText y, (myUse, y, H.currentLayer h))) ysCont
      hCont <- liftIO $ H.extendByNominalEnv h VDK.Relayed contAddition
      (mxt', e2'') <- discernBinderWithBody' hCont mxt startLoc endLoc e2'
      case pat of
        RP.Var _ ->
          Tag.insertBinder mxt'
        _ ->
          return ()
      when mustIgnoreRelayedVars $ do
        forM_ ysCont $ UnusedVariable.delete . snd
      return $ m :< WT.BoxElim yetsInner mxt' e1' yetsCont e2''
    m :< RT.Embody e -> do
      embodyVar <- locatorToVarGlobal m coreBoxEmbody
      discern h $ m :< RT.piElim embodyVar [e]
    m :< RT.Let letKind _ (mx, pat, c1, c2, t) _ _ e1 _ startLoc _ e2 endLoc -> do
      discernLet h m letKind (mx, pat, c1, c2, t) e1 e2 startLoc endLoc
    m :< RT.LetOn mustIgnoreRelayedVars _ pat _ mys _ e1 _ startLoc _ e2 endLoc -> do
      let e1' = m :< RT.BoxIntroQuote [] [] (e1, [])
      discern h $ m :< RT.BoxElim VariantT mustIgnoreRelayedVars [] pat [] mys [] e1' [] startLoc [] e2 endLoc
    m :< RT.Pin _ mxt@(mx, x, _, _, t) _ mys _ e1 _ startLoc _ e2@(m2 :< _) endLoc -> do
      let m2' = blur m2
      let x' = SE.fromListWithComment Nothing SE.Comma [([], ((mx, x), []))]
      resultType <- Gensym.newPreHole m2'
      resultVar <- Var <$> Gensym.newTextFromText "tmp-pin"
      let resultParam = (m2', RP.Var resultVar, [], [], resultType)
      let isNoetic = not $ null $ SE.extract mys
      if isNoetic
        then do
          let mxt' = (mx, RP.Var (Var x), [], [], t)
          let outerLet cont = m :< RT.LetOn False [] mxt' [] mys [] e1 [] startLoc [] cont endLoc
          discern h $
            outerLet $
              m :< RT.LetOn True [] resultParam [] x' [] e2 [] startLoc [] (m2' :< RT.Var resultVar) endLoc
        else do
          discern h $
            bind startLoc endLoc mxt e1 $
              m :< RT.LetOn True [] resultParam [] x' [] e2 [] startLoc [] (m2' :< RT.Var resultVar) endLoc
    m :< RT.StaticText s str -> do
      s' <- discern h s
      case parseText str of
        Left reason ->
          Throw.raiseError m $ "Could not interpret the following as a text: " <> str <> "\nReason: " <> reason
        Right str' -> do
          return $ m :< WT.Prim (WP.Value $ WPV.StaticText s' str')
    m :< RT.Rune -> do
      return $ m :< WT.Prim (WP.Type PT.Rune)
    m :< RT.RuneIntro _ r -> do
      return $ m :< WT.Prim (WP.Value $ WPV.Rune r)
    m :< RT.Hole k ->
      return $ m :< WT.Hole k []
    m :< RT.Magic _ magic -> do
      magic' <- discernMagic h m magic
      return $ m :< WT.Magic magic'
    m :< RT.Annotation remarkLevel annot e -> do
      e' <- discern h e
      case annot of
        AN.Type _ ->
          return $ m :< WT.Annotation remarkLevel (AN.Type (doNotCare m)) e'
    m :< RT.Resource dd _ (discarder, _) (copier, _) -> do
      unitType <- locatorToVarGlobal m coreUnit >>= discern h
      resourceID <- Gensym.newCount
      discarder' <- discern h discarder
      copier' <- discern h copier
      return $ m :< WT.Resource dd resourceID unitType discarder' copier'
    m :< RT.Use _ e _ xs _ cont endLoc -> do
      e' <- discern h e
      (xs', h') <- discernBinder h (RT.extractArgs xs) endLoc
      cont' <- discern h' cont
      return $ m :< WT.Use e' xs' cont'
    m :< RT.If ifClause elseIfClauseList (_, (elseBody, _)) -> do
      let (ifCond, ifBody) = RT.extractFromKeywordClause ifClause
      boolTrue <- locatorToName (blur m) coreBoolTrue
      boolFalse <- locatorToName (blur m) coreBoolFalse
      discern h $ foldIf m boolTrue boolFalse ifCond ifBody elseIfClauseList elseBody
    m :< RT.Seq (e1, _) _ e2 -> do
      hole <- Gensym.newTextForHole
      unit <- locatorToVarGlobal m coreUnit
      discern h $ bind fakeLoc fakeLoc (m, hole, [], [], unit) e1 e2
    m :< RT.When whenClause -> do
      let (whenCond, whenBody) = RT.extractFromKeywordClause whenClause
      boolTrue <- locatorToName (blur m) coreBoolTrue
      boolFalse <- locatorToName (blur m) coreBoolFalse
      unitUnit <- locatorToVarGlobal m coreUnitUnit
      discern h $ foldIf m boolTrue boolFalse whenCond whenBody [] unitUnit
    m :< RT.ListIntro es -> do
      let m' = m {metaShouldSaveLocation = False}
      listNil <- locatorToVarGlobal m' coreListNil
      listCons <- locatorToVarGlobal m' coreListCons
      discern h $ foldListApp m' listNil listCons $ SE.extract es
    m :< RT.Admit -> do
      panic <- locatorToVarGlobal m coreTrickUnsafePanic
      textType <- locatorToVarGlobal m coreText
      discern h $
        asOpaqueValue $
          m
            :< RT.Annotation
              R.Warning
              (AN.Type ())
              ( m
                  :< RT.piElim
                    panic
                    [m :< RT.StaticText textType ("Admitted: " <> T.pack (Hint.toString m) <> "\n")]
              )
    m :< RT.Detach _ _ (e, _) -> do
      t <- Gensym.newPreHole (blur m)
      detachVar <- locatorToVarGlobal m coreThreadDetach
      cod <- Gensym.newPreHole (blur m)
      discern h $ m :< RT.piElim detachVar [t, RT.lam fakeLoc m [] cod e]
    m :< RT.Attach _ _ (e, _) -> do
      t <- Gensym.newPreHole (blur m)
      attachVar <- locatorToVarGlobal m coreThreadAttach
      discern h $ m :< RT.piElim attachVar [t, e]
    m :< RT.Option t -> do
      eitherVar <- locatorToVarGlobal m coreEither
      unit <- locatorToVarGlobal m coreUnit
      discern h $ m :< RT.piElim eitherVar [unit, t]
    m :< RT.Assert _ (mText, message) _ _ (e@(mCond :< _), _) -> do
      assert <- locatorToVarGlobal m coreTrickAssert
      textType <- locatorToVarGlobal m coreText
      let fullMessage = T.pack (Hint.toString m) <> "\nAssertion failure: " <> message <> "\n"
      cod <- Gensym.newPreHole (blur m)
      discern h $
        m
          :< RT.piElim
            assert
            [mText :< RT.StaticText textType fullMessage, RT.lam fakeLoc mCond [] cod e]
    m :< RT.Introspect _ key _ clauseList -> do
      value <- getIntrospectiveValue m key
      clause <- lookupIntrospectiveClause m value $ SE.extract clauseList
      discern h clause
    m :< RT.IncludeText _ _ mKey (key, _) -> do
      hLoc <- Locator.new
      contentOrNone <- liftIO $ Locator.getStaticFileContent hLoc key
      case contentOrNone of
        Just (path, content) -> do
          UnusedStaticFile.delete key
          textType <- locatorToVarGlobal m coreText >>= discern h
          Tag.insertFileLoc mKey (T.length key) (newSourceHint path)
          return $ m :< WT.Prim (WP.Value $ WPV.StaticText textType content)
        Nothing ->
          Throw.raiseError m $ "No such static file is defined: `" <> key <> "`"
    m :< RT.With withClause -> do
      let (binder, body) = RT.extractFromKeywordClause withClause
      case body of
        mLet :< RT.Let letKind c1 mxt@(mPat, pat, c2, c3, t) c c4 e1 c5 startLoc c6 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          case letKind of
            RT.Bind -> do
              tmpVar <- Gensym.newText
              (x, e2'') <- modifyLetContinuation (mPat, pat) endLoc False e2'
              let m' = blur m
              dom <- Gensym.newPreHole m'
              cod <- Gensym.newPreHole m'
              discern h $
                bind'
                  False
                  startLoc
                  endLoc
                  (mPat, tmpVar, c2, c3, dom)
                  e1'
                  ( m
                      :< RT.piElim
                        binder
                        [ m' :< RT.Var (Var tmpVar),
                          RT.lam
                            startLoc
                            m'
                            [((mPat, x, c2, c3, t), c)]
                            cod
                            e2''
                        ]
                  )
            _ -> do
              discern h $ mLet :< RT.Let letKind c1 mxt c c4 e1' c5 startLoc c6 e2' endLoc
        mLet :< RT.LetOn mustIgnoreRelayedVars c1 mxt c2 mys c3 e1 c4 startLoc c5 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $ mLet :< RT.LetOn mustIgnoreRelayedVars c1 mxt c2 mys c3 e1' c4 startLoc c5 e2' endLoc
        mBox :< RT.BoxElim nesVariant mustIgnoreRelayedVars c1 mxt c2 mys c3 e1 c4 startLoc c5 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $
            mBox :< RT.BoxElim nesVariant mustIgnoreRelayedVars c1 mxt c2 mys c3 e1' c4 startLoc c5 e2' endLoc
        mSeq :< RT.Seq (e1, c1) c2 e2 -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $ mSeq :< RT.Seq (e1', c1) c2 e2'
        mUse :< RT.Use c1 item c2 vars c3 cont endLoc -> do
          let cont' = m :< RT.With (([], (binder, [])), ([], (cont, [])))
          discern h $ mUse :< RT.Use c1 item c2 vars c3 cont' endLoc
        mPin :< RT.Pin c1 (mx, x, c2, c3, t) c4 mys c5 e1 c6 startLoc c7 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $ mPin :< RT.Pin c1 (mx, x, c2, c3, t) c4 mys c5 e1' c6 startLoc c7 e2' endLoc
        _ ->
          discern h body
    _ :< RT.Projection e (mProj, proj) loc -> do
      t <- Gensym.newPreHole (blur mProj)
      let args = (SE.fromList SE.Brace SE.Comma [(mProj, proj, [], [], t)], [])
      let var = mProj :< RT.Var (Var proj)
      discern h $ mProj :< RT.Use [] e [] args [] var loc
    _ :< RT.Brace _ (e, _) ->
      discern h e
    m :< RT.Pointer ->
      return $ m :< WT.Prim (WP.Type PT.Pointer)
    m :< RT.Void ->
      return $ m :< WT.Void

type ShouldInsertTagInfo =
  Bool

discernNoeticVarList :: ShouldInsertTagInfo -> [(Hint, (Hint, Ident))] -> App [(BinderF WT.WeakTerm, WT.WeakTerm)]
discernNoeticVarList mustInsertTagInfo xsOuter = do
  forM xsOuter $ \(mDef, (mUse, outerVar)) -> do
    xInner <- Gensym.newIdentFromIdent outerVar
    t <- Gensym.newHole mUse []
    when mustInsertTagInfo $ do
      Tag.insertLocalVar mUse outerVar mDef
    return ((mUse, xInner, t), mDef :< WT.Var outerVar)

discernMagic :: H.Handle -> Hint -> RT.RawMagic -> App (M.WeakMagic WT.WeakTerm)
discernMagic h m magic =
  case magic of
    RT.Cast _ (_, (from, _)) (_, (to, _)) (_, (e, _)) _ -> do
      from' <- discern h from
      to' <- discern h to
      e' <- discern h e
      return $ M.WeakMagic $ M.Cast from' to' e'
    RT.Store _ (_, (t, _)) (_, (value, _)) (_, (pointer, _)) _ -> do
      t' <- discern h t
      unit <- locatorToVarGlobal m coreUnit >>= discern h
      value' <- discern h value
      pointer' <- discern h pointer
      return $ M.WeakMagic $ M.Store t' unit value' pointer'
    RT.Load _ (_, (t, _)) (_, (pointer, _)) _ -> do
      t' <- discern h t
      pointer' <- discern h pointer
      return $ M.WeakMagic $ M.Load t' pointer'
    RT.Alloca _ (_, (t, _)) (_, (size, _)) _ -> do
      t' <- discern h t
      size' <- discern h size
      return $ M.WeakMagic $ M.Alloca t' size'
    RT.External _ mUse funcName _ args varArgsOrNone -> do
      mDef <- Decl.lookupPreDeclEnv m funcName
      Tag.insertExternalName mUse funcName mDef
      let domList = []
      let cod = FCT.Void
      args' <- mapM (discern h) $ SE.extract args
      varArgs' <- case varArgsOrNone of
        Nothing ->
          return []
        Just (_, varArgs) ->
          forM (SE.extract varArgs) $ \(_, arg, _, _, t) -> do
            arg' <- discern h arg
            t' <- discern h t
            return (arg', t')
      return $ M.WeakMagic $ M.External domList cod funcName args' varArgs'
    RT.Global _ (_, (name, _)) (_, (t, _)) _ -> do
      t' <- discern h t
      return $ M.WeakMagic $ M.Global name t'
    RT.OpaqueValue _ (_, (e, _)) -> do
      e' <- discern h e
      return $ M.WeakMagic $ M.OpaqueValue e'

modifyLetContinuation :: (Hint, RP.RawPattern) -> Loc -> N.IsNoetic -> RT.RawTerm -> App (RawIdent, RT.RawTerm)
modifyLetContinuation pat endLoc isNoetic cont@(mCont :< _) =
  case pat of
    (_, RP.Var (Var x))
      | not (isConsName x) ->
          return (x, cont)
    _ -> do
      tmp <- Gensym.newTextForHole
      return
        ( tmp,
          mCont
            :< RT.DataElim
              []
              isNoetic
              (SE.fromList'' [mCont :< RT.Var (Var tmp)])
              (SE.fromList SE.Brace SE.Bar [(SE.fromList'' [pat], [], cont, endLoc)])
        )

bind ::
  Loc ->
  Loc ->
  RawBinder RT.RawTerm ->
  RT.RawTerm ->
  RT.RawTerm ->
  RT.RawTerm
bind startLoc endLoc (m, x, c1, c2, t) =
  bind' False startLoc endLoc (m, x, c1, c2, t)

bind' ::
  RT.MustIgnoreRelayedVars ->
  Loc ->
  Loc ->
  RawBinder RT.RawTerm ->
  RT.RawTerm ->
  RT.RawTerm ->
  RT.RawTerm
bind' mustIgnoreRelayedVars loc endLoc (m, x, c1, c2, t) e cont =
  m
    :< RT.Let
      (RT.Plain mustIgnoreRelayedVars)
      []
      (m, RP.Var (Var x), c1, c2, t)
      []
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
      Throw.raiseError m $ "This term does not support `" <> value <> "`."
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
  p <- toApp $ getPlatform (Just m)
  case key of
    "architecture" ->
      return $ Arch.reify (Platform.arch p)
    "operating-system" ->
      return $ OS.reify (Platform.os p)
    "build-mode" ->
      return $ BM.reify bm
    _ ->
      Throw.raiseError m $ "No such introspective value is defined: " <> key

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
  H.Handle ->
  Hint ->
  RT.LetKind ->
  (Hint, RP.RawPattern, C, C, RT.RawTerm) ->
  RT.RawTerm ->
  RT.RawTerm ->
  Loc ->
  Loc ->
  App WT.WeakTerm
discernLet h m letKind (mx, pat, c1, c2, t) e1@(m1 :< _) e2 startLoc endLoc = do
  let opacity = WT.Clear
  let discernLet' isNoetic = do
        e1' <- discern h e1
        (x, e2') <- modifyLetContinuation (mx, pat) endLoc isNoetic e2
        (mxt', e2'') <- discernBinderWithBody' h (mx, x, c1, c2, t) startLoc endLoc e2'
        Tag.insertBinder mxt'
        return $ m :< WT.Let opacity mxt' e1' e2''
  case letKind of
    RT.Plain _ -> do
      discernLet' False
    RT.Noetic -> do
      discernLet' True
    RT.Bind -> do
      Throw.raiseError m "`bind` can only be used inside `with`"
    RT.Try -> do
      let m' = blur m
      eitherTypeInner <- locatorToVarGlobal m' coreEither
      leftType <- Gensym.newPreHole m'
      let eitherType = m' :< RT.piElim eitherTypeInner [leftType, t]
      e1' <- discern h e1
      tmpVar <- Gensym.newText
      eitherCont <- constructEitherBinder m mx m1 pat tmpVar e2 endLoc
      (mxt', eitherCont') <- discernBinderWithBody' h (mx, tmpVar, c1, c2, eitherType) startLoc endLoc eitherCont
      return $ m :< WT.Let opacity mxt' e1' eitherCont'

constructEitherBinder ::
  Hint ->
  Hint ->
  Hint ->
  RP.RawPattern ->
  RawIdent ->
  Cofree RT.RawTermF Hint ->
  Loc ->
  App RT.RawTerm
constructEitherBinder m mx m1 pat tmpVar cont endLoc = do
  let m' = blur m
  let mx' = blur mx
  let m1' = blur m1
  earlyRetVar <- Gensym.newText
  eitherL <- locatorToName m1 coreEitherLeft
  eitherR <- locatorToName m1 coreEitherRight
  eitherVarL <- locatorToVarGlobal m1 coreEitherLeft
  let longClause =
        ( SE.fromList'' [(mx', RP.Cons eitherR [] (RP.Paren (SE.fromList' [(mx, pat)])))],
          [],
          cont,
          endLoc
        )
  let shortClause =
        ( SE.fromList'' [(m', RP.Cons eitherL [] (RP.Paren (SE.fromList' [(m', RP.Var (Var earlyRetVar))])))],
          [],
          m' :< RT.piElim eitherVarL [m' :< RT.Var (Var earlyRetVar)],
          fakeLoc
        )
  return $
    m'
      :< RT.DataElim
        []
        False
        (SE.fromList'' [m1' :< RT.Var (Var tmpVar)])
        (SE.fromList SE.Brace SE.Bar [longClause, shortClause])

discernIdent :: Hint -> H.Handle -> RawIdent -> App (Hint, (Hint, Ident))
discernIdent mUse h x =
  case lookup x (H.nameEnv h) of
    Nothing ->
      Throw.raiseError mUse $ "Undefined variable: " <> x
    Just (mDef, x', _) -> do
      UnusedVariable.delete x'
      return (mDef, (mUse, x'))

discernBinder ::
  H.Handle ->
  [RawBinder RT.RawTerm] ->
  Loc ->
  App ([BinderF WT.WeakTerm], H.Handle)
discernBinder h binder endLoc =
  case binder of
    [] -> do
      return ([], h)
    (mx, x, _, _, t) : xts -> do
      t' <- discern h t
      x' <- Gensym.newIdentFromText x
      h' <- liftIO $ H.extend' h mx x' VDK.Normal
      (xts', h'') <- discernBinder h' xts endLoc
      Tag.insertBinder (mx, x', t')
      SymLoc.insert x' (metaLocation mx) endLoc
      return ((mx, x', t') : xts', h'')

discernBinder' ::
  H.Handle ->
  [RawBinder RT.RawTerm] ->
  App ([BinderF WT.WeakTerm], H.Handle)
discernBinder' h binder =
  case binder of
    [] -> do
      return ([], h)
    (mx, x, _, _, t) : xts -> do
      t' <- discern h t
      x' <- Gensym.newIdentFromText x
      h' <- liftIO $ H.extend' h mx x' VDK.Normal
      (xts', h'') <- discernBinder' h' xts
      Tag.insertBinder (mx, x', t')
      return ((mx, x', t') : xts', h'')

discernBinderWithBody' ::
  H.Handle ->
  RawBinder RT.RawTerm ->
  Loc ->
  Loc ->
  RT.RawTerm ->
  App (BinderF WT.WeakTerm, WT.WeakTerm)
discernBinderWithBody' h (mx, x, _, _, codType) startLoc endLoc e = do
  codType' <- discern h codType
  x' <- Gensym.newIdentFromText x
  h'' <- liftIO $ H.extend' h mx x' VDK.Normal
  e' <- discern h'' e
  SymLoc.insert x' startLoc endLoc
  return ((mx, x', codType'), e')

discernPatternMatrix ::
  H.Handle ->
  [RP.RawPatternRow RT.RawTerm] ->
  App (PAT.PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
discernPatternMatrix h patternMatrix =
  case List.uncons patternMatrix of
    Nothing ->
      return $ PAT.new []
    Just (row, rows) -> do
      row' <- discernPatternRow h row
      rows' <- discernPatternMatrix h rows
      return $ PAT.consRow row' rows'

discernPatternRow ::
  H.Handle ->
  RP.RawPatternRow RT.RawTerm ->
  App (PAT.PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
discernPatternRow h (patList, _, body, _) = do
  (patList', body') <- discernPatternRow' h (SE.extract patList) [] body
  return (V.fromList patList', body')

discernPatternRow' ::
  H.Handle ->
  [(Hint, RP.RawPattern)] ->
  NominalEnv ->
  RT.RawTerm ->
  App ([(Hint, PAT.Pattern)], ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
discernPatternRow' h patList newVarList body = do
  case patList of
    [] -> do
      ensureVariableLinearity newVarList
      h' <- liftIO $ H.extendByNominalEnv h VDK.Normal newVarList
      body' <- discern h' body
      return ([], ([], [], body'))
    pat : rest -> do
      (pat', varsInPat) <- discernPattern h (H.currentLayer h) pat
      (rest', body') <- discernPatternRow' h rest (varsInPat ++ newVarList) body
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
    (from, (m, _, _)) : rest
      | S.member from found ->
          getNonLinearOccurrences rest found ((m, from) : nonLinear)
      | otherwise ->
          getNonLinearOccurrences rest (S.insert from found) nonLinear

discernPattern ::
  H.Handle ->
  Layer ->
  (Hint, RP.RawPattern) ->
  App ((Hint, PAT.Pattern), NominalEnv)
discernPattern h layer (m, pat) = do
  case pat of
    RP.Var name -> do
      case name of
        Var x
          | Just i <- R.readMaybe (T.unpack x) -> do
              return ((m, PAT.Literal (LI.Int i)), [])
          | isConsName x -> do
              (consDD, dataArgNum, consArgNum, disc, isConstLike, _) <- toApp $ resolveConstructor h m $ Var x
              unless isConstLike $ do
                mainModule <- Env.getMainModule
                let consDD' = Locator.getReadableDD mainModule consDD
                Throw.raiseError m $
                  "The constructor `" <> consDD' <> "` cannot be used as a constant"
              return ((m, PAT.Cons (PAT.ConsInfo {args = [], ..})), [])
          | otherwise -> do
              x' <- Gensym.newIdentFromText x
              return ((m, PAT.Var x'), [(x, (m, x', layer))])
        Locator l -> do
          (dd, gn) <- toApp $ resolveName h m $ Locator l
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
              mainModule <- Env.getMainModule
              let dd' = Locator.getReadableDD mainModule dd
              Throw.raiseError m $
                "The symbol `" <> dd' <> "` is not defined as a constuctor"
    RP.Cons cons _ mArgs -> do
      (consName, dataArgNum, consArgNum, disc, isConstLike, _) <- toApp $ resolveConstructor h m cons
      when isConstLike $
        Throw.raiseError m $
          "The constructor `" <> showName cons <> "` cannot have any arguments"
      case mArgs of
        RP.Paren args -> do
          (args', hList) <- mapAndUnzipM (discernPattern h layer) $ SE.extract args
          let consInfo =
                PAT.ConsInfo
                  { consDD = consName,
                    isConstLike = isConstLike,
                    disc = disc,
                    dataArgNum = dataArgNum,
                    consArgNum = consArgNum,
                    args = args'
                  }
          return ((m, PAT.Cons consInfo), concat hList)
        RP.Of mkvs -> do
          let (ks, mvcs) = unzip $ SE.extract mkvs
          let mvs = map (\(mv, _, v) -> (mv, v)) mvcs
          toApp $ ensureFieldLinearity m ks S.empty S.empty
          hKeyArg <- KeyArg.new
          (_, keyList) <- toApp $ KeyArg.lookup hKeyArg m consName
          defaultKeyMap <- constructDefaultKeyMap m keyList
          let specifiedKeyMap = Map.fromList $ zip ks mvs
          let keyMap = Map.union specifiedKeyMap defaultKeyMap
          reorderedArgs <- toApp $ KeyArg.reorderArgs m keyList keyMap
          (patList', hList) <- mapAndUnzipM (discernPattern h layer) reorderedArgs
          let consInfo =
                PAT.ConsInfo
                  { consDD = consName,
                    isConstLike = isConstLike,
                    disc = disc,
                    dataArgNum = dataArgNum,
                    consArgNum = consArgNum,
                    args = patList'
                  }
          return ((m, PAT.Cons consInfo), concat hList)
    RP.ListIntro patList -> do
      let m' = m {metaShouldSaveLocation = False}
      listNil <- Throw.liftEither $ DD.getLocatorPair m' coreListNil
      listCons <- locatorToName m' coreListCons
      discernPattern h layer $ foldListAppPat m' listNil listCons $ SE.extract patList
    RP.RuneIntro r -> do
      return ((m, PAT.Literal (LI.Rune r)), [])

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

getLayer :: Hint -> H.Handle -> Ident -> App Layer
getLayer m h x =
  case lookup (Ident.toText x) (H.nameEnv h) of
    Nothing ->
      Throw.raiseCritical m $ "Scene.Parse.Discern.getLayer: Undefined variable: " <> Ident.toText x
    Just (_, _, l) -> do
      return l

findExternalVariable :: Hint -> H.Handle -> WT.WeakTerm -> App (Maybe (Ident, Layer))
findExternalVariable m h e = do
  let fvs = S.toList $ freeVars e
  ls <- mapM (getLayer m h) fvs
  return $ List.find (\(_, l) -> l > H.currentLayer h) $ zip fvs ls

ensureLayerClosedness :: Hint -> H.Handle -> WT.WeakTerm -> App ()
ensureLayerClosedness mClosure h e = do
  mvar <- findExternalVariable mClosure h e
  case mvar of
    Nothing ->
      return ()
    Just (x, l) -> do
      Throw.raiseError mClosure $
        "This function is at the layer "
          <> T.pack (show (H.currentLayer h))
          <> ", but the free variable `"
          <> Ident.toText x
          <> "` is at the layer "
          <> T.pack (show l)
          <> " (> "
          <> T.pack (show (H.currentLayer h))
          <> ")"

raiseLayerError :: Hint -> Layer -> Layer -> EIO a
raiseLayerError m expected found = do
  raiseError m $
    "Expected layer:\n  "
      <> T.pack (show expected)
      <> "\nFound layer:\n  "
      <> T.pack (show found)

asOpaqueValue :: RT.RawTerm -> RT.RawTerm
asOpaqueValue e@(m :< _) =
  m :< RT.Magic [] (RT.OpaqueValue [] ([], (e, [])))
