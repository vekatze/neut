module Kernel.Parse.Internal.Discern (discernStmtList) where

import App.App (App)
import App.Error qualified as E
import App.Run (raiseCritical, raiseError)
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils qualified as ListUtils
import Data.HashMap.Strict qualified as Map
import Data.List ((\\))
import Data.List qualified as List
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Gensym.Gensym qualified as Gensym
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.BuildMode qualified as BM
import Kernel.Common.Const
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.KeyArg (ExpKey, ImpKey, _showKeyList)
import Kernel.Common.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.SymLoc qualified as SymLoc
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Handle.Local.TopCandidate qualified as TopCandidate
import Kernel.Common.OS qualified as OS
import Kernel.Common.Platform qualified as Platform
import Kernel.Common.ReadableDD
import Kernel.Common.TopCandidate
import Kernel.Parse.Internal.Discern.Handle qualified as H
import Kernel.Parse.Internal.Discern.Name
import Kernel.Parse.Internal.Discern.Noema
import Kernel.Parse.Internal.Discern.PatternMatrix
import Kernel.Parse.Internal.Discern.Struct
import Kernel.Parse.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Internal.Handle.PreDecl qualified as PreDecl
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.Internal.Util
import Kernel.Parse.Layer
import Kernel.Parse.NominalEnv
import Kernel.Parse.Pattern qualified as PAT
import Kernel.Parse.VarDefKind qualified as VDK
import Language.Common.Annotation qualified as AN
import Language.Common.ArgNum qualified as AN
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign qualified as F
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Geist qualified as G
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.Literal qualified as LI
import Language.Common.Magic qualified as M
import Language.Common.Noema qualified as N
import Language.Common.Opacity qualified as O
import Language.Common.PiKind qualified as PK
import Language.Common.PrimNumSize (IntSize (IntSize64))
import Language.Common.PrimType qualified as PT
import Language.Common.RuleKind (RuleKind (FoldLeft, FoldRight))
import Language.Common.StmtKind qualified as SK
import Language.Common.Text.Util
import Language.RawTerm.CreateHole qualified as RT
import Language.RawTerm.Key
import Language.RawTerm.Name
import Language.RawTerm.NecessityVariant
import Language.RawTerm.RawBinder
import Language.RawTerm.RawIdent hiding (isHole)
import Language.RawTerm.RawPattern qualified as RP
import Language.RawTerm.RawStmt
import Language.RawTerm.RawTerm qualified as RT
import Language.WeakTerm.CreateHole qualified as WT
import Language.WeakTerm.FreeVars (freeVars)
import Language.WeakTerm.WeakPrim qualified as WP
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakStmt
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint
import Logger.Hint.Reify qualified as Hint
import Logger.Log qualified as L
import Logger.LogLevel qualified as L
import SyntaxTree.C
import SyntaxTree.Series qualified as SE
import Text.Read qualified as R

discernStmtList :: H.Handle -> [PostRawStmt] -> App [WeakStmt]
discernStmtList h =
  fmap concat . mapM (discernStmt h)

discernStmt :: H.Handle -> PostRawStmt -> App [WeakStmt]
discernStmt h stmt = do
  case stmt of
    PostRawStmtDefine _ stmtKind (RT.RawDef {geist, body, endLoc}) -> do
      registerTopLevelName h stmt
      let impArgsWithDefaults = RT.extractImpArgsWithDefaults $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let (_, codType) = RT.cod geist
      let m = RT.loc geist
      let functionName = fst $ RT.name geist
      let isConstLike = RT.isConstLike geist
      (impArgs', nenv) <- discernBinderWithDefaults h impArgsWithDefaults endLoc
      (expArgs', nenv') <- discernBinder nenv expArgs endLoc
      codType' <- discern nenv' codType
      stmtKind' <- discernStmtKind h stmtKind m
      body' <- discern nenv' body
      liftIO $ Tag.insertGlobalVar (H.tagHandle h) m functionName isConstLike m
      when (metaShouldSaveLocation m) $ do
        liftIO $ TopCandidate.insert (H.topCandidateHandle h) $ do
          TopCandidate {loc = metaLocation m, dd = functionName, kind = toCandidateKind stmtKind'}
      liftIO $ forM_ (map fst impArgs') $ Tag.insertBinder (H.tagHandle h)
      liftIO $ forM_ expArgs' $ Tag.insertBinder (H.tagHandle h)
      return [WeakStmtDefine isConstLike stmtKind' m functionName impArgs' expArgs' codType' body']
    PostRawStmtDefineResource _ m (dd, _) (_, discarder) (_, copier) (_, typeTag) _ -> do
      registerTopLevelName h stmt
      t' <- discern h $ m :< RT.Tau
      e' <- discern h $ m :< RT.Resource dd [] (discarder, []) (copier, []) (typeTag, [])
      liftIO $ Tag.insertGlobalVar (H.tagHandle h) m dd True m
      liftIO $ TopCandidate.insert (H.topCandidateHandle h) $ do
        TopCandidate {loc = metaLocation m, dd = dd, kind = Constant}
      return [WeakStmtDefine True (SK.Normal O.Clear) m dd [] [] t' e']
    PostRawStmtVariadic kind m dd -> do
      registerTopLevelName h stmt
      liftIO $ Tag.insertGlobalVar (H.tagHandle h) m dd True m
      liftIO $ TopCandidate.insert (H.topCandidateHandle h) $ do
        TopCandidate {loc = metaLocation m, dd = dd, kind = Function}
      return [WeakStmtVariadic kind m dd]
    PostRawStmtNominal _ m geistList -> do
      geistList' <- forM (SE.extract geistList) $ \(geist, endLoc) -> do
        NameMap.registerGeist (H.nameMapHandle h) geist
        discernGeist h endLoc geist
      return [WeakStmtNominal m geistList']
    PostRawStmtForeign _ foreignList -> do
      let foreignList' = SE.extract foreignList
      foreignList'' <- mapM (mapM (discern h)) foreignList'
      foreign' <- liftIO $ interpretForeign h foreignList''
      return [WeakStmtForeign foreign']

discernGeist :: H.Handle -> Loc -> RT.RawGeist DD.DefiniteDescription -> App (G.Geist WT.WeakTerm)
discernGeist h endLoc geist = do
  let impArgsWithDefaults = RT.extractImpArgsWithDefaults $ RT.impArgs geist
  let expArgs = RT.extractArgs $ RT.expArgs geist
  (impArgs', h') <- discernBinderWithDefaults h impArgsWithDefaults endLoc
  (expArgs', h'') <- discernBinder h' expArgs endLoc
  forM_ (map fst impArgs' ++ expArgs') $ \(_, x, _) -> liftIO $ Unused.deleteVariable (H.unusedHandle h) x
  cod' <- discern h'' $ snd $ RT.cod geist
  let m = RT.loc geist
  let dd = fst $ RT.name geist
  let kind = if RT.isConstLike geist then Constant else Function
  liftIO $ TopCandidate.insert (H.topCandidateHandle h) $ TopCandidate {loc = metaLocation m, dd = dd, kind = kind}
  return $
    G.Geist
      { loc = m,
        name = dd,
        isConstLike = RT.isConstLike geist,
        impArgs = impArgs',
        expArgs = expArgs',
        cod = cod'
      }

registerTopLevelName :: H.Handle -> PostRawStmt -> App ()
registerTopLevelName h stmt = do
  let arrow = NameMap.getGlobalNames [stmt]
  NameMap.insert (H.nameMapHandle h) arrow

discernStmtKind :: H.Handle -> RawStmtKind DD.DefiniteDescription -> Hint -> App (SK.StmtKind WT.WeakTerm)
discernStmtKind h stmtKind m =
  case stmtKind of
    SK.Normal opacity ->
      return $ SK.Normal opacity
    SK.Main opacity _ -> do
      unitType <- getUnitType h m
      return $ SK.Main opacity unitType
    SK.Data dataName dataArgs consInfoList -> do
      (dataArgs', h') <- discernBinder' h dataArgs
      let (locList, consNameList, isConstLikeList, consArgsList, discriminantList) = List.unzip5 consInfoList
      (consArgsList', hList) <- mapAndUnzipM (discernBinder' h') consArgsList
      forM_ (concatMap H.nameEnv hList) $ \(_, (_, newVar, _)) -> do
        liftIO $ Unused.deleteVariable (H.unusedHandle h') newVar
      let consNameList' = consNameList
      let consInfoList' = List.zip5 locList consNameList' isConstLikeList consArgsList' discriminantList
      return $ SK.Data dataName dataArgs' consInfoList'
    SK.DataIntro dataName dataArgs expConsArgs discriminant -> do
      (dataArgs', h') <- discernBinder' h dataArgs
      (expConsArgs', h'') <- discernBinder' h' expConsArgs
      forM_ (H.nameEnv h'') $ \(_, (_, newVar, _)) -> do
        liftIO $ Unused.deleteVariable (H.unusedHandle h'') newVar
      return $ SK.DataIntro dataName dataArgs' expConsArgs' discriminant

getUnitType :: H.Handle -> Hint -> App WT.WeakTerm
getUnitType h m = do
  locator <- liftEither $ DD.getLocatorPair m coreUnit
  (unitDD, _) <- resolveName h m (Locator locator)
  let attr = AttrVG.Attr {argNum = AN.fromInt 0, isConstLike = True}
  return $ m :< WT.PiElim False (m :< WT.VarGlobal attr unitDD) ImpArgs.Unspecified []

toCandidateKind :: SK.StmtKind a -> CandidateKind
toCandidateKind stmtKind =
  case stmtKind of
    SK.Normal {} ->
      Function
    SK.Main {} ->
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
              hole <- liftIO $ WT.createHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int hole x)
          | Just x <- readIntBinaryMaybe s -> do
              hole <- liftIO $ WT.createHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int hole x)
          | Just x <- readIntOctalMaybe s -> do
              hole <- liftIO $ WT.createHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int hole x)
          | Just x <- readIntHexadecimalMaybe s -> do
              hole <- liftIO $ WT.createHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WP.Value $ WPV.Int hole x)
          | Just x <- R.readMaybe (T.unpack s) -> do
              hole <- liftIO $ WT.createHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WP.Value $ WPV.Float hole x)
          | Just (mDef, name', layer) <- lookup s (H.nameEnv h) -> do
              if layer == H.currentLayer h
                then do
                  liftIO $ Unused.deleteVariable (H.unusedHandle h) name'
                  liftIO $ Tag.insertLocalVar (H.tagHandle h) m name' mDef
                  return $ m :< WT.Var name'
                else raiseLayerError m (H.currentLayer h) layer
        _ -> do
          (dd, (_, gn)) <- resolveName h m name
          interpretGlobalName h m dd gn
    m :< RT.VarGlobal dd gn -> do
      interpretGlobalName h m dd gn
    m :< RT.Pi impArgs expArgs _ t endLoc -> do
      let impArgsWithDefaults = RT.extractImpArgsWithDefaults impArgs
      (impArgs', h') <- discernBinderWithDefaults h impArgsWithDefaults endLoc
      (expArgs', h'') <- discernBinder h' (RT.extractArgs expArgs) endLoc
      t' <- discern h'' t
      forM_ (map fst impArgs' ++ expArgs') $ \(_, x, _) -> liftIO (Unused.deleteVariable (H.unusedHandle h'') x)
      return $ m :< WT.Pi PK.normal impArgs' expArgs' t'
    m :< RT.PiIntro _ (RT.RawDef {geist, body, endLoc}) -> do
      lamID <- liftIO $ Gensym.newCount (H.gensymHandle h)
      let (name, _) = RT.name geist
      let impArgsWithDefaults = RT.extractImpArgsWithDefaults $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      (impArgs', h') <- discernBinderWithDefaults h impArgsWithDefaults endLoc
      (expArgs', h'') <- discernBinder h' expArgs endLoc
      codType' <- discern h'' $ snd $ RT.cod geist
      body' <- discern h'' body
      ensureLayerClosedness m h'' body'
      return $ m :< WT.PiIntro (AttrL.normal' name lamID codType') impArgs' expArgs' body'
    m :< RT.PiIntroFix _ (RT.RawDef {geist, body, endLoc}) -> do
      let impArgsWithDefaults = RT.extractImpArgsWithDefaults $ RT.impArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let mx = RT.loc geist
      let (x, _) = RT.name geist
      (impArgs', h') <- discernBinderWithDefaults h impArgsWithDefaults endLoc
      (expArgs', h'') <- discernBinder h' expArgs endLoc
      codType' <- discern h'' $ snd $ RT.cod geist
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h''' <- liftIO $ H.extend' h'' mx x' VDK.Normal
      body' <- discern h''' body
      let mxt' = (mx, x', codType')
      liftIO $ Tag.insertBinder (H.tagHandle h) mxt'
      lamID <- liftIO $ Gensym.newCount (H.gensymHandle h)
      ensureLayerClosedness m h''' body'
      return $ m :< WT.PiIntro (AttrL.Attr {lamKind = LK.Fix mxt', identity = lamID}) impArgs' expArgs' body'
    m :< RT.PiElim e _ expArgs -> do
      let isNoetic = False -- overwritten later in `infer`
      e' <- discern h e
      expArgs' <- mapM (discern h) $ SE.extract expArgs
      return $ m :< WT.PiElim isNoetic e' ImpArgs.Unspecified expArgs'
    m :< RT.PiElimByKey name _ kvs -> do
      (dd, (_, gn)) <- resolveName h m name
      _ :< func <- interpretGlobalName h m dd (GN.disableConstLikeFlag gn)
      let (ks, vs) = unzip $ map (\(_, k, _, _, v) -> (k, v)) $ SE.extract kvs
      ensureFieldLinearity m ks S.empty S.empty
      (impKeys, expKeys) <- KeyArg.lookup (H.keyArgHandle h) m dd
      vs' <- mapM (discern h) vs
      let kvs' = Map.fromList $ zip ks vs'
      let impArgs = resolveImpKeys h m impKeys kvs'
      expArgs <- resolveExpKeys h m expKeys kvs'
      checkRedundancy m impKeys expKeys kvs'
      let isNoetic = False -- overwritten later in `infer`
      return $ m :< WT.PiElim isNoetic (m :< func) (ImpArgs.PartiallySpecified impArgs) expArgs
    m :< RT.PiElimRule name _ es -> do
      (dd, (_, gn)) <- resolveName h m name
      kind <- interpretRuleName m dd gn
      let leafDD = DD.getLeafDD dd
      let nodeDD = DD.getNodeDD dd
      let rootDD = DD.getRootDD dd
      leafGN <- resolveDefiniteDescription h m leafDD
      nodeGN <- resolveDefiniteDescription h m nodeDD
      rootGN <- resolveDefiniteDescription h m rootDD
      let size = m :< RT.Int (toInteger $ length es)
      let leafTM = m :< RT.piElim (RT.force (m :< RT.VarGlobal leafDD leafGN)) [size]
      let nodeTM = RT.force (m :< RT.VarGlobal nodeDD nodeGN)
      let rootTM = RT.force (m :< RT.VarGlobal rootDD rootGN)
      let args = SE.extract es
      case kind of
        FoldLeft -> do
          foldedTerm <- buildFoldLeft nodeTM (leafTM : args)
          discern h (m :< RT.piElim rootTM [foldedTerm])
        FoldRight -> do
          foldedTerm <- buildFoldRight nodeTM (args ++ [leafTM])
          discern h (m :< RT.piElim rootTM [foldedTerm])
    m :< RT.PiElimExact _ e -> do
      e' <- discern h e
      return $ m :< WT.PiElimExact e'
    m :< RT.Data attr dataName es -> do
      es' <- mapM (discern h) es
      return $ m :< WT.Data attr dataName es'
    m :< RT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (discern h) dataArgs
      consArgs' <- mapM (discern h) consArgs
      return $ m :< WT.DataIntro attr consName dataArgs' consArgs'
    m :< RT.DataElim _ isNoetic es patternMatrix -> do
      let es' = SE.extract es
      let ms = map (\(me :< _) -> me) es'
      os <- liftIO $ mapM (const $ Gensym.newIdentFromText (H.gensymHandle h) "match") es' -- os: occurrences
      es'' <- mapM (discern h >=> liftIO . castFromNoemaIfNecessary h isNoetic) es'
      ts <- liftIO $ mapM (const $ WT.createHole (H.gensymHandle h) m []) es''
      patternMatrix' <- discernPatternMatrix h $ SE.extract patternMatrix
      ensurePatternMatrixSanity h patternMatrix'
      let os' = zip ms os
      decisionTree <- compilePatternMatrix h isNoetic (V.fromList os') patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es'' ts) decisionTree
    m :< RT.Box t -> do
      t' <- discern h t
      return $ m :< WT.Box t'
    m :< RT.BoxNoema t -> do
      t' <- discern h t
      return $ m :< WT.BoxNoema t'
    m :< RT.BoxIntro _ _ mxs (body, _) -> do
      xsOuter <- forM (SE.extract mxs) $ \(mx, x) -> discernIdent mx h x
      xets <- liftIO $ discernNoeticVarList h True xsOuter
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
      case nv of
        VariantK ->
          unless (SE.isEmpty mys) $ raiseError m "`on` cannot be used with: `letbox`"
        VariantT ->
          return ()
      tmp <- liftIO $ Gensym.newTextFromText (H.gensymHandle h) "tmp"
      let mxt = (mx, tmp, c1, c2, t)
      let m' = blur m
      let patParam = (mx, pat, [], [], t)
      let e2' = m' :< RT.Let (RT.Plain False) [] patParam [] [] (m' :< RT.Var (Var tmp)) [] startLoc [] e2 endLoc
      -- inner
      ysOuter <- forM (SE.extract mys) $ \(my, y) -> discernIdent my h y
      yetsInner <- liftIO $ discernNoeticVarList h True ysOuter
      let innerLayer = H.currentLayer h + layerOffset nv
      let ysInner = map (\((myUse, y, myDef :< _), _) -> (myDef, (myUse, y))) yetsInner
      let innerAddition = map (\(_, (myUse, y)) -> (Ident.toText y, (myUse, y, innerLayer))) ysInner
      hInner <- liftIO $ H.extendByNominalEnv (h {H.currentLayer = innerLayer}) VDK.Borrowed innerAddition
      e1' <- discern hInner e1
      -- cont
      yetsCont <- liftIO $ discernNoeticVarList h False ysInner
      let ysCont = map (\((myUse, y, _), _) -> (myUse, y)) yetsCont
      let contAddition = map (\(myUse, y) -> (Ident.toText y, (myUse, y, H.currentLayer h))) ysCont
      hCont <- liftIO $ H.extendByNominalEnv h VDK.Relayed contAddition
      (mxt', e2'') <- discernBinderWithBody' hCont mxt startLoc endLoc e2'
      when mustIgnoreRelayedVars $ do
        forM_ ysCont $ \(_, y) -> liftIO (Unused.deleteVariable (H.unusedHandle h) y)
      return $ m :< WT.BoxElim yetsInner mxt' e1' yetsCont e2''
    m :< RT.Embody e -> do
      embodyVar <- liftEither $ locatorToVarGlobal m coreBoxEmbody
      discern h $ m :< RT.piElim embodyVar [e]
    m :< RT.Let letKind _ (mx, pat, c1, c2, t) _ _ e1 _ startLoc _ e2 endLoc -> do
      discernLet h m letKind (mx, pat, c1, c2, t) e1 e2 startLoc endLoc
    m :< RT.LetOn letKind _ pat _ mys _ e1@(m1 :< _) _ startLoc _ e2 endLoc -> do
      case letKind of
        RT.Plain mustIgnoreRelayedVars -> do
          let e1' = m :< RT.BoxIntroQuote [] [] (e1, [])
          discern h $ m :< RT.BoxElim VariantT mustIgnoreRelayedVars [] pat [] mys [] e1' [] startLoc [] e2 endLoc
        RT.Noetic -> do
          raiseError m $ "`on` cannot be used with: `" <> RT.decodeLetKind letKind <> "`"
        RT.Try -> do
          tmpType <- liftIO $ RT.createHole (H.gensymHandle h) m1
          tmpVar <- liftIO $ Var <$> Gensym.newTextFromText (H.gensymHandle h) "tmp-try"
          discern h $
            m
              :< RT.LetOn
                (RT.Plain False)
                []
                (m1, RP.Var tmpVar, [], [], tmpType)
                []
                mys
                []
                e1
                []
                startLoc
                []
                (m :< RT.Let RT.Try [] pat [] [] (m :< RT.Var tmpVar) [] startLoc [] e2 endLoc)
                endLoc
    m :< RT.Pin _ mxt@(mx, x, _, _, t) _ mys _ e1 _ startLoc _ e2@(m2 :< _) endLoc -> do
      let m2' = blur m2
      let x' = SE.fromListWithComment Nothing SE.Comma [([], ((mx, x), []))]
      resultType <- liftIO $ RT.createHole (H.gensymHandle h) m2'
      resultVar <- liftIO $ Var <$> Gensym.newTextFromText (H.gensymHandle h) "tmp-pin"
      let resultParam = (m2', RP.Var resultVar, [], [], resultType)
      let isNoetic = not $ null $ SE.extract mys
      if isNoetic
        then do
          let mxt' = (mx, RP.Var (Var x), [], [], t)
          let outerLet cont = m :< RT.LetOn (RT.Plain False) [] mxt' [] mys [] e1 [] startLoc [] cont endLoc
          discern h $
            outerLet $
              m :< RT.LetOn (RT.Plain True) [] resultParam [] x' [] e2 [] startLoc [] (m2' :< RT.Var resultVar) endLoc
        else do
          discern h $
            bind startLoc endLoc mxt e1 $
              m :< RT.LetOn (RT.Plain True) [] resultParam [] x' [] e2 [] startLoc [] (m2' :< RT.Var resultVar) endLoc
    m :< RT.StaticText s str -> do
      s' <- discern h s
      case parseText str of
        Left reason ->
          raiseError m $ "Could not interpret the following as a text: " <> str <> "\nReason: " <> reason
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
    m :< RT.Resource dd _ (discarder, _) (copier, _) (typeTag, _) -> do
      unitType <- liftEither (locatorToVarGlobal m coreUnit) >>= discern h
      resourceID <- liftIO $ Gensym.newCount (H.gensymHandle h)
      discarder' <- discern h discarder
      copier' <- discern h copier
      typeTag' <- discern h typeTag
      return $ m :< WT.Resource dd resourceID unitType discarder' copier' typeTag'
    m :< RT.If ifClause elseIfClauseList (_, (elseBody, _)) -> do
      let (ifCond, ifBody) = RT.extractFromKeywordClause ifClause
      boolTrue <- liftEither $ locatorToName (blur m) coreBoolTrue
      boolFalse <- liftEither $ locatorToName (blur m) coreBoolFalse
      discern h $ foldIf m boolTrue boolFalse ifCond ifBody elseIfClauseList elseBody
    m :< RT.Seq (e1, _) _ e2 -> do
      hole <- liftIO $ Gensym.newTextForHole (H.gensymHandle h)
      unit <- liftEither $ locatorToVarGlobal m coreUnit
      discern h $ bind fakeLoc fakeLoc (m, hole, [], [], unit) e1 e2
    m :< RT.SeqEnd e1 -> do
      hole <- liftIO $ Gensym.newTextForHole (H.gensymHandle h)
      unit <- liftEither $ locatorToVarGlobal m coreUnit
      unitUnit <- liftEither $ locatorToVarGlobal m coreUnitUnit
      discern h $ bind fakeLoc fakeLoc (m, hole, [], [], unit) e1 unitUnit
    m :< RT.When whenClause -> do
      let (whenCond, whenBody) = RT.extractFromKeywordClause whenClause
      boolTrue <- liftEither $ locatorToName (blur m) coreBoolTrue
      boolFalse <- liftEither $ locatorToName (blur m) coreBoolFalse
      unitUnit <- liftEither $ locatorToVarGlobal m coreUnitUnit
      discern h $ foldIf m boolTrue boolFalse whenCond whenBody [] unitUnit
    m :< RT.Admit -> do
      panic <- liftEither $ locatorToVarGlobal m coreTrickPanic
      textType <- liftEither $ locatorToVarGlobal m coreText
      discern h $
        asOpaqueValue $
          m
            :< RT.Annotation
              L.Warning
              (AN.Type ())
              ( m
                  :< RT.piElim
                    panic
                    [m :< RT.StaticText textType ("Admitted: " <> T.pack (Hint.toString m) <> "\n")]
              )
    m :< RT.Detach _ _ (e, _) -> do
      t <- liftIO $ RT.createHole (H.gensymHandle h) (blur m)
      detachVar <- liftEither $ locatorToVarGlobal m coreThreadDetach
      cod <- liftIO $ RT.createHole (H.gensymHandle h) (blur m)
      discern h $ m :< RT.piElim detachVar [t, RT.lam fakeLoc m [] cod e]
    m :< RT.Attach _ _ (e, _) -> do
      t <- liftIO $ RT.createHole (H.gensymHandle h) (blur m)
      attachVar <- liftEither $ locatorToVarGlobal m coreThreadAttach
      discern h $ m :< RT.piElim attachVar [t, e]
    m :< RT.Option t -> do
      eitherVar <- liftEither $ locatorToVarGlobal m coreEither
      unit <- liftEither $ locatorToVarGlobal m coreUnit
      discern h $ m :< RT.piElim eitherVar [unit, t]
    m :< RT.Assert _ (mText, message) _ _ (e@(mCond :< _), _) -> do
      assert <- liftEither $ locatorToVarGlobal m coreTrickAssert
      textType <- liftEither $ locatorToVarGlobal m coreText
      let fullMessage = T.pack (Hint.toString m) <> "\nAssertion failure: " <> message <> "\n"
      cod <- liftIO $ RT.createHole (H.gensymHandle h) (blur m)
      discern h $
        m
          :< RT.piElim
            assert
            [mText :< RT.StaticText textType fullMessage, RT.lam fakeLoc mCond [] cod e]
    m :< RT.Introspect _ key _ clauseList -> do
      value <- getIntrospectiveValue h m key
      clause <- lookupIntrospectiveClause m value $ SE.extract clauseList
      discern h clause
    m :< RT.IncludeText _ _ mKey (key, _) -> do
      contentOrNone <- liftIO $ Locator.getStaticFileContent (H.locatorHandle h) key
      case contentOrNone of
        Just (path, content) -> do
          liftIO $ Unused.deleteStaticFile (H.unusedHandle h) key
          textType <- liftEither (locatorToVarGlobal m coreText) >>= discern h
          liftIO $ Tag.insertFileLoc (H.tagHandle h) mKey (T.length key) (newSourceHint path)
          return $ m :< WT.Prim (WP.Value $ WPV.StaticText textType content)
        Nothing ->
          raiseError m $ "No such static file is defined: `" <> key <> "`"
    _ :< RT.Brace _ (e, _) ->
      discern h e
    m :< RT.Int i -> do
      let intType = m :< WT.Prim (WP.Type $ PT.Int IntSize64)
      return $ m :< WT.Prim (WP.Value $ WPV.Int intType i)
    m :< RT.Pointer ->
      return $ m :< WT.Prim (WP.Type PT.Pointer)
    m :< RT.Void ->
      return $ m :< WT.Void

type ShouldInsertTagInfo =
  Bool

discernNoeticVarList ::
  H.Handle ->
  ShouldInsertTagInfo ->
  [(Hint, (Hint, Ident))] ->
  IO [(BinderF WT.WeakTerm, WT.WeakTerm)]
discernNoeticVarList h mustInsertTagInfo xsOuter = do
  forM xsOuter $ \(mDef, (mUse, outerVar)) -> do
    xInner <- Gensym.newIdentFromIdent (H.gensymHandle h) outerVar
    t <- WT.createHole (H.gensymHandle h) mUse []
    when mustInsertTagInfo $ do
      Tag.insertLocalVar (H.tagHandle h) mUse outerVar mDef
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
      unit <- liftEither (locatorToVarGlobal m coreUnit) >>= discern h
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
      mDef <- PreDecl.lookup (H.preDeclHandle h) m funcName
      liftIO $ Tag.insertExternalName (H.tagHandle h) mUse funcName mDef
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
    RT.CallType _ (_, (func, _)) (_, (arg1, _)) (_, (arg2, _)) -> do
      func' <- discern h func
      arg1' <- discern h arg1
      arg2' <- discern h arg2
      return $ M.WeakMagic $ M.CallType func' arg1' arg2'

modifyLetContinuation ::
  H.Handle ->
  (Hint, RP.RawPattern) ->
  N.IsNoetic ->
  RT.RawTerm ->
  App (RawIdent, RT.RawTerm)
modifyLetContinuation h pat isNoetic cont@(mCont :< _) =
  case pat of
    (_, RP.Var (Var x))
      | not (isConsName x) ->
          return (x, cont)
    _ -> do
      tmp <- liftIO $ Gensym.newTextForHole (H.gensymHandle h)
      return
        ( tmp,
          mCont
            :< RT.DataElim
              []
              isNoetic
              (SE.fromList'' [mCont :< RT.Var (Var tmp)])
              (SE.fromList SE.Brace SE.Bar [(SE.fromList'' [pat], [], cont)])
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

lookupIntrospectiveClause :: Hint -> T.Text -> [(Maybe T.Text, C, RT.RawTerm)] -> App RT.RawTerm
lookupIntrospectiveClause m value clauseList =
  case clauseList of
    [] ->
      raiseError m $ "This term does not support `" <> value <> "`."
    (Just key, _, clause) : rest
      | key == value ->
          return clause
      | otherwise ->
          lookupIntrospectiveClause m value rest
    (Nothing, _, clause) : _ ->
      return clause

getIntrospectiveValue :: H.Handle -> Hint -> T.Text -> App T.Text
getIntrospectiveValue h m key = do
  bm <- liftIO $ Env.getBuildMode (H.envHandle h)
  let p = Platform.getPlatform (H.platformHandle h)
  case key of
    "architecture" ->
      return $ Arch.reify (Platform.arch p)
    "operating-system" ->
      return $ OS.reify (Platform.os p)
    "build-mode" ->
      return $ BM.reify bm
    _ ->
      raiseError m $ "No such introspective value is defined: " <> key

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
                  ifBody
                ),
                ( SE.fromList'' [(blur m, RP.Var false)],
                  [],
                  elseBody
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
              [ (SE.fromList'' [(blur m, RP.Var true)], [], ifBody),
                (SE.fromList'' [(blur m, RP.Var false)], [], cont)
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
        (x, e2') <- modifyLetContinuation h (mx, pat) isNoetic e2
        (mxt', e2'') <- discernBinderWithBody' h (mx, x, c1, c2, t) startLoc endLoc e2'
        liftIO $ Tag.insertBinder (H.tagHandle h) mxt'
        return $ m :< WT.Let opacity mxt' e1' e2''
  case letKind of
    RT.Plain _ -> do
      discernLet' False
    RT.Noetic -> do
      discernLet' True
    RT.Try -> do
      let m' = blur m
      eitherTypeInner <- liftEither $ locatorToVarGlobal m' coreEither
      leftType <- liftIO $ RT.createHole (H.gensymHandle h) m'
      let eitherType = m' :< RT.piElim eitherTypeInner [leftType, t]
      e1' <- discern h e1
      tmpVar <- liftIO $ Gensym.newText (H.gensymHandle h)
      eitherCont <- constructEitherBinder h m mx m1 pat tmpVar e2
      (mxt', eitherCont') <- discernBinderWithBody' h (mx, tmpVar, c1, c2, eitherType) startLoc endLoc eitherCont
      return $ m :< WT.Let opacity mxt' e1' eitherCont'

constructEitherBinder ::
  H.Handle ->
  Hint ->
  Hint ->
  Hint ->
  RP.RawPattern ->
  RawIdent ->
  Cofree RT.RawTermF Hint ->
  App RT.RawTerm
constructEitherBinder h m mx m1 pat tmpVar cont = do
  let m' = blur m
  let mx' = blur mx
  let m1' = blur m1
  earlyRetVar <- liftIO $ Gensym.newText (H.gensymHandle h)
  eitherL <- liftEither $ locatorToName m1 coreEitherLeft
  eitherR <- liftEither $ locatorToName m1 coreEitherRight
  eitherVarL <- liftEither $ locatorToVarGlobal m1 coreEitherLeft
  let longClause =
        ( SE.fromList'' [(mx', RP.Cons eitherR [] (RP.Paren (SE.fromList' [(mx, pat)])))],
          [],
          cont
        )
  let shortClause =
        ( SE.fromList'' [(m', RP.Cons eitherL [] (RP.Paren (SE.fromList' [(m', RP.Var (Var earlyRetVar))])))],
          [],
          m' :< RT.piElim eitherVarL [m' :< RT.Var (Var earlyRetVar)]
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
      raiseError mUse $ "Undefined variable: " <> x
    Just (mDef, x', _) -> do
      liftIO $ Unused.deleteVariable (H.unusedHandle h) x'
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
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extend' h mx x' VDK.Normal
      (xts', h'') <- discernBinder h' xts endLoc
      liftIO $ Tag.insertBinder (H.tagHandle h'') (mx, x', t')
      liftIO $ SymLoc.insert (H.symLocHandle h'') x' (metaLocation mx) endLoc
      return ((mx, x', t') : xts', h'')

discernBinderWithDefaults ::
  H.Handle ->
  [(RawBinder RT.RawTerm, Maybe RT.RawTerm)] ->
  Loc ->
  App ([(BinderF WT.WeakTerm, Maybe WT.WeakTerm)], H.Handle)
discernBinderWithDefaults h binder endLoc =
  case binder of
    [] -> do
      return ([], h)
    ((mx, x, _, _, t), maybeDefault) : xts -> do
      t' <- discern h t
      maybeDefault' <- traverse (discern h {H.nameEnv = []}) maybeDefault -- default values must be closed
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extend' h mx x' VDK.Normal
      (xts', h'') <- discernBinderWithDefaults h' xts endLoc
      liftIO $ Tag.insertBinder (H.tagHandle h'') (mx, x', t')
      liftIO $ SymLoc.insert (H.symLocHandle h'') x' (metaLocation mx) endLoc
      return (((mx, x', t'), maybeDefault') : xts', h'')

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
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extend' h mx x' VDK.Normal
      (xts', h'') <- discernBinder' h' xts
      liftIO $ Tag.insertBinder (H.tagHandle h) (mx, x', t')
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
  x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
  h'' <- liftIO $ H.extend' h mx x' VDK.Normal
  e' <- discern h'' e
  liftIO $ SymLoc.insert (H.symLocHandle h'') x' startLoc endLoc
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
discernPatternRow h (patList, _, body) = do
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
  unless (null linearityErrors) $ throwError $ E.MakeError linearityErrors

getNonLinearOccurrences :: NominalEnv -> S.Set T.Text -> [(Hint, T.Text)] -> [L.Log]
getNonLinearOccurrences vars found nonLinear =
  case vars of
    [] -> do
      let nonLinearVars = reverse $ ListUtils.nubOrdOn snd nonLinear
      flip map nonLinearVars $ \(m, x) ->
        L.newLog m L.Error $
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
              (consDD, dataArgNum, consArgNum, disc, isConstLike, _) <- resolveConstructor h m $ Var x
              unless isConstLike $ do
                let mainModule = Env.getMainModule (H.envHandle h)
                let consDD' = readableDD mainModule consDD
                raiseError m $
                  "The constructor `" <> consDD' <> "` cannot be used as a constant"
              return ((m, PAT.Cons (PAT.ConsInfo {args = [], ..})), [])
          | otherwise -> do
              x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
              return ((m, PAT.Var x'), [(x, (m, x', layer))])
        Locator l -> do
          (dd, gn) <- resolveName h m $ Locator l
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
              let mainModule = Env.getMainModule (H.envHandle h)
              let dd' = readableDD mainModule dd
              raiseError m $
                "The symbol `" <> dd' <> "` is not defined as a constuctor"
    RP.Cons cons _ mArgs -> do
      (consName, dataArgNum, consArgNum, disc, isConstLike, _) <- resolveConstructor h m cons
      when isConstLike $
        raiseError m $
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
          ensureFieldLinearity m ks S.empty S.empty
          (_, expKeys) <- KeyArg.lookup (H.keyArgHandle h) m consName
          defaultKeyMap <- liftIO $ constructDefaultKeyMap h m expKeys
          let specifiedKeyMap = Map.fromList $ zip ks mvs
          let kvs' = Map.union specifiedKeyMap defaultKeyMap
          expArgs <- resolveExpKeys h m expKeys kvs'
          checkRedundancy m [] expKeys kvs'
          (patList', hList) <- mapAndUnzipM (discernPattern h layer) expArgs
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
    RP.RuneIntro r -> do
      return ((m, PAT.Literal (LI.Rune r)), [])

constructDefaultKeyMap :: H.Handle -> Hint -> [Key] -> IO (Map.HashMap Key (Hint, RP.RawPattern))
constructDefaultKeyMap h m keyList = do
  names <- mapM (const $ Gensym.newTextForHole (H.gensymHandle h)) keyList
  return $ Map.fromList $ zipWith (\k v -> (k, (m, RP.Var (Var v)))) keyList names

locatorToName :: Hint -> T.Text -> Either E.Error Name
locatorToName m text = do
  (gl, ll) <- DD.getLocatorPair m text
  return $ Locator (gl, ll)

locatorToVarGlobal :: Hint -> T.Text -> Either E.Error RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- DD.getLocatorPair (blur m) text
  return $ blur m :< RT.Var (Locator (gl, ll))

getLayer :: Hint -> H.Handle -> Ident -> App Layer
getLayer m h x =
  case lookup (Ident.toText x) (H.nameEnv h) of
    Nothing ->
      raiseCritical m $ "Scene.Parse.Discern.getLayer: Undefined variable: " <> Ident.toText x
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
      raiseError mClosure $
        "This function is at the layer "
          <> T.pack (show (H.currentLayer h))
          <> ", but the free variable `"
          <> Ident.toText x
          <> "` is at the layer "
          <> T.pack (show l)
          <> " (> "
          <> T.pack (show (H.currentLayer h))
          <> ")"

raiseLayerError :: Hint -> Layer -> Layer -> App a
raiseLayerError m expected found = do
  raiseError m $
    "Expected layer:\n  "
      <> T.pack (show expected)
      <> "\nFound layer:\n  "
      <> T.pack (show found)

asOpaqueValue :: RT.RawTerm -> RT.RawTerm
asOpaqueValue e@(m :< _) =
  m :< RT.Magic [] (RT.OpaqueValue [] ([], (e, [])))

interpretForeign :: H.Handle -> [RawForeignItemF WT.WeakTerm] -> IO [WT.WeakForeign]
interpretForeign h foreignItemList = do
  mapM (interpretForeignItem h) foreignItemList

interpretForeignItem :: H.Handle -> RawForeignItemF WT.WeakTerm -> IO WT.WeakForeign
interpretForeignItem h (RawForeignItemF m name _ lts _ _ cod) = do
  let lts' = SE.extract lts
  Tag.insertExternalName (H.tagHandle h) m name m
  PreDecl.insert (H.preDeclHandle h) name m
  return $ F.Foreign m name lts' cod

resolveImpKeys :: H.Handle -> Hint -> [ImpKey] -> Map.HashMap Key WT.WeakTerm -> [Maybe WT.WeakTerm]
resolveImpKeys h m impKeys kvs = do
  case impKeys of
    [] ->
      []
    impKey : rest -> do
      let args = resolveImpKeys h m rest kvs
      case Map.lookup impKey kvs of
        Just value -> do
          Just value : args
        Nothing -> do
          Nothing : args

resolveExpKeys :: H.Handle -> Hint -> [ExpKey] -> Map.HashMap Key a -> App [a]
resolveExpKeys h m expKeys kvs = do
  case expKeys of
    [] ->
      return []
    expKey : rest -> do
      args <- resolveExpKeys h m rest kvs
      case Map.lookup expKey kvs of
        Just value -> do
          return $ value : args
        Nothing -> do
          raiseError m $ "The field `" <> expKey <> "` is missing"

checkRedundancy :: Hint -> [ImpKey] -> [ExpKey] -> Map.HashMap Key a -> App ()
checkRedundancy m impKeys expKeys kvs = do
  let keys = Map.keys kvs
  let diff = keys \\ (impKeys ++ expKeys)
  if null diff
    then return ()
    else raiseError m $ "The following field(s) are redundant:\n" <> _showKeyList diff

buildFoldRight :: RT.RawTerm -> [RT.RawTerm] -> App RT.RawTerm
buildFoldRight func@(mFunc :< _) argList =
  case argList of
    [] -> do
      raiseCritical mFunc "`buildFoldRight` requires at least one argument"
    [lastArg] ->
      return lastArg
    (firstArg : restArgs) -> do
      restTerm <- buildFoldRight func restArgs
      return $ mFunc :< RT.piElim func [firstArg, restTerm]

buildFoldLeft :: RT.RawTerm -> [RT.RawTerm] -> App RT.RawTerm
buildFoldLeft func@(mFunc :< _) argList =
  case argList of
    [] -> do
      raiseCritical mFunc "`buildFoldLeft` requires at least one argument"
    [firstArg] ->
      return firstArg
    (firstArg : secondArg : restArgs) -> do
      let headTerm = mFunc :< RT.piElim func [firstArg, secondArg]
      buildFoldLeft func $ headTerm : restArgs
