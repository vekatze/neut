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
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Gensym.Gensym qualified as Gensym
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.BuildMode qualified as BM
import Kernel.Common.Const
import Kernel.Common.GlobalName qualified as GN
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.KeyArg (DefaultKey, ExpKey, _showKeyList)
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
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.NameMap qualified as NameMap
import Kernel.Parse.Internal.Handle.PreDecl qualified as PreDecl
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.Internal.Util
import Kernel.Parse.Layer
import Kernel.Parse.NominalEnv
import Kernel.Parse.Pattern qualified as PAT
import Kernel.Parse.Stage
import Kernel.Parse.VarDefKind qualified as VDK
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign qualified as F
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Geist qualified as G
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.LamKind qualified as LK
import Language.Common.Literal qualified as LI
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Common.ModuleAlias (coreModuleAlias)
import Language.Common.Noema qualified as N
import Language.Common.PiElimKind qualified as PEK
import Language.Common.PiKind qualified as PK
import Language.Common.PrimNumSize (IntSize (IntSize64))
import Language.Common.PrimType qualified as PT
import Language.Common.RuleKind (RuleKind (FoldLeft, FoldRight))
import Language.Common.StmtKind qualified as SK
import Language.Common.Text.Util
import Language.Common.VarKind qualified as VK
import Language.RawTerm.CreateHole qualified as RT
import Language.RawTerm.Key
import Language.RawTerm.Name
import Language.RawTerm.NecessityVariant
import Language.RawTerm.RawBinder
import Language.RawTerm.RawIdent hiding (isHole)
import Language.RawTerm.RawPattern qualified as RP
import Language.RawTerm.RawStmt
import Language.RawTerm.RawTerm (CodeVariant (CodeVariantK))
import Language.RawTerm.RawTerm qualified as RT
import Language.WeakTerm.CreateHole qualified as WT
import Language.WeakTerm.FreeVars (freeVars, freeVarsType)
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
    PostRawStmtDefineTerm _ stmtKind (RT.RawDef {geist, body, endLoc}) -> do
      registerTopLevelName h stmt
      let baseStage = if SK.isMacroStmtKind stmtKind then 1 else 0
      let h' = h {H.currentStage = baseStage}
      let impArgs = RT.extractImpArgs $ RT.impArgs geist
      let defaultArgs = SE.extract $ fst $ RT.defaultArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let (_, codType) = RT.cod geist
      let m = RT.loc geist
      let functionName = fst $ RT.name geist
      let isConstLike = RT.isConstLike geist
      (impArgs', nenv) <- discernImpArgs h' impArgs endLoc
      (expArgs', nenv') <- discernBinder nenv expArgs endLoc
      (defaultArgs', nenv'') <- discernBinderWithDefaultArgs nenv' defaultArgs endLoc
      codType' <- discernType nenv'' codType
      stmtKind' <- discernStmtKindTerm h' stmtKind m
      body' <- discern nenv'' body
      liftIO $ Tag.insertGlobalVar (H.tagHandle h') m functionName isConstLike m
      when (metaShouldSaveLocation m) $ do
        liftIO $ TopCandidate.insert (H.topCandidateHandle h') $ do
          TopCandidate {loc = metaLocation m, dd = functionName, kind = toCandidateKindTerm stmtKind'}
      liftIO $ forM_ impArgs' $ Tag.insertBinder (H.tagHandle h')
      liftIO $ forM_ expArgs' $ Tag.insertBinder (H.tagHandle h')
      liftIO $ forM_ (map fst defaultArgs') $ Tag.insertBinder (H.tagHandle h')
      return [WeakStmtDefineTerm isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' body']
    PostRawStmtDefineType _ stmtKind (RT.RawTypeDef {typeGeist, typeBody, typeEndLoc}) -> do
      registerTopLevelName h stmt
      let h' = h {H.currentStage = 0}
      let impArgs = RT.extractImpArgs $ RT.impArgs typeGeist
      let defaultArgs = SE.extract $ fst $ RT.defaultArgs typeGeist
      let expArgs = RT.extractArgs $ RT.expArgs typeGeist
      let (_, codType) = RT.cod typeGeist
      let m = RT.loc typeGeist
      let functionName = fst $ RT.name typeGeist
      let isConstLike = RT.isConstLike typeGeist
      (impArgs', nenv) <- discernImpArgs h' impArgs typeEndLoc
      (expArgs', nenv') <- discernTypeBinder nenv expArgs typeEndLoc
      (defaultArgs', nenv'') <- discernTypeBinderWithDefaultArgs nenv' defaultArgs typeEndLoc
      codType' <- discernType nenv'' codType
      stmtKind' <- discernStmtKindType h' stmtKind m
      body' <- discernType nenv'' typeBody
      liftIO $ Tag.insertGlobalVar (H.tagHandle h') m functionName isConstLike m
      when (metaShouldSaveLocation m) $ do
        liftIO $ TopCandidate.insert (H.topCandidateHandle h') $ do
          TopCandidate {loc = metaLocation m, dd = functionName, kind = toCandidateKindType stmtKind'}
      liftIO $ forM_ impArgs' $ Tag.insertBinder (H.tagHandle h')
      liftIO $ forM_ expArgs' $ Tag.insertBinder (H.tagHandle h')
      liftIO $ forM_ (map fst defaultArgs') $ Tag.insertBinder (H.tagHandle h')
      return [WeakStmtDefineType isConstLike stmtKind' m functionName impArgs' expArgs' defaultArgs' codType' body']
    PostRawStmtDefineResource _ m (dd, _) (_, discarder) (_, copier) (_, resourceSize) _ -> do
      registerTopLevelName h stmt
      unitType <- liftEither (locatorToTypeVar m coreUnit) >>= discernType h
      resourceID <- liftIO $ Gensym.newCount (H.gensymHandle h)
      discarder' <- discern h discarder
      copier' <- discern h copier
      resourceSize' <- discern h resourceSize
      liftIO $ Tag.insertGlobalVar (H.tagHandle h) m dd True m
      liftIO $ TopCandidate.insert (H.topCandidateHandle h) $ do
        TopCandidate {loc = metaLocation m, dd = dd, kind = Constant}
      return [WeakStmtDefineResource m dd resourceID unitType discarder' copier' resourceSize']
    PostRawStmtVariadic kind m dd -> do
      registerTopLevelName h stmt
      liftIO $ Tag.insertGlobalVar (H.tagHandle h) m dd True m
      liftIO $ TopCandidate.insert (H.topCandidateHandle h) $ do
        TopCandidate {loc = metaLocation m, dd = dd, kind = Function}
      return [WeakStmtVariadic kind m dd]
    PostRawStmtNominal _ m geistList -> do
      geistList' <- forM (SE.extract geistList) $ \(tag, geist, endLoc) -> do
        NameMap.registerGeist (H.nameMapHandle h) tag geist
        geist' <- discernGeist h endLoc geist
        return (tag, geist')
      return [WeakStmtNominal m geistList']
    PostRawStmtForeign _ foreignList -> do
      let foreignList' = SE.extract foreignList
      foreignList'' <- mapM (mapM (discernType h)) foreignList'
      foreign' <- liftIO $ interpretForeign h foreignList''
      return [WeakStmtForeign foreign']

discernGeist :: H.Handle -> Loc -> RT.RawGeist DD.DefiniteDescription -> App (G.Geist WT.WeakType WT.WeakTerm)
discernGeist h endLoc geist = do
  let impArgs = RT.extractImpArgs $ RT.impArgs geist
  let defaultArgs = SE.extract $ fst $ RT.defaultArgs geist
  let expArgs = RT.extractArgs $ RT.expArgs geist
  (impArgs', h') <- discernImpArgs h impArgs endLoc
  (expArgs', h'') <- discernBinder h' expArgs endLoc
  (defaultArgs', h''') <- discernBinderWithDefaultArgs h'' defaultArgs endLoc
  forM_ (impArgs' ++ expArgs' ++ map fst defaultArgs') $ \(_, _, x, _) ->
    liftIO $ Unused.deleteVariable (H.unusedHandle h) x
  cod' <- discernType h''' $ snd $ RT.cod geist
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
        defaultArgs = defaultArgs',
        expArgs = expArgs',
        cod = cod'
      }

registerTopLevelName :: H.Handle -> PostRawStmt -> App ()
registerTopLevelName h stmt = do
  let arrow = NameMap.getGlobalNames [stmt]
  NameMap.insert (H.nameMapHandle h) arrow

discernStmtKindTerm :: H.Handle -> RawStmtKindTerm DD.DefiniteDescription -> Hint -> App (SK.StmtKindTerm WT.WeakType)
discernStmtKindTerm h stmtKind m =
  case stmtKind of
    SK.Define ->
      return SK.Define
    SK.DestPassing ->
      return SK.DestPassing
    SK.DestPassingInline ->
      return SK.DestPassingInline
    SK.Inline ->
      return SK.Inline
    SK.Constant ->
      return SK.Constant
    SK.Macro ->
      return SK.Macro
    SK.MacroInline ->
      return SK.MacroInline
    SK.Main _ -> do
      unitType <- getUnitType h m
      return $ SK.Main unitType
    SK.DataIntro dataName dataArgs expConsArgs discriminant -> do
      (dataArgs', h') <- discernTypeBinder' h dataArgs
      (expConsArgs', h'') <- discernBinder' h' expConsArgs
      forM_ (H.nameEnv h'') $ \(_, (_, newVar, _, _)) -> do
        liftIO $ Unused.deleteVariable (H.unusedHandle h'') newVar
      forM_ dataArgs' $ \(_, _, x, _) -> do
        liftIO $ Unused.deleteVariable (H.unusedHandle h'') x
      return $ SK.DataIntro dataName dataArgs' expConsArgs' discriminant

discernStmtKindType :: H.Handle -> RawStmtKindType DD.DefiniteDescription -> Hint -> App (SK.StmtKindType WT.WeakType)
discernStmtKindType h stmtKind _m =
  case stmtKind of
    SK.Alias ->
      return SK.Alias
    SK.AliasOpaque ->
      return SK.AliasOpaque
    SK.Data dataName dataArgs consInfoList -> do
      (dataArgs', h') <- discernTypeBinder' h dataArgs
      let (locList, consNameList, isConstLikeList, consArgsList, discriminantList) = List.unzip5 consInfoList
      (consArgsList', hList) <- mapAndUnzipM (discernBinder' h') consArgsList
      forM_ (concatMap H.nameEnv hList) $ \(_, (_, newVar, _, _)) -> do
        liftIO $ Unused.deleteVariable (H.unusedHandle h') newVar
      let consNameList' = consNameList
      let consInfoList' = List.zip5 locList consNameList' isConstLikeList consArgsList' discriminantList
      return $ SK.Data dataName dataArgs' consInfoList'

getUnitType :: H.Handle -> Hint -> App WT.WeakType
getUnitType h m = do
  unitType <- liftEither $ locatorToTypeVar m coreUnit
  discernType h unitType

toCandidateKindTerm :: SK.StmtKindTerm a -> CandidateKind
toCandidateKindTerm stmtKind =
  case stmtKind of
    SK.Define ->
      Function
    SK.DestPassing ->
      Function
    SK.DestPassingInline ->
      Function
    SK.Inline ->
      Function
    SK.Constant ->
      Function
    SK.Macro ->
      Function
    SK.MacroInline ->
      Function
    SK.Main {} ->
      Function
    SK.DataIntro {} ->
      Constructor

toCandidateKindType :: SK.StmtKindType a -> CandidateKind
toCandidateKindType stmtKind =
  case stmtKind of
    SK.Alias ->
      Function
    SK.AliasOpaque ->
      Function
    SK.Data {} ->
      Function

discern :: H.Handle -> RT.RawTerm -> App WT.WeakTerm
discern h term =
  case term of
    m :< RT.Var name ->
      case name of
        Var s
          | Just x <- readIntDecimalMaybe s -> do
              hole <- liftIO $ WT.createTypeHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WPV.Int hole x)
          | Just x <- readIntBinaryMaybe s -> do
              hole <- liftIO $ WT.createTypeHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WPV.Int hole x)
          | Just x <- readIntOctalMaybe s -> do
              hole <- liftIO $ WT.createTypeHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WPV.Int hole x)
          | Just x <- readIntHexadecimalMaybe s -> do
              hole <- liftIO $ WT.createTypeHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WPV.Int hole x)
          | Just x <- R.readMaybe (T.unpack s) -> do
              hole <- liftIO $ WT.createTypeHole (H.gensymHandle h) m []
              return $ m :< WT.Prim (WPV.Float hole x)
          | Just (mDef, name', layer, stage) <- lookup s (H.nameEnv h) -> do
              case (layer == H.currentLayer h, stage == H.currentStage h) of
                (True, True) -> do
                  liftIO $ Unused.deleteVariable (H.unusedHandle h) name'
                  liftIO $ Tag.insertLocalVar (H.tagHandle h) m name' mDef
                  return $ m :< WT.Var name'
                (False, _) ->
                  raiseLayerError m (H.currentLayer h) layer
                (_, False) ->
                  raiseStageError m (H.currentStage h) stage
        _ -> do
          (dd, (_, gn)) <- resolveName h m name
          interpretGlobalName h m dd gn
    m :< RT.VarGlobal dd gn -> do
      interpretGlobalName h m dd gn
    m :< RT.PiIntro _ (RT.RawDef {geist, body, endLoc}) -> do
      lamID <- liftIO $ Gensym.newCount (H.gensymHandle h)
      let (name, _) = RT.name geist
      let impArgs = RT.extractImpArgs $ RT.impArgs geist
      let defaultArgs = SE.extract $ fst $ RT.defaultArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      (impArgs', h') <- discernImpArgs h impArgs endLoc
      (expArgs', h'') <- discernBinder h' expArgs endLoc
      (defaultArgs', h''') <- discernBinderWithDefaultArgs h'' defaultArgs endLoc
      codType' <- discernType h''' $ snd $ RT.cod geist
      body' <- discern h''' body
      ensureLayerClosedness m h''' body'
      let isDestPassing = RT.isDestPassing geist
      return $ m :< WT.PiIntro (AttrL.normal' name isDestPassing lamID codType') impArgs' expArgs' defaultArgs' body'
    m :< RT.PiIntroFix opacity _ (RT.RawDef {geist, body, endLoc}) -> do
      let isDestPassing = RT.isDestPassing geist
      let impArgs = RT.extractImpArgs $ RT.impArgs geist
      let defaultArgs = SE.extract $ fst $ RT.defaultArgs geist
      let expArgs = RT.extractArgs $ RT.expArgs geist
      let mx = RT.loc geist
      let (x, _) = RT.name geist
      (impArgs', h') <- discernImpArgs h impArgs endLoc
      (expArgs', h'') <- discernBinder h' expArgs endLoc
      (defaultArgs', h''') <- discernBinderWithDefaultArgs h'' defaultArgs endLoc
      codType' <- discernType h''' $ snd $ RT.cod geist
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h'''' <- liftIO $ H.extend' h''' mx x' VDK.Normal
      liftIO $ Unused.deleteVariable (H.unusedHandle h) x'
      body' <- discern h'''' body
      let mxt' = (mx, VK.Normal, x', codType')
      liftIO $ Tag.insertBinder (H.tagHandle h) mxt'
      lamID <- liftIO $ Gensym.newCount (H.gensymHandle h)
      ensureLayerClosedness m h'''' body'
      return $ m :< WT.PiIntro (AttrL.Attr {lamKind = LK.Fix opacity isDestPassing mxt', identity = lamID}) impArgs' expArgs' defaultArgs' body'
    m :< RT.PiElim e _ mImpArgs _ expArgs _ mDefaultArgs -> do
      let kind = PEK.Normal -- overwritten later in `infer`
      e' <- discern h e
      impArgs' <- case mImpArgs of
        Nothing ->
          return ImpArgs.Unspecified
        Just impArgs -> do
          impArgs' <- mapM (discernType h) $ SE.extract impArgs
          return $ ImpArgs.FullySpecified impArgs'
      expArgs' <- mapM (discern h) $ SE.extract expArgs
      defaultArgs' <- case mDefaultArgs of
        Nothing ->
          return $ DefaultArgs.ByKey []
        Just defaultArgs -> do
          let (ks, vs) = unzip $ map (\(_, k, _, _, v) -> (k, v)) $ SE.extract defaultArgs
          ensureFieldLinearity m ks S.empty S.empty
          vs' <- mapM (discern h) vs
          return $ DefaultArgs.ByKey (zip ks vs')
      return $ m :< WT.PiElim kind e' impArgs' expArgs' defaultArgs'
    m :< RT.PiElimByKey name _ mImpArgs _ kvs -> do
      let kind = PEK.Normal -- overwritten later in `infer`
      (dd, (_, gn)) <- resolveName h m name
      _ :< func <- interpretGlobalName h m dd (GN.disableConstLikeFlag gn)
      let (ks, vs) = unzip $ map (\(_, k, _, _, v) -> (k, v)) $ SE.extract kvs
      ensureFieldLinearity m ks S.empty S.empty
      (impKeys, expKeys, defaultKeys) <- KeyArg.lookup (H.keyArgHandle h) m dd
      when (any (`elem` impKeys) ks) $ do
        raiseError m "Implicit key arguments must be specified via explicit type arguments"
      impArgs' <- case mImpArgs of
        Nothing ->
          return ImpArgs.Unspecified
        Just impArgs -> do
          impArgs' <- mapM (discernType h) $ SE.extract impArgs
          return $ ImpArgs.FullySpecified impArgs'
      let keyMap = Map.fromList $ zip ks (repeat ())
      checkRedundancy m (expKeys ++ defaultKeys) keyMap
      vs' <- mapM (discern h) vs
      let expKvs = Map.fromList $ zip ks vs'
      expArgs <- resolveExpKeys h m expKeys expKvs
      let defaultArgs = selectDefaultKeyArgs defaultKeys expKvs
      return $ m :< WT.PiElim kind (m :< func) impArgs' expArgs defaultArgs
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
    m :< RT.PiElimMeta name _ mImpArgs _ es -> do
      let var = m :< RT.Var name
      let args = fmap (\e -> m :< RT.CodeIntro CodeVariantK [] [] (e, [])) es
      discern h $ m :< RT.CodeElim [] [] (m :< RT.PiElim var [] mImpArgs [] args [] Nothing, [])
    m :< RT.PiElimExact _ e -> do
      e' <- discern h e
      return $ m :< WT.PiElimExact e'
    m :< RT.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (discernType h) dataArgs
      consArgs' <- mapM (discern h) consArgs
      let allowedVars = S.unions $ map freeVarsType dataArgs'
      let nameEnv' = filter (\(_, (_, x, _, _)) -> S.member x allowedVars) (H.typeNameEnv h)
      let hAttr = h {H.typeNameEnv = nameEnv'}
      attr' <- discernAttrDataIntro hAttr attr
      return $ m :< WT.DataIntro attr' consName dataArgs' consArgs'
    m :< RT.DataElim _ isNoetic es patternMatrix -> do
      let es' = SE.extract es
      let ms = map (\(me :< _) -> me) es'
      os <- liftIO $ mapM (const $ Gensym.newIdentFromText (H.gensymHandle h) "match") es' -- os: occurrences
      es'' <- mapM (discern h >=> liftIO . castFromNoemaIfNecessary h isNoetic) es'
      ts <- liftIO $ mapM (const $ WT.createTypeHole (H.gensymHandle h) m []) es''
      patternMatrix' <- discernPatternMatrix h $ SE.extract patternMatrix
      ensurePatternMatrixSanity h patternMatrix'
      let os' = zip ms os
      decisionTree <- compilePatternMatrix h isNoetic (V.fromList os') patternMatrix'
      return $ m :< WT.DataElim isNoetic (zip3 os es'' ts) decisionTree
    m :< RT.BoxIntro _ _ mxs (body, _) -> do
      ensureRuntimeStage m h "meta operation (`box`)"
      xsOuter <- forM (SE.extract mxs) $ \(mx, k, x) -> do
        (mDef, (mUse, x')) <- discernIdent mx h x
        return (mDef, (mUse, k, x'))
      xets <- liftIO $ discernNoeticVarList h True xsOuter
      let innerLayer = H.currentLayer h - 1
      let innerStage = H.currentStage h
      let xsInner = map (\((mx, _, x, _), _) -> (mx, x)) xets
      let innerAddition = map (\(mx, x) -> (Ident.toText x, (mx, x, innerLayer, innerStage))) xsInner
      hInner <- liftIO $ H.extendByNominalEnv (h {H.currentLayer = innerLayer}) VDK.Borrowed innerAddition
      body' <- discern hInner body
      return $ m :< WT.BoxIntro xets body'
    m :< RT.BoxIntroLift _ _ (body, _) -> do
      ensureRuntimeStage m h "meta operation (`lift`)"
      body' <- discern h body
      return $ m :< WT.BoxIntroLift body'
    m :< RT.BoxElim nv mustIgnoreRelayedVars _ (mx, pat, c1, c2, t) _ mys _ e1 _ startLoc _ e2 endLoc -> do
      ensureRuntimeStage m h "meta operation (`letbox`)"
      case nv of
        VariantK ->
          unless (SE.isEmpty mys) $ raiseError m "`on` cannot be used with: `letbox`"
        VariantT ->
          return ()
      tmp <- liftIO $ Gensym.newTextFromText (H.gensymHandle h) "tmp"
      let mxt = (mx, VK.Normal, tmp, c1, c2, t)
      let m' = blur m
      let patParam = (mx, pat, [], [], t)
      let e2' = m' :< RT.Let (RT.Plain False) [] patParam [] [] (m' :< RT.Var (Var tmp)) [] startLoc [] e2 endLoc
      -- inner
      ysOuter <- forM (SE.extract mys) $ \(my, k, y) -> do
        (mDef, (mUse, y')) <- discernIdent my h y
        return (mDef, (mUse, k, y'))
      yetsInner <- liftIO $ discernNoeticVarList h True ysOuter
      let innerLayer = H.currentLayer h + layerOffset nv
      let innerStage = H.currentStage h
      let ysInner = map (\((myUse, k, y, myDef :< _), _) -> (myDef, (myUse, k, y))) yetsInner
      let innerAddition = map (\(_, (myUse, _, y)) -> (Ident.toText y, (myUse, y, innerLayer, innerStage))) ysInner
      hInner <- liftIO $ H.extendByNominalEnv (h {H.currentLayer = innerLayer}) VDK.Borrowed innerAddition
      e1' <- discern hInner e1
      -- cont
      yetsCont <- liftIO $ discernNoeticVarList h False ysInner
      let ysCont = map (\((myUse, _, y, _), _) -> (myUse, y)) yetsCont
      let contAddition = map (\(myUse, y) -> (Ident.toText y, (myUse, y, H.currentLayer h, H.currentStage h))) ysCont
      hCont <- liftIO $ H.extendByNominalEnv h VDK.Relayed contAddition
      (mxt', e2'') <- discernBinderWithBody' hCont mxt startLoc endLoc e2'
      when mustIgnoreRelayedVars $ do
        forM_ ysCont $ \(_, y) -> liftIO (Unused.deleteVariable (H.unusedHandle h) y)
      return $ m :< WT.BoxElim yetsInner mxt' e1' yetsCont e2''
    m :< RT.CodeIntro codeVariant _ _ (body, _) -> do
      let offset = case codeVariant of RT.CodeVariantK -> 1; RT.CodeVariantC -> 0
      let innerStage = H.currentStage h - offset
      let hInner = h {H.currentStage = innerStage}
      body' <- discern hInner body
      return $ m :< WT.CodeIntro body'
    m :< RT.CodeElim _ _ (body, _) -> do
      let innerStage = H.currentStage h + 1
      let hInner = h {H.currentStage = innerStage}
      body' <- discern hInner body
      return $ m :< WT.CodeElim body'
    m :< RT.TauIntro _ (_, (ty, _)) -> do
      ty' <- discernType h ty
      return $ m :< WT.TauIntro ty'
    m :< RT.TauElim _ (mx, x, _) _ e1 _ _ _ e2 _ -> do
      e1' <- discern h e1
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extendType' h mx x' VDK.Normal
      e2' <- discern h' e2
      return $ m :< WT.TauElim (mx, x') e1' e2'
    m :< RT.Embody e -> do
      ensureRuntimeStage m h "meta operation (`*`)"
      embodyVar <- liftEither $ locatorToVarGlobal m coreBoxEmbody
      discern h $ m :< RT.piElim embodyVar [e]
    m :< RT.Let letKind _ (mx, pat, c1, c2, t) _ _ e1 _ startLoc _ e2 endLoc -> do
      discernLet h m letKind (mx, pat, c1, c2, t) e1 e2 startLoc endLoc
    m :< RT.LetOn letKind _ pat _ mys _ e1@(m1 :< _) _ startLoc _ e2 endLoc -> do
      ensureRuntimeStage m h "meta operation (`on`)"
      case letKind of
        RT.Plain mustIgnoreRelayedVars -> do
          let e1' = m :< RT.BoxIntroLift [] [] (e1, [])
          discern h $ m :< RT.BoxElim VariantT mustIgnoreRelayedVars [] pat [] mys [] e1' [] startLoc [] e2 endLoc
        RT.Noetic -> do
          raiseError m $ "`on` cannot be used with: `" <> RT.decodeLetKind letKind <> "`"
        RT.Bind -> do
          raiseError m $ "`on` cannot be used with: `" <> RT.decodeLetKind letKind <> "`"
        RT.Try -> do
          tmpType <- liftIO $ RT.createTypeHole (H.gensymHandle h) m1
          tmpVar <- liftIO $ Var <$> Gensym.newTextFromText (H.gensymHandle h) "tmp-try"
          discern h $
            m
              :< RT.LetOn
                (RT.Plain False)
                []
                (m1, RP.Var VK.Normal tmpVar, [], [], tmpType)
                []
                mys
                []
                e1
                []
                startLoc
                []
                (m :< RT.Let RT.Try [] pat [] [] (m :< RT.Var tmpVar) [] startLoc [] e2 endLoc)
                endLoc
    m :< RT.Pin _ mxt@(mx, _k, x, _, _, t) _ mys _ e1 _ startLoc _ e2@(m2 :< _) endLoc -> do
      let m2' = blur m2
      let x' = SE.fromListWithComment Nothing SE.Comma [([], ((mx, VK.Normal, x), []))]
      resultType <- liftIO $ RT.createTypeHole (H.gensymHandle h) m2'
      resultVar <- liftIO $ Var <$> Gensym.newTextFromText (H.gensymHandle h) "tmp-pin"
      let resultParam = (m2', RP.Var VK.Normal resultVar, [], [], resultType)
      let isNoetic = not $ null $ SE.extract mys
      if isNoetic
        then do
          let mxt' = (mx, RP.Var VK.Normal (Var x), [], [], t)
          let outerLet cont = m :< RT.LetOn (RT.Plain False) [] mxt' [] mys [] e1 [] startLoc [] cont endLoc
          discern h $
            outerLet $
              m :< RT.LetOn (RT.Plain True) [] resultParam [] x' [] e2 [] startLoc [] (m2' :< RT.Var resultVar) endLoc
        else do
          discern h $
            bind startLoc endLoc mxt e1 $
              m :< RT.LetOn (RT.Plain True) [] resultParam [] x' [] e2 [] startLoc [] (m2' :< RT.Var resultVar) endLoc
    m :< RT.StaticText s str -> do
      s' <- discernType h s
      case parseText str of
        Left reason ->
          raiseError m $ "Could not interpret the following as a text: " <> str <> "\nReason: " <> reason
        Right str' -> do
          return $ m :< WT.Prim (WPV.StaticText s' str')
    m :< RT.RuneIntro _ r -> do
      return $ m :< WT.Prim (WPV.Rune r)
    m :< RT.Magic _ magic -> do
      magic' <- discernMagic h m magic
      return $ m :< WT.Magic magic'
    m :< RT.Annotation remarkLevel annot e -> do
      e' <- discern h e
      case annot of
        AN.Type _ ->
          return $ m :< WT.Annotation remarkLevel (AN.Type (doNotCare m)) e'
    m :< RT.If ifClause elseIfClauseList (_, (elseBody, _)) -> do
      let (ifCond, ifBody) = RT.extractFromKeywordClause ifClause
      boolTrue <- liftEither $ locatorToName (blur m) coreBoolTrue
      boolFalse <- liftEither $ locatorToName (blur m) coreBoolFalse
      discern h $ foldIf m boolTrue boolFalse ifCond ifBody elseIfClauseList elseBody
    m :< RT.Seq (e1, _) _ e2 -> do
      hole <- liftIO $ Gensym.newTextForHole (H.gensymHandle h)
      unit <- liftEither $ locatorToTypeVar m coreUnit
      discern h $ bind fakeLoc fakeLoc (m, VK.Normal, hole, [], [], unit) e1 e2
    m :< RT.SeqEnd e1 -> do
      hole <- liftIO $ Gensym.newTextForHole (H.gensymHandle h)
      unit <- liftEither $ locatorToTypeVar m coreUnit
      unitUnit <- liftEither $ locatorToVarGlobal m coreUnitUnit
      discern h $ bind fakeLoc fakeLoc (m, VK.Normal, hole, [], [], unit) e1 unitUnit
    m :< RT.When whenClause -> do
      let (whenCond, whenBody) = RT.extractFromKeywordClause whenClause
      boolTrue <- liftEither $ locatorToName (blur m) coreBoolTrue
      boolFalse <- liftEither $ locatorToName (blur m) coreBoolFalse
      unitUnit <- liftEither $ locatorToVarGlobal m coreUnitUnit
      discern h $ foldIf m boolTrue boolFalse whenCond whenBody [] unitUnit
    m :< RT.Admit -> do
      panic <- liftEither $ locatorToVarGlobal m coreTrickPanic
      textType <- liftEither $ locatorToTypeVar m coreText
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
      t <- liftIO $ RT.createTypeHole (H.gensymHandle h) (blur m)
      detachVar <- liftEither $ locatorToVarGlobal m coreThreadDetach
      cod <- liftIO $ RT.createTypeHole (H.gensymHandle h) (blur m)
      detachVar' <- discern h detachVar
      t' <- discernType h t
      lam' <- discern h $ RT.lam fakeLoc m [] cod e
      return $ m :< WT.PiElim PEK.Normal detachVar' (ImpArgs.FullySpecified [t']) [lam'] (DefaultArgs.ByKey [])
    m :< RT.Attach _ _ (e, _) -> do
      t <- liftIO $ RT.createTypeHole (H.gensymHandle h) (blur m)
      attachVar <- liftEither $ locatorToVarGlobal m coreThreadAttach
      attachVar' <- discern h attachVar
      t' <- discernType h t
      e' <- discern h e
      return $ m :< WT.PiElim PEK.Normal attachVar' (ImpArgs.FullySpecified [t']) [e'] (DefaultArgs.ByKey [])
    m :< RT.Assert _ (mText, message) _ _ (e@(mCond :< _), _) -> do
      assert <- liftEither $ locatorToVarGlobal m coreTrickAssert
      textType <- liftEither $ locatorToTypeVar m coreText
      let fullMessage = T.pack (Hint.toString m) <> "\nAssertion failure: " <> message <> "\n"
      cod <- liftIO $ RT.createTypeHole (H.gensymHandle h) (blur m)
      assertVar' <- discern h assert
      textTerm' <- discern h (mText :< RT.StaticText textType fullMessage)
      lam' <- discern h $ RT.lam fakeLoc mCond [] cod e
      return $ m :< WT.PiElim PEK.Normal assertVar' ImpArgs.Unspecified [textTerm', lam'] (DefaultArgs.ByKey [])
    m :< RT.Introspect _ key _ clauseList -> do
      value <- getIntrospectiveValue h m key
      clause <- lookupIntrospectiveClause m value $ SE.extract clauseList
      discern h clause
    m :< RT.IncludeText _ _ mKey (key, _) -> do
      contentOrNone <- liftIO $ Locator.getStaticFileContent (H.locatorHandle h) key
      case contentOrNone of
        Just (path, content) -> do
          liftIO $ Unused.deleteStaticFile (H.unusedHandle h) key
          textType <- liftEither (locatorToTypeVar m coreText) >>= discernType h
          liftIO $ Tag.insertFileLoc (H.tagHandle h) mKey (T.length key) (newSourceHint path)
          return $ m :< WT.Prim (WPV.StaticText textType content)
        Nothing ->
          raiseError m $ "No such static file is defined: `" <> key <> "`"
    m :< RT.With withClause -> do
      let (binder, body) = RT.extractFromKeywordClause withClause
      case body of
        mLet :< RT.Let letKind c1 mxt@(mPat, pat, c2, c3, t) c c4 e1 c5 startLoc c6 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          case letKind of
            RT.Bind -> do
              tmpVar <- liftIO $ Gensym.newText (H.gensymHandle h)
              (k, x, e2'') <- modifyLetContinuation h (mPat, pat) False e2'
              let m' = blur m
              dom <- liftIO $ RT.createTypeHole (H.gensymHandle h) m'
              cod <- liftIO $ RT.createTypeHole (H.gensymHandle h) m'
              discern h $
                bind'
                  False
                  startLoc
                  endLoc
                  (mPat, VK.Normal, tmpVar, c2, c3, dom)
                  e1'
                  ( m
                      :< RT.piElim
                        binder
                        [ m' :< RT.Var (Var tmpVar),
                          RT.lam
                            startLoc
                            m'
                            [((mPat, k, x, c2, c3, t), c)]
                            cod
                            e2''
                        ]
                  )
            _ -> do
              discern h $ mLet :< RT.Let letKind c1 mxt c c4 e1' c5 startLoc c6 e2' endLoc
        mLet :< RT.LetOn letKind c1 mxt c2 mys c3 e1 c4 startLoc c5 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $ mLet :< RT.LetOn letKind c1 mxt c2 mys c3 e1' c4 startLoc c5 e2' endLoc
        mTau :< RT.TauElim c1 mxt c2 e1 c3 startLoc c4 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $ mTau :< RT.TauElim c1 mxt c2 e1' c3 startLoc c4 e2' endLoc
        mBox :< RT.BoxElim nesVariant mustIgnoreRelayedVars c1 mxt c2 mys c3 e1 c4 startLoc c5 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $
            mBox :< RT.BoxElim nesVariant mustIgnoreRelayedVars c1 mxt c2 mys c3 e1' c4 startLoc c5 e2' endLoc
        mSeq :< RT.Seq (e1, c1) c2 e2 -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $ mSeq :< RT.Seq (e1', c1) c2 e2'
        mPin :< RT.Pin c1 mxt c2 mys c3 e1 c4 startLoc c5 e2 endLoc -> do
          let e1' = m :< RT.With (([], (binder, [])), ([], (e1, [])))
          let e2' = m :< RT.With (([], (binder, [])), ([], (e2, [])))
          discern h $ mPin :< RT.Pin c1 mxt c2 mys c3 e1' c4 startLoc c5 e2' endLoc
        _ ->
          discern h body
    _ :< RT.Brace _ (e, _) ->
      discern h e
    m :< RT.Int i -> do
      let intType = m :< WT.PrimType (PT.Int IntSize64)
      return $ m :< WT.Prim (WPV.Int intType i)

discernType :: H.Handle -> RT.RawType -> App WT.WeakType
discernType h ty =
  case ty of
    m :< RT.Tau ->
      return $ m :< WT.Tau
    m :< RT.TypeHole k ->
      return $ m :< WT.TypeHole k []
    m :< RT.TyVar name -> do
      case name of
        Var s
          | Just (mDef, name', _, _) <- lookup s (H.typeNameEnv h) -> do
              liftIO $ Unused.deleteVariable (H.unusedHandle h) name'
              liftIO $ Tag.insertLocalVar (H.tagHandle h) m name' mDef
              return $ m :< WT.TVar name'
        _ -> do
          (dd, (_, gn)) <- resolveName h m name
          interpretGlobalTypeName m dd gn
    m :< RT.TyApp t _ args -> do
      t' <- discernType h t
      args' <- mapM (discernType h) $ SE.extract args
      return $ m :< WT.TyApp t' args'
    m :< RT.Pi impArgs expArgs defaultArgs rawPiKind _ t endLoc -> do
      let impArgsBase = RT.extractImpArgs impArgs
      let defaultArgsBase = SE.extract $ fst defaultArgs
      (impArgs', h') <- discernImpArgs h impArgsBase endLoc
      (expArgs', h'') <-
        case rawPiKind of
          RT.PiDataIntro ->
            discernBinder h' (RT.extractArgs expArgs) endLoc
          _ ->
            discernTypeBinder h' (RT.extractArgs expArgs) endLoc
      (defaultArgs', h''') <-
        case rawPiKind of
          RT.PiDataIntro ->
            discernBinderWithDefaultArgs h'' defaultArgsBase endLoc
          _ ->
            discernTypeBinderWithDefaultArgs h'' defaultArgsBase endLoc
      t' <- discernType h''' t
      let defaultBinders = map fst defaultArgs'
      forM_ (impArgs' ++ expArgs' ++ defaultBinders) $ \(_, _, x, _) ->
        liftIO (Unused.deleteVariable (H.unusedHandle h''') x)
      let piKind =
            case rawPiKind of
              RT.PiNormal ->
                PK.normal
              RT.PiDestPass ->
                PK.DestPass False
              RT.PiDataIntro ->
                PK.normal
      return $ m :< WT.Pi piKind impArgs' expArgs' defaultBinders t'
    m :< RT.Data attr dataName es -> do
      es' <- mapM (discernType h) es
      let allowedVars = S.unions $ map freeVarsType es'
      let nameEnv' = filter (\(_, (_, x, _, _)) -> S.member x allowedVars) (H.typeNameEnv h)
      let hAttr = h {H.typeNameEnv = nameEnv'}
      attr' <- discernAttrData hAttr attr
      return $ m :< WT.Data attr' dataName es'
    m :< RT.Box t -> do
      t' <- discernType h t
      return $ m :< WT.Box t'
    m :< RT.BoxNoema t -> do
      t' <- discernType h t
      return $ m :< WT.BoxNoema t'
    m :< RT.Code t -> do
      t' <- discernType h t
      return $ m :< WT.Code t'
    m :< RT.Rune ->
      return $ m :< WT.PrimType PT.Rune
    m :< RT.Pointer ->
      return $ m :< WT.PrimType PT.Pointer
    m :< RT.Void ->
      return $ m :< WT.Void
    m :< RT.Option t -> do
      eitherType <- liftEither $ locatorToTypeVar m coreEither
      unitType <- liftEither $ locatorToTypeVar m coreUnit
      eitherType' <- discernType h eitherType
      unitType' <- discernType h unitType
      t' <- discernType h t
      return $ m :< WT.TyApp eitherType' [unitType', t']
    _ :< RT.TyBrace _ (t, _) ->
      discernType h t
    m :< RT.TyIntrospect _ key _ clauseList -> do
      value <- getIntrospectiveValue h m key
      clause <- lookupIntrospectiveClause m value $ SE.extract clauseList
      discernType h clause

type ShouldInsertTagInfo =
  Bool

discernNoeticVarList ::
  H.Handle ->
  ShouldInsertTagInfo ->
  [(Hint, (Hint, VK.VarKind, Ident))] ->
  IO [(BinderF WT.WeakType, WT.WeakTerm)]
discernNoeticVarList h mustInsertTagInfo xsOuter = do
  forM xsOuter $ \(mDef, (mUse, k, outerVar)) -> do
    xInner <- Gensym.newIdentFromIdent (H.gensymHandle h) outerVar
    t <- WT.createTypeHole (H.gensymHandle h) mUse []
    when mustInsertTagInfo $ do
      Tag.insertLocalVar (H.tagHandle h) mUse outerVar mDef
    return ((mUse, k, xInner, t), mDef :< WT.Var outerVar)

discernMagic :: H.Handle -> Hint -> RT.RawMagic -> App (M.WeakMagic WT.WeakType WT.WeakType WT.WeakTerm)
discernMagic h m magic =
  case magic of
    RT.Cast _ (_, (from, _)) (_, (to, _)) (_, (e, _)) _ -> do
      from' <- discernType h from
      to' <- discernType h to
      e' <- discern h e
      return $ M.WeakMagic $ M.LowMagic $ LM.Cast from' to' e'
    RT.Store _ (_, (t, _)) (_, (value, _)) (_, (pointer, _)) _ -> do
      ensureRuntimeStage m h "runtime magic (`store`)"
      t' <- discernType h t
      unit <- liftEither (locatorToTypeVar m coreUnit) >>= discernType h
      value' <- discern h value
      pointer' <- discern h pointer
      return $ M.WeakMagic $ M.LowMagic $ LM.Store t' unit value' pointer'
    RT.Load _ (_, (t, _)) (_, (pointer, _)) _ -> do
      ensureRuntimeStage m h "runtime magic (`load`)"
      t' <- discernType h t
      pointer' <- discern h pointer
      return $ M.WeakMagic $ M.LowMagic $ LM.Load t' pointer'
    RT.Alloca _ (_, (t, _)) (_, (size, _)) _ -> do
      ensureRuntimeStage m h "runtime magic (`alloca`)"
      t' <- discernType h t
      size' <- discern h size
      return $ M.WeakMagic $ M.LowMagic $ LM.Alloca t' size'
    RT.Calloc _ (_, (num, _)) (_, (size, _)) _ -> do
      ensureRuntimeStage m h "runtime magic (`calloc`)"
      sizeType <- liftEither (locatorToTypeVar m coreCSize) >>= discernType h
      num' <- discern h num
      size' <- discern h size
      return $ M.WeakMagic $ M.Calloc sizeType num' size'
    RT.Malloc _ (_, (size, _)) _ -> do
      ensureRuntimeStage m h "runtime magic (`malloc`)"
      sizeType <- liftEither (locatorToTypeVar m coreCSize) >>= discernType h
      size' <- discern h size
      return $ M.WeakMagic $ M.Malloc sizeType size'
    RT.Realloc _ (_, (ptr, _)) (_, (size, _)) _ -> do
      ensureRuntimeStage m h "runtime magic (`realloc`)"
      sizeType <- liftEither (locatorToTypeVar m coreCSize) >>= discernType h
      ptr' <- discern h ptr
      size' <- discern h size
      return $ M.WeakMagic $ M.Realloc sizeType ptr' size'
    RT.Free _ (_, (ptr, _)) _ -> do
      ensureRuntimeStage m h "runtime magic (`free`)"
      unitType <- liftEither (locatorToTypeVar m coreUnit) >>= discernType h
      ptr' <- discern h ptr
      return $ M.WeakMagic $ M.Free unitType ptr'
    RT.External _ mUse funcName _ args varArgsOrNone -> do
      ensureRuntimeStage m h "runtime magic (`external`)"
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
            t' <- discernType h t
            return (arg', t')
      return $ M.WeakMagic $ M.LowMagic $ LM.External domList cod funcName args' varArgs'
    RT.Global _ (_, (name, _)) (_, (t, _)) _ -> do
      ensureRuntimeStage m h "runtime magic (`global`)"
      t' <- discernType h t
      return $ M.WeakMagic $ M.LowMagic $ LM.Global name t'
    RT.OpaqueValue _ (_, (e, _)) -> do
      ensureRuntimeStage m h "runtime magic (`opaque-value`)"
      e' <- discern h e
      return $ M.WeakMagic $ M.LowMagic $ LM.OpaqueValue e'
    RT.CallType _ (_, (func, _)) (_, (arg1, _)) (_, (arg2, _)) -> do
      ensureRuntimeStage m h "runtime magic (`call-type`)"
      func' <- discern h func
      arg1' <- discern h arg1
      arg2' <- discern h arg2
      return $ M.WeakMagic $ M.LowMagic $ LM.CallType func' arg1' arg2'
    RT.InspectType (_, (typeExpr, _)) -> do
      ensureCompileStage m h "inline magic (`inspect-type`)"
      coreModuleID <- Alias.resolveModuleAlias (H.aliasHandle h) m coreModuleAlias
      typeValueVar <- liftEither $ locatorToTypeVar m coreTypeValueTypeValue
      typeValueExpr <- discernType h typeValueVar
      typeExpr' <- discernType h typeExpr
      return $ M.WeakMagic $ M.InspectType coreModuleID typeValueExpr typeExpr'
    RT.EqType (_, (typeExpr1, _)) (_, (typeExpr2, _)) -> do
      ensureCompileStage m h "inline magic (`eq-type`)"
      coreModuleID <- Alias.resolveModuleAlias (H.aliasHandle h) m coreModuleAlias
      typeExpr1' <- discernType h typeExpr1
      typeExpr2' <- discernType h typeExpr2
      return $ M.WeakMagic $ M.EqType coreModuleID typeExpr1' typeExpr2'
    RT.ShowType _ (_, (typeExpr, _)) -> do
      textType <- liftEither (locatorToTypeVar m coreText) >>= discernType h
      typeExpr' <- discernType h typeExpr
      return $ M.WeakMagic $ M.ShowType textType typeExpr'
    RT.TextCons _ (_, (rune, _)) (_, (text, _)) -> do
      textType <- liftEither (locatorToTypeVar m coreText) >>= discernType h
      rune' <- discern h rune
      text' <- discern h text
      return $ M.WeakMagic $ M.TextCons textType rune' text'
    RT.TextUncons _ (_, (text, _)) -> do
      moduleID <- Alias.resolveModuleAlias (H.aliasHandle h) m coreModuleAlias
      text' <- discern h text
      return $ M.WeakMagic $ M.TextUncons moduleID text'
    RT.CompileError _ (_, (msg, _)) -> do
      ensureCompileStage m h "inline magic (`compile-error`)"
      textType <- liftEither (locatorToTypeVar m coreText) >>= discernType h
      msg' <- discern h msg
      return $ M.WeakMagic $ M.CompileError textType msg'

modifyLetContinuation ::
  H.Handle ->
  (Hint, RP.RawPattern) ->
  N.IsNoetic ->
  RT.RawTerm ->
  App (VK.VarKind, RawIdent, RT.RawTerm)
modifyLetContinuation h pat isNoetic cont@(mCont :< _) =
  case pat of
    (_, RP.Var k (Var x))
      | not (isConsName x) ->
          return (k, x, cont)
    _ -> do
      tmp <- liftIO $ Gensym.newTextForHole (H.gensymHandle h)
      return
        ( VK.Normal,
          tmp,
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
  RawBinder RT.RawType ->
  RT.RawTerm ->
  RT.RawTerm ->
  RT.RawTerm
bind startLoc endLoc (m, k, x, c1, c2, t) =
  bind' False startLoc endLoc (m, k, x, c1, c2, t)

bind' ::
  RT.MustIgnoreRelayedVars ->
  Loc ->
  Loc ->
  RawBinder RT.RawType ->
  RT.RawTerm ->
  RT.RawTerm ->
  RT.RawTerm
bind' mustIgnoreRelayedVars loc endLoc (m, _, x, c1, c2, t) e cont =
  m
    :< RT.Let
      (RT.Plain mustIgnoreRelayedVars)
      []
      (m, RP.Var VK.Normal (Var x), c1, c2, t)
      []
      []
      e
      []
      loc
      []
      cont
      endLoc

lookupIntrospectiveClause :: Hint -> T.Text -> [(Maybe T.Text, C, a)] -> App a
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
              [ ( SE.fromList'' [(blur m, RP.Var VK.Normal true)],
                  [],
                  ifBody
                ),
                ( SE.fromList'' [(blur m, RP.Var VK.Normal false)],
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
              [ (SE.fromList'' [(blur m, RP.Var VK.Normal true)], [], ifBody),
                (SE.fromList'' [(blur m, RP.Var VK.Normal false)], [], cont)
              ]
          )

doNotCare :: Hint -> WT.WeakType
doNotCare m =
  m :< WT.Tau

discernLet ::
  H.Handle ->
  Hint ->
  RT.LetKind ->
  (Hint, RP.RawPattern, C, C, RT.RawType) ->
  RT.RawTerm ->
  RT.RawTerm ->
  Loc ->
  Loc ->
  App WT.WeakTerm
discernLet h m letKind (mx, pat, c1, c2, t) e1@(m1 :< _) e2 startLoc endLoc = do
  let opacity = WT.Clear
  let discernLet' isNoetic = do
        e1' <- discern h e1
        (k, x, e2') <- modifyLetContinuation h (mx, pat) isNoetic e2
        (mxt', e2'') <- discernBinderWithBody' h (mx, k, x, c1, c2, t) startLoc endLoc e2'
        liftIO $ Tag.insertBinder (H.tagHandle h) mxt'
        return $ m :< WT.Let opacity mxt' e1' e2''
  case letKind of
    RT.Plain _ -> do
      discernLet' False
    RT.Noetic -> do
      discernLet' True
    RT.Bind -> do
      raiseError m "`bind` can only be used inside `with`"
    RT.Try -> do
      let m' = blur m
      eitherTypeInner <- liftEither $ locatorToTypeVar m' coreEither
      leftType <- liftIO $ RT.createTypeHole (H.gensymHandle h) m'
      let eitherType = m' :< RT.TyApp eitherTypeInner [] (SE.fromList' [leftType, t])
      e1' <- discern h e1
      tmpVar <- liftIO $ Gensym.newText (H.gensymHandle h)
      eitherCont <- constructEitherBinder h m mx m1 pat tmpVar e2
      (mxt', eitherCont') <- discernBinderWithBody' h (mx, VK.Normal, tmpVar, c1, c2, eitherType) startLoc endLoc eitherCont
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
        ( SE.fromList'' [(m', RP.Cons eitherL [] (RP.Paren (SE.fromList' [(m', RP.Var VK.Normal (Var earlyRetVar))])))],
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
    Just (mDef, x', _, stage) -> do
      if stage == H.currentStage h
        then do
          liftIO $ Unused.deleteVariable (H.unusedHandle h) x'
          return (mDef, (mUse, x'))
        else raiseStageError mUse (H.currentStage h) stage

discernImpArgs ::
  H.Handle ->
  [RawBinder RT.RawType] ->
  Loc ->
  App ([BinderF WT.WeakType], H.Handle)
discernImpArgs h binder endLoc =
  case binder of
    [] -> do
      return ([], h)
    (mx, k, x, _, _, t) : xts -> do
      t' <- discernType h t
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extendType' h mx x' VDK.Normal
      (xts', h'') <- discernImpArgs h' xts endLoc
      liftIO $ Tag.insertBinder (H.tagHandle h'') (mx, k, x', t')
      liftIO $ SymLoc.insert (H.symLocHandle h'') x' (metaLocation mx) endLoc
      return ((mx, k, x', t') : xts', h'')

discernBinder ::
  H.Handle ->
  [RawBinder RT.RawType] ->
  Loc ->
  App ([BinderF WT.WeakType], H.Handle)
discernBinder h binder endLoc =
  case binder of
    [] -> do
      return ([], h)
    (mx, k, x, _, _, t) : xts -> do
      t' <- discernType h t
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extend' h mx x' VDK.Normal
      (xts', h'') <- discernBinder h' xts endLoc
      liftIO $ Tag.insertBinder (H.tagHandle h'') (mx, k, x', t')
      liftIO $ SymLoc.insert (H.symLocHandle h'') x' (metaLocation mx) endLoc
      return ((mx, k, x', t') : xts', h'')

discernTypeBinder ::
  H.Handle ->
  [RawBinder RT.RawType] ->
  Loc ->
  App ([BinderF WT.WeakType], H.Handle)
discernTypeBinder h binder endLoc =
  case binder of
    [] -> do
      return ([], h)
    (mx, k, x, _, _, t) : xts -> do
      t' <- discernType h t
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extendType' h mx x' VDK.Normal
      (xts', h'') <- discernTypeBinder h' xts endLoc
      liftIO $ Tag.insertBinder (H.tagHandle h'') (mx, k, x', t')
      liftIO $ SymLoc.insert (H.symLocHandle h'') x' (metaLocation mx) endLoc
      return ((mx, k, x', t') : xts', h'')

discernBinderWithDefaultArgs ::
  H.Handle ->
  [(RawBinder RT.RawType, RT.RawTerm)] ->
  Loc ->
  App ([(BinderF WT.WeakType, WT.WeakTerm)], H.Handle)
discernBinderWithDefaultArgs h binder endLoc =
  case binder of
    [] -> do
      return ([], h)
    ((mx, k, x, _, _, t), defaultValue) : xts -> do
      t' <- discernType h t
      defaultValue' <- discern h defaultValue
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extend' h mx x' VDK.Normal
      (xts', h'') <- discernBinderWithDefaultArgs h' xts endLoc
      liftIO $ Tag.insertBinder (H.tagHandle h'') (mx, k, x', t')
      liftIO $ SymLoc.insert (H.symLocHandle h'') x' (metaLocation mx) endLoc
      return (((mx, k, x', t'), defaultValue') : xts', h'')

discernTypeBinderWithDefaultArgs ::
  H.Handle ->
  [(RawBinder RT.RawType, RT.RawTerm)] ->
  Loc ->
  App ([(BinderF WT.WeakType, WT.WeakTerm)], H.Handle)
discernTypeBinderWithDefaultArgs h binder endLoc =
  case binder of
    [] -> do
      return ([], h)
    ((mx, k, x, _, _, t), defaultValue) : xts -> do
      t' <- discernType h t
      defaultValue' <- discern h {H.nameEnv = []} defaultValue
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extendType' h mx x' VDK.Normal
      (xts', h'') <- discernTypeBinderWithDefaultArgs h' xts endLoc
      liftIO $ Tag.insertBinder (H.tagHandle h'') (mx, k, x', t')
      liftIO $ SymLoc.insert (H.symLocHandle h'') x' (metaLocation mx) endLoc
      return (((mx, k, x', t'), defaultValue') : xts', h'')

discernBinder' ::
  H.Handle ->
  [RawBinder RT.RawType] ->
  App ([BinderF WT.WeakType], H.Handle)
discernBinder' h binder =
  case binder of
    [] -> do
      return ([], h)
    (mx, k, x, _, _, t) : xts -> do
      t' <- discernType h t
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extend' h mx x' VDK.Normal
      (xts', h'') <- discernBinder' h' xts
      liftIO $ Tag.insertBinder (H.tagHandle h) (mx, k, x', t')
      return ((mx, k, x', t') : xts', h'')

discernTypeBinder' ::
  H.Handle ->
  [RawBinder RT.RawType] ->
  App ([BinderF WT.WeakType], H.Handle)
discernTypeBinder' h binder =
  case binder of
    [] -> do
      return ([], h)
    (mx, k, x, _, _, t) : xts -> do
      t' <- discernType h t
      x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
      h' <- liftIO $ H.extendType' h mx x' VDK.Normal
      (xts', h'') <- discernTypeBinder' h' xts
      liftIO $ Tag.insertBinder (H.tagHandle h) (mx, k, x', t')
      return ((mx, k, x', t') : xts', h'')

discernBinderWithBody' ::
  H.Handle ->
  RawBinder RT.RawType ->
  Loc ->
  Loc ->
  RT.RawTerm ->
  App (BinderF WT.WeakType, WT.WeakTerm)
discernBinderWithBody' h (mx, k, x, _, _, codType) startLoc endLoc e = do
  codType' <- discernType h codType
  x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
  h'' <- liftIO $ H.extend' h mx x' VDK.Normal
  e' <- discern h'' e
  liftIO $ SymLoc.insert (H.symLocHandle h'') x' startLoc endLoc
  return ((mx, k, x', codType'), e')

-- Helper to convert Attr with RawBinder to Attr with BinderF
discernAttrData ::
  H.Handle ->
  AttrD.Attr DD.DefiniteDescription (RawBinder RT.RawType) ->
  App (AttrD.Attr DD.DefiniteDescription (BinderF WT.WeakType))
discernAttrData h attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- forM consNameList $ \(name, binders, isConstLike) -> do
    (binders', _) <- discernBinder' h binders
    forM_ binders' $ \(_, _, x, _) -> do
      liftIO $ Unused.deleteVariable (H.unusedHandle h) x
    return (name, binders', isConstLike)
  return $ attr {AttrD.consNameList = consNameList'}

discernAttrDataIntro ::
  H.Handle ->
  AttrDI.Attr DD.DefiniteDescription (RawBinder RT.RawType) ->
  App (AttrDI.Attr DD.DefiniteDescription (BinderF WT.WeakType))
discernAttrDataIntro h attr = do
  let consNameList = AttrDI.consNameList attr
  consNameList' <- forM consNameList $ \(name, binders, isConstLike) -> do
    (binders', _) <- discernBinder' h binders
    forM_ binders' $ \(_, _, x, _) -> do
      liftIO $ Unused.deleteVariable (H.unusedHandle h) x
    return (name, binders', isConstLike)
  return $ attr {AttrDI.consNameList = consNameList'}

discernPatternMatrix ::
  H.Handle ->
  [RP.RawPatternRow RT.RawTerm] ->
  App (PAT.PatternMatrix ([Ident], [(BinderF WT.WeakType, WT.WeakTerm)], WT.WeakTerm))
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
  App (PAT.PatternRow ([Ident], [(BinderF WT.WeakType, WT.WeakTerm)], WT.WeakTerm))
discernPatternRow h (patList, _, body) = do
  (patList', body') <- discernPatternRow' h (SE.extract patList) [] body
  return (V.fromList patList', body')

discernPatternRow' ::
  H.Handle ->
  [(Hint, RP.RawPattern)] ->
  NominalEnv ->
  RT.RawTerm ->
  App ([(Hint, PAT.Pattern)], ([Ident], [(BinderF WT.WeakType, WT.WeakTerm)], WT.WeakTerm))
discernPatternRow' h patList newVarList body = do
  case patList of
    [] -> do
      ensureVariableLinearity newVarList
      h' <- liftIO $ H.extendByNominalEnv h VDK.Normal newVarList
      body' <- discern h' body
      return ([], ([], [], body'))
    pat : rest -> do
      (pat', varsInPat) <- discernPattern h (H.currentLayer h) (H.currentStage h) pat
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
    (from, (m, _, _, _)) : rest
      | S.member from found ->
          getNonLinearOccurrences rest found ((m, from) : nonLinear)
      | otherwise ->
          getNonLinearOccurrences rest (S.insert from found) nonLinear

discernPattern ::
  H.Handle ->
  Layer ->
  Stage ->
  (Hint, RP.RawPattern) ->
  App ((Hint, PAT.Pattern), NominalEnv)
discernPattern h layer stage (m, pat) = do
  case pat of
    RP.Var k name -> do
      case name of
        Var x
          | Just i <- R.readMaybe (T.unpack x) -> do
              case k of
                VK.Exp ->
                  raiseError m "Numeric literal cannot be marked with `!`"
                VK.Normal ->
                  return ((m, PAT.Literal (LI.Int i)), [])
          | isConsName x && k == VK.Normal -> do
              (consDD, dataArgNum, consArgNum, disc, isConstLike, _) <- resolveConstructor h m $ Var x
              unless isConstLike $ do
                let mainModule = Env.getMainModule (H.envHandle h)
                let consDD' = readableDD mainModule consDD
                raiseError m $
                  "The constructor `" <> consDD' <> "` cannot be used as a constant"
              return ((m, PAT.Cons (PAT.ConsInfo {args = [], ..})), [])
          | otherwise -> do
              x' <- liftIO $ Gensym.newIdentFromText (H.gensymHandle h) x
              return ((m, PAT.Var k x'), [(x, (m, x', layer, stage))])
        Locator l -> do
          case k of
            VK.Exp ->
              raiseError m "Locator patterns cannot be marked with `!`"
            VK.Normal -> do
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
          (args', hList) <- mapAndUnzipM (discernPattern h layer stage) $ SE.extract args
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
          (_, expKeys, _) <- KeyArg.lookup (H.keyArgHandle h) m consName
          defaultKeyMap <- liftIO $ constructDefaultKeyMap h m expKeys
          let specifiedKeyMap = Map.fromList $ zip ks mvs
          let kvs' = Map.union specifiedKeyMap defaultKeyMap
          expArgs <- resolveExpKeys h m expKeys kvs'
          checkRedundancy m expKeys kvs'
          (patList', hList) <- mapAndUnzipM (discernPattern h layer stage) expArgs
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
  return $ Map.fromList $ zipWith (\k v -> (k, (m, RP.Var VK.Normal (Var v)))) keyList names

locatorToName :: Hint -> T.Text -> Either E.Error Name
locatorToName m text = do
  (gl, ll) <- DD.getLocatorPair m text
  return $ Locator (gl, ll)

locatorToVarGlobal :: Hint -> T.Text -> Either E.Error RT.RawTerm
locatorToVarGlobal m text = do
  (gl, ll) <- DD.getLocatorPair (blur m) text
  return $ blur m :< RT.Var (Locator (gl, ll))

locatorToTypeVar :: Hint -> T.Text -> Either E.Error RT.RawType
locatorToTypeVar m text = do
  (gl, ll) <- DD.getLocatorPair (blur m) text
  return $ blur m :< RT.TyVar (Locator (gl, ll))

getLayer :: Hint -> H.Handle -> Ident -> App Layer
getLayer m h x =
  case lookup (Ident.toText x) (H.nameEnv h) of
    Nothing ->
      raiseCritical m $ "Scene.Parse.Discern.getLayer: Undefined variable: " <> Ident.toText x
    Just (_, _, l, _) -> do
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

raiseStageError :: Hint -> Stage -> Stage -> App a
raiseStageError m expected found = do
  raiseError m $
    "Expected stage:\n  "
      <> T.pack (show expected)
      <> "\nFound stage:\n  "
      <> T.pack (show found)

ensureRuntimeStage :: Hint -> H.Handle -> T.Text -> App ()
ensureRuntimeStage m h target = do
  let stage = H.currentStage h
  when (stage >= 1) $ do
    raiseError m $
      target
        <> " is only allowed at stage <= 0, but found stage "
        <> T.pack (show stage)

ensureCompileStage :: Hint -> H.Handle -> T.Text -> App ()
ensureCompileStage m h target = do
  let stage = H.currentStage h
  when (stage <= 0) $ do
    raiseError m $
      target
        <> " is only allowed at stage >= 1, but found stage "
        <> T.pack (show stage)

asOpaqueValue :: RT.RawTerm -> RT.RawTerm
asOpaqueValue e@(m :< _) =
  m :< RT.Magic [] (RT.OpaqueValue [] ([], (e, [])))

interpretForeign :: H.Handle -> [RawForeignItemF WT.WeakType] -> IO [WT.WeakForeign]
interpretForeign h foreignItemList = do
  mapM (interpretForeignItem h) foreignItemList

interpretForeignItem :: H.Handle -> RawForeignItemF WT.WeakType -> IO WT.WeakForeign
interpretForeignItem h (RawForeignItemF m name _ lts _ _ cod) = do
  let lts' = SE.extract lts
  Tag.insertExternalName (H.tagHandle h) m name m
  PreDecl.insert (H.preDeclHandle h) name m
  return $ F.Foreign m name lts' cod

selectDefaultKeyArgs :: [DefaultKey] -> Map.HashMap Key a -> DefaultArgs.DefaultArgs a
selectDefaultKeyArgs defaultKeys kvs = do
  let args = mapMaybe (\k -> fmap (k,) (Map.lookup k kvs)) defaultKeys
  DefaultArgs.ByKey args

resolveExpKeys :: H.Handle -> Hint -> [ExpKey] -> Map.HashMap Key a -> App [a]
resolveExpKeys h m expKeys kvs = do
  case expKeys of
    [] ->
      return []
    expKey : rest -> do
      case Map.lookup expKey kvs of
        Just value -> do
          args <- resolveExpKeys h m rest kvs
          return $ value : args
        Nothing -> do
          raiseError m $ "The field `" <> expKey <> "` is missing"

checkRedundancy :: Hint -> [Key] -> Map.HashMap Key a -> App ()
checkRedundancy m allowedKeys kvs = do
  let keys = Map.keys kvs
  let diff = keys \\ allowedKeys
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
