module Kernel.Clarify.Move.Clarify
  ( Handle,
    new,
    MainHandle,
    newMain,
    clarify,
    clarifyEntryPoint,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.Maybe
import Data.Text qualified as T
import Error.Move.Run (raiseCritical, raiseCritical')
import Error.Rule.EIO (EIO)
import Gensym.Move.Gensym qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Clarify.Move.Internal.Handle.AuxEnv qualified as AuxEnv
import Kernel.Clarify.Move.Internal.Handle.CompDef qualified as CompDef
import Kernel.Clarify.Move.Internal.Linearize qualified as Linearize
import Kernel.Clarify.Move.Internal.Sigma qualified as Sigma
import Kernel.Clarify.Move.Internal.Utility (toRelevantApp)
import Kernel.Clarify.Move.Internal.Utility qualified as Utility
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.CreateLocalHandle qualified as Local
import Kernel.Common.Move.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Move.Handle.Global.Platform qualified as Platform
import Kernel.Common.Move.Handle.Local.Locator qualified as Locator
import Kernel.Common.Rule.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Rule.Handle.Local.Locator qualified as Locator
import Kernel.Common.Rule.OptimizableData qualified as OD
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.Attr.DataIntro qualified as AttrDI
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.Attr.VarGlobal qualified as AttrVG
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.Binder
import Language.Common.Rule.DataSize qualified as DS
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Common.Rule.LamKind qualified as LK
import Language.Common.Rule.Literal qualified as L
import Language.Common.Rule.Magic qualified as M
import Language.Common.Rule.Noema qualified as N
import Language.Common.Rule.Opacity (isOpaque)
import Language.Common.Rule.Opacity qualified as O
import Language.Common.Rule.PrimNumSize (dataSizeToIntSize)
import Language.Common.Rule.PrimNumSize qualified as PNS
import Language.Common.Rule.PrimOp
import Language.Common.Rule.PrimType qualified as PT
import Language.Common.Rule.Rune qualified as RU
import Language.Common.Rule.StmtKind
import Language.Comp.Move.CreateVar qualified as Gensym
import Language.Comp.Move.Reduce qualified as Reduce
import Language.Comp.Move.Subst qualified as CompSubst
import Language.Comp.Rule.Comp qualified as C
import Language.Comp.Rule.EnumCase qualified as EC
import Language.Term.Move.Subst qualified as Subst
import Language.Term.Rule.Prim qualified as P
import Language.Term.Rule.PrimValue qualified as PV
import Language.Term.Rule.Stmt
import Language.Term.Rule.Term qualified as TM
import Language.Term.Rule.Term.Chain (nubFreeVariables)
import Language.Term.Rule.Term.Chain qualified as TM
import Language.Term.Rule.Term.FromPrimNum
import Logger.Rule.Hint

data Handle = Handle
  { gensymHandle :: Gensym.Handle,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle,
    auxEnvHandle :: AuxEnv.Handle,
    sigmaHandle :: Sigma.Handle,
    locatorHandle :: Locator.Handle,
    optDataHandle :: OptimizableData.Handle,
    reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle,
    compDefHandle :: CompDef.Handle,
    baseSize :: DS.DataSize
  }

new :: Global.Handle -> Local.Handle -> IO Handle
new (Global.Handle {..}) (Local.Handle {..}) = do
  let baseSize = Platform.getDataSize platformHandle
  auxEnvHandle <- AuxEnv.new
  defMap <- CompDef.get compDefHandle
  let substHandle = Subst.new gensymHandle
  let compSubstHandle = CompSubst.new gensymHandle
  let reduceHandle = Reduce.new compSubstHandle gensymHandle defMap
  let utilityHandle = Utility.new gensymHandle compSubstHandle auxEnvHandle baseSize
  let linearizeHandle = Linearize.new gensymHandle utilityHandle
  let sigmaHandle = Sigma.new gensymHandle linearizeHandle utilityHandle
  return $ Handle {..}

clarify :: Handle -> [Stmt] -> EIO [C.CompStmt]
clarify h stmtList = do
  liftIO $ AuxEnv.clear (auxEnvHandle h)
  baseAuxEnv <- AuxEnv.toCompStmtList <$> liftIO (getBaseAuxEnv (auxEnvHandle h) (sigmaHandle h))
  liftIO $ AuxEnv.clear (auxEnvHandle h)
  stmtList' <- do
    stmtList' <- mapM (clarifyStmt h) stmtList
    auxEnv <- liftIO $ AuxEnv.toCompStmtList <$> AuxEnv.get (auxEnvHandle h)
    return $ stmtList' ++ auxEnv
  forM_ (stmtList' ++ baseAuxEnv) $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        liftIO $ CompDef.insert (compDefHandle h) x (opacity, args, e)
      C.Foreign {} ->
        return ()
  forM stmtList' $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        e' <- liftIO $ Reduce.reduce (reduceHandle h) e
        -- liftIO $ putStrLn $ T.unpack "==================="
        -- liftIO $ putStrLn $ T.unpack $ DD.reify x
        -- liftIO $ putStrLn $ T.unpack $ T.pack $ show args
        -- liftIO $ putStrLn $ T.unpack $ T.pack $ show e'
        return $ C.Def x opacity args e'
      C.Foreign {} ->
        return stmt

data MainHandle = MainHandle
  { mainAuxEnvHandle :: AuxEnv.Handle,
    mainSigmaHandle :: Sigma.Handle,
    mainReduceHandle :: Reduce.Handle
  }

newMain :: Global.Handle -> IO MainHandle
newMain Global.Handle {..} = do
  mainAuxEnvHandle <- AuxEnv.new
  defMap <- CompDef.get compDefHandle
  let baseSize = Platform.getDataSize platformHandle
  let compSubstHandle = CompSubst.new gensymHandle
  let mainReduceHandle = Reduce.new compSubstHandle gensymHandle defMap
  let utilityHandle = Utility.new gensymHandle compSubstHandle mainAuxEnvHandle baseSize
  let linearizeHandle = Linearize.new gensymHandle utilityHandle
  let mainSigmaHandle = Sigma.new gensymHandle linearizeHandle utilityHandle
  return $ MainHandle {..}

clarifyEntryPoint :: MainHandle -> IO [C.CompStmt]
clarifyEntryPoint h = do
  baseAuxEnv <- getBaseAuxEnv (mainAuxEnvHandle h) (mainSigmaHandle h)
  forM (Map.toList baseAuxEnv) $ \(x, (opacity, args, e)) -> do
    e' <- Reduce.reduce (mainReduceHandle h) e
    return $ C.Def x opacity args e'

getBaseAuxEnv :: AuxEnv.Handle -> Sigma.Handle -> IO C.DefMap
getBaseAuxEnv auxEnvHandle sigmaHandle = do
  Sigma.registerImmediateS4 sigmaHandle
  Sigma.registerClosureS4 sigmaHandle
  AuxEnv.get auxEnvHandle

clarifyStmt :: Handle -> Stmt -> EIO C.CompStmt
clarifyStmt h stmt =
  case stmt of
    StmtDefine _ stmtKind (SavedHint m) f impArgs expArgs _ e -> do
      let xts = map fst impArgs ++ expArgs
      xts' <- dropFst <$> clarifyBinder h IntMap.empty xts
      envArg <- liftIO $ makeEnvArg h
      switchArg <- liftIO $ makeSwitchArg h
      let xts'' = xts' ++ [envArg, switchArg]
      case stmtKind of
        Data name dataArgs consInfoList -> do
          od <- liftIO $ OptimizableData.lookup (optDataHandle h) name
          case od of
            Just OD.Enum -> do
              let discrimintList = map (\(_, _, _, _, d) -> d) consInfoList
              liftIO (Sigma.returnSigmaEnumS4 (sigmaHandle h) name O.Clear discrimintList)
                >>= clarifyStmtDefineBody' h name xts''
            Just OD.Unary
              | [(_, _, _, [(_, _, t)], _)] <- consInfoList -> do
                  (dataArgs', t') <- clarifyBinderBody h IntMap.empty dataArgs t
                  return $ C.Def f O.Clear (map fst $ dataArgs' ++ [envArg, switchArg]) t'
              | otherwise ->
                  raiseCritical m "Found a broken unary data"
            _ -> do
              let dataInfo = map (\(_, _, _, consArgs, discriminant) -> (discriminant, dataArgs, consArgs)) consInfoList
              dataInfo' <- mapM (clarifyDataClause h) dataInfo
              liftIO (Sigma.returnSigmaDataS4 (sigmaHandle h) name O.Opaque dataInfo')
                >>= clarifyStmtDefineBody' h name xts''
        _ -> do
          let tenv = TM.insTypeEnv xts IntMap.empty
          e' <- clarifyStmtDefineBody h tenv xts'' e
          return $ C.Def f (toLowOpacity stmtKind) (map fst xts'') e'
    StmtForeign foreignList ->
      return $ C.Foreign foreignList

makeEnvArg :: Handle -> IO (Ident, C.Comp)
makeEnvArg h = do
  x <- Gensym.newIdentFromText (gensymHandle h) "env"
  return (x, Sigma.returnImmediateNullS4) -- top-level function's env is always null

makeSwitchArg :: Handle -> IO (Ident, C.Comp)
makeSwitchArg h = do
  x <- Gensym.newIdentFromText (gensymHandle h) "sw"
  return (x, Sigma.returnImmediateIntS4 PNS.IntSize64)

clarifyBinderBody ::
  Handle ->
  TM.TypeEnv ->
  [BinderF TM.Term] ->
  TM.Term ->
  EIO ([(Ident, C.Comp)], C.Comp)
clarifyBinderBody h tenv xts e =
  case xts of
    [] -> do
      e' <- clarifyTerm h tenv e
      return ([], e')
    (m, x, t) : rest -> do
      t' <- clarifyTerm h tenv t
      (binder, e') <- clarifyBinderBody h (TM.insTypeEnv [(m, x, t)] tenv) rest e
      return ((x, t') : binder, e')

clarifyStmtDefineBody ::
  Handle ->
  TM.TypeEnv ->
  [(Ident, C.Comp)] ->
  TM.Term ->
  EIO C.Comp
clarifyStmtDefineBody h tenv xts e = do
  clarifyTerm h tenv e
    >>= liftIO . Linearize.linearize (linearizeHandle h) xts
    >>= liftIO . Reduce.reduce (reduceHandle h)

clarifyStmtDefineBody' ::
  Handle ->
  DD.DefiniteDescription ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  EIO C.CompStmt
clarifyStmtDefineBody' h name xts' dataType = do
  dataType' <- liftIO $ Linearize.linearize (linearizeHandle h) xts' dataType >>= Reduce.reduce (reduceHandle h)
  return $ C.Def name O.Clear (map fst xts') dataType'

clarifyTerm :: Handle -> TM.TypeEnv -> TM.Term -> EIO C.Comp
clarifyTerm h tenv term =
  case term of
    _ :< TM.Tau -> do
      return Sigma.returnImmediateTypeS4
    _ :< TM.Var x -> do
      return $ C.UpIntro $ C.VarLocal x
    _ :< TM.VarGlobal (AttrVG.Attr {..}) x -> do
      return $
        C.UpIntro $
          C.SigmaIntro
            [ Sigma.immediateNullS4,
              C.SigmaIntro [],
              C.VarGlobal x (AN.add argNum (AN.fromInt 2))
            ]
    _ :< TM.Pi {} ->
      return Sigma.returnClosureS4
    _ :< TM.PiIntro attr impArgs expArgs e -> do
      clarifyLambda h tenv attr (TM.chainOf tenv [term]) (map fst impArgs ++ expArgs) e
    _ :< TM.PiElim b e impArgs expArgs -> do
      impArgs' <- mapM (clarifyPlus h tenv) impArgs
      expArgs' <- mapM (clarifyPlus h tenv) expArgs
      let allArgs = impArgs' ++ expArgs'
      case e of
        _ :< TM.Prim (P.Value (PV.Op op)) ->
          return $ callPrimOp op allArgs
        _ -> do
          e' <- clarifyTerm h tenv e
          liftIO $ callClosure h b e' allArgs
    _ :< TM.Data _ name dataArgs -> do
      let argNum = AN.fromInt $ length dataArgs + 2
      let cls = C.UpIntro $ C.SigmaIntro [Sigma.immediateNullS4, C.SigmaIntro [], C.VarGlobal name argNum]
      dataArgs' <- mapM (clarifyPlus h tenv) dataArgs
      liftIO $ callClosure h False cls dataArgs'
    m :< TM.DataIntro (AttrDI.Attr {..}) consName dataArgs consArgs -> do
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) consName
      case od of
        Just OD.Enum ->
          return $ C.UpIntro $ C.Int (dataSizeToIntSize (baseSize h)) (D.reify discriminant)
        Just OD.Unary
          | [e] <- consArgs ->
              clarifyTerm h tenv e
          | otherwise ->
              raiseCritical m "Found a malformed unary data in Scene.Clarify.clarifyTerm"
        _ -> do
          (zs, es, xs) <- fmap unzip3 $ mapM (clarifyPlus h tenv) $ dataArgs ++ consArgs
          return $
            Utility.bindLet (zip zs es) $
              C.UpIntro $
                C.SigmaIntro $
                  C.Int (dataSizeToIntSize (baseSize h)) (D.reify discriminant) : xs
    m :< TM.DataElim isNoetic xets tree -> do
      let (xs, es, _) = unzip3 xets
      let mxts = map (m,,m :< TM.Tau) xs
      es' <- mapM (clarifyTerm h tenv) es
      (tree', _) <- clarifyDecisionTree h (TM.insTypeEnv mxts tenv) isNoetic IntMap.empty tree
      return $ Utility.irreducibleBindLet (zip xs es') tree'
    _ :< TM.Box t -> do
      clarifyTerm h tenv t
    _ :< TM.BoxNoema {} ->
      return Sigma.returnImmediateNoemaS4
    _ :< TM.BoxIntro letSeq e -> do
      embody h tenv letSeq e
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      clarifyTerm h tenv $
        TM.fromLetSeqOpaque castSeq $
          TM.fromLetSeq ((mxt, e1) : uncastSeq) e2
    _ :< TM.Let opacity mxt@(_, x, _) e1 e2 -> do
      e2' <- clarifyTerm h (TM.insTypeEnv [mxt] tenv) e2
      mxts' <- dropFst <$> clarifyBinder h tenv [mxt]
      e2'' <- liftIO $ Linearize.linearize (linearizeHandle h) mxts' e2'
      e1' <- clarifyTerm h tenv e1
      return $ Utility.bindLetWithReducibility (not $ isOpaque opacity) [(x, e1')] e2''
    m :< TM.Prim prim ->
      case prim of
        P.Type primType ->
          case primType of
            PT.Int intSize ->
              return $ Sigma.returnImmediateIntS4 intSize
            PT.Float floatSize ->
              return $ Sigma.returnImmediateFloatS4 floatSize
            PT.Rune ->
              return Sigma.returnImmediateRuneS4
            PT.Pointer ->
              return Sigma.returnImmediatePointerS4
        P.Value primValue ->
          case primValue of
            PV.Int _ size l ->
              return $ C.UpIntro (C.Int size l)
            PV.Float _ size l ->
              return $ C.UpIntro (C.Float size l)
            PV.Op op -> do
              clarifyPrimOp h tenv op m
            PV.StaticText _ text ->
              return $ C.UpIntro $ C.VarStaticText text
            PV.Rune r -> do
              let t = fromPrimNum m (PT.Int PNS.IntSize32)
              clarifyTerm h tenv $ m :< TM.Prim (P.Value (PV.Int t PNS.IntSize32 (RU.asInt r)))
    _ :< TM.Magic der -> do
      clarifyMagic h tenv der
    m :< TM.Resource _ resourceID _ discarder copier typeTag -> do
      let liftedName = Locator.attachCurrentLocator (locatorHandle h) $ BN.resourceName resourceID
      isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) liftedName
      unless isAlreadyRegistered $ do
        switch <- liftIO $ Gensym.createVar (gensymHandle h) "switch"
        arg@(argVarName, _) <- liftIO $ Gensym.createVar (gensymHandle h) "arg"
        discard <-
          clarifyTerm h IntMap.empty (m :< TM.PiElim False discarder [] [m :< TM.Var argVarName])
            >>= liftIO . Reduce.reduce (reduceHandle h)
        copy <-
          clarifyTerm h IntMap.empty (m :< TM.PiElim False copier [] [m :< TM.Var argVarName])
            >>= liftIO . Reduce.reduce (reduceHandle h)
        tagMaker <-
          clarifyTerm h IntMap.empty typeTag
            >>= liftIO . Reduce.reduce (reduceHandle h)
        let resourceSpec = Utility.ResourceSpec {switch, arg, defaultClause = tagMaker, clauses = [discard, copy]}
        liftIO $ Utility.registerSwitcher (utilityHandle h) O.Clear liftedName resourceSpec
      return $ C.UpIntro $ C.VarGlobal liftedName AN.argNumS4
    _ :< TM.Void ->
      return Sigma.returnImmediateNullS4

embody :: Handle -> TM.TypeEnv -> [(BinderF TM.Term, TM.Term)] -> TM.Term -> EIO C.Comp
embody h tenv xets cont =
  case xets of
    [] ->
      clarifyTerm h tenv cont
    (mxt@(_, x, t), e) : rest -> do
      t' <- clarifyTerm h tenv t
      (valueVarName, value, valueVar) <- clarifyPlus h tenv e
      relApp <- liftIO $ toRelevantApp (utilityHandle h) valueVar t'
      cont' <- embody h (TM.insTypeEnv [mxt] tenv) rest cont
      cont'' <- liftIO $ Linearize.linearize (linearizeHandle h) [(x, t')] cont'
      return $ Utility.bindLet [(valueVarName, value), (x, relApp)] cont''

type Size =
  Int

type DataArgsMap = IntMap.IntMap ([(Ident, TM.Term)], Size)

clarifyDataClause ::
  Handle ->
  (D.Discriminant, [BinderF TM.Term], [BinderF TM.Term]) ->
  EIO (D.Discriminant, [(Ident, C.Comp)])
clarifyDataClause h (discriminant, dataArgs, consArgs) = do
  let args = dataArgs ++ consArgs
  args' <- dropFst <$> clarifyBinder h IntMap.empty args
  return (discriminant, args')

clarifyDecisionTree ::
  Handle ->
  TM.TypeEnv ->
  N.IsNoetic ->
  DataArgsMap ->
  DT.DecisionTree TM.Term ->
  EIO (C.Comp, [BinderF TM.Term])
clarifyDecisionTree h tenv isNoetic dataArgsMap tree =
  case tree of
    DT.Leaf consumedCursorList letSeq cont@(m :< _) -> do
      let chain = TM.chainOfDecisionTree tenv m tree
      cont' <- clarifyTerm h tenv $ TM.fromLetSeq letSeq cont
      if isNoetic
        then return (cont', chain)
        else do
          (cont'', dataChain) <- tidyCursorList h tenv dataArgsMap consumedCursorList cont'
          return (cont'', dataChain ++ chain)
    DT.Unreachable -> do
      return (C.Unreachable, [])
    DT.Switch (cursor, t@(m :< _)) (fallbackClause, clauseList) -> do
      (fallbackClause', fallbackChain) <- clarifyDecisionTree h tenv isNoetic dataArgsMap fallbackClause
      tmp <- mapM (clarifyCase h tenv isNoetic dataArgsMap cursor) clauseList
      let (enumCaseList, clauseList', clauseChainList) = unzip3 tmp
      let chain = nubFreeVariables $ fallbackChain ++ concat clauseChainList
      let aligner = alignFreeVariable h tenv chain
      clauseList'' <- mapM aligner clauseList'
      let newChain = (m, cursor, m :< TM.Tau) : chain
      let idents = nubOrd $ map (\(_, x, _) -> x) newChain
      ck <- getClauseDataGroup h t
      case ck of
        Just OD.Enum -> do
          tree' <- liftIO $ Utility.getEnumElim (utilityHandle h) idents (C.VarLocal cursor) fallbackClause' (zip enumCaseList clauseList'')
          return (tree', newChain)
        Just OD.Unary -> do
          return (getFirstClause fallbackClause' clauseList'', newChain)
        _ -> do
          (disc, discVar) <- liftIO $ Gensym.createVar (gensymHandle h) "disc"
          enumElim <- liftIO $ Utility.getEnumElim (utilityHandle h) idents discVar fallbackClause' (zip enumCaseList clauseList'')
          return
            ( C.UpElim True disc (C.Primitive (C.Magic (M.Load BLT.Pointer (C.VarLocal cursor)))) enumElim,
              newChain
            )

getFirstClause :: C.Comp -> [C.Comp] -> C.Comp
getFirstClause fallbackClause clauseList =
  case clauseList of
    [] ->
      fallbackClause
    clause : _ ->
      clause

getClauseDataGroup :: Handle -> TM.Term -> EIO (Maybe OD.OptimizableData)
getClauseDataGroup h term =
  case term of
    _ :< TM.Data _ dataName _ -> do
      liftIO $ OptimizableData.lookup (optDataHandle h) dataName
    _ :< TM.PiElim _ (_ :< TM.Data _ dataName _) _ _ -> do
      liftIO $ OptimizableData.lookup (optDataHandle h) dataName
    _ :< TM.PiElim _ (_ :< TM.VarGlobal _ dataName) _ _ -> do
      liftIO $ OptimizableData.lookup (optDataHandle h) dataName
    _ :< TM.Prim (P.Type (PT.Int _)) -> do
      return $ Just OD.Enum
    _ :< TM.Prim (P.Type PT.Rune) -> do
      return $ Just OD.Enum
    _ ->
      raiseCritical' "Clarify.isEnumType"

tidyCursorList :: Handle -> TM.TypeEnv -> DataArgsMap -> [Ident] -> C.Comp -> EIO (C.Comp, [BinderF TM.Term])
tidyCursorList h tenv dataArgsMap consumedCursorList cont =
  case consumedCursorList of
    [] ->
      return (cont, [])
    cursor : rest -> do
      case IntMap.lookup (Ident.toInt cursor) dataArgsMap of
        Nothing ->
          error "tidyCursor"
        Just (dataArgs, cursorSize) -> do
          let (dataArgVars, dataTypes) = unzip dataArgs
          dataTypes' <- mapM (clarifyTerm h tenv) dataTypes
          (cont', chain) <- tidyCursorList h tenv dataArgsMap rest cont
          tmp <- liftIO $ Linearize.linearize (linearizeHandle h) (zip dataArgVars dataTypes') $ do
            C.Free (C.VarLocal cursor) cursorSize cont'
          let newChain = zipWith (\x t@(m :< _) -> (m, x, t)) dataArgVars dataTypes
          return (tmp, newChain ++ chain)

clarifyCase ::
  Handle ->
  TM.TypeEnv ->
  N.IsNoetic ->
  DataArgsMap ->
  Ident ->
  DT.Case TM.Term ->
  EIO (EC.EnumCase, C.Comp, [BinderF TM.Term])
clarifyCase h tenv isNoetic dataArgsMap cursor decisionCase = do
  case decisionCase of
    DT.LiteralCase _ l cont -> do
      (body', contChain) <- clarifyDecisionTree h tenv isNoetic dataArgsMap cont
      case l of
        L.Int i ->
          return (EC.Int i, body', contChain)
        L.Rune r ->
          return (EC.Int (RU.asInt r), body', contChain)
    DT.ConsCase (DT.ConsCaseRecord {..}) -> do
      let (_, dataTypes) = unzip dataArgs
      dataArgVars <- liftIO $ mapM (const $ Gensym.newIdentFromText (gensymHandle h) "dataArg") dataTypes
      let cursorSize = 1 + length dataArgVars + length consArgs
      let dataArgsMap' = IntMap.insert (Ident.toInt cursor) (zip dataArgVars dataTypes, cursorSize) dataArgsMap
      let consArgs' = map (\(m, x, _) -> (m, x, m :< TM.Tau)) consArgs
      let prefixChain = TM.chainOfCaseWithoutCont tenv decisionCase
      (body', contChain) <- clarifyDecisionTree h (TM.insTypeEnv consArgs' tenv) isNoetic dataArgsMap' cont
      let consArgVars = map (\(_, x, _) -> x) consArgs
      let argVars = dataArgVars ++ consArgVars
      let contChain' = filter (\(_, x, _) -> x `notElem` argVars) contChain
      let chain = prefixChain ++ contChain'
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) consDD
      case od of
        Just OD.Enum -> do
          return (EC.Int (D.reify disc), body', chain)
        Just OD.Unary
          | [(_, consArg, _)] <- consArgs ->
              return
                ( EC.Int 0,
                  C.UpElim True consArg (C.UpIntro (C.VarLocal cursor)) body',
                  chain
                )
          | otherwise ->
              raiseCritical' "Found a non-unary consArgs for unary ADT"
        _ -> do
          discriminantVar <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "discriminant"
          return
            ( EC.Int (D.reify disc),
              C.SigmaElim
                False
                (discriminantVar : dataArgVars ++ map (\(_, x, _) -> x) consArgs)
                (C.VarLocal cursor)
                body',
              chain
            )

alignFreeVariable :: Handle -> TM.TypeEnv -> [BinderF TM.Term] -> C.Comp -> EIO C.Comp
alignFreeVariable h tenv fvs e = do
  fvs' <- dropFst <$> clarifyBinder h tenv fvs
  liftIO $ Linearize.linearize (linearizeHandle h) fvs' e

clarifyMagic :: Handle -> TM.TypeEnv -> M.Magic BLT.BaseLowType TM.Term -> EIO C.Comp
clarifyMagic h tenv der = do
  case der of
    M.Cast from to value -> do
      (fromVarName, from', fromVar) <- clarifyPlus h tenv from
      (toVarName, to', toVar) <- clarifyPlus h tenv to
      (valueVarName, value', valueVar) <- clarifyPlus h tenv value
      return $
        Utility.bindLet [(fromVarName, from'), (toVarName, to'), (valueVarName, value')] $
          C.Primitive (C.Magic (M.Cast fromVar toVar valueVar))
    M.Store lt _ value pointer -> do
      let doNotCare = C.SigmaIntro []
      (valueVarName, value', valueVar) <- clarifyPlus h tenv value
      (pointerVarName, pointer', pointerVar) <- clarifyPlus h tenv pointer
      return $
        Utility.bindLet [(valueVarName, value'), (pointerVarName, pointer')] $
          C.Primitive (C.Magic (M.Store lt doNotCare valueVar pointerVar))
    M.Load lt pointer -> do
      (pointerVarName, pointer', pointerVar) <- clarifyPlus h tenv pointer
      return $
        Utility.bindLet [(pointerVarName, pointer')] $
          C.Primitive (C.Magic (M.Load lt pointerVar))
    M.Alloca lt size -> do
      (sizeVarName, size', sizeVar) <- clarifyPlus h tenv size
      return $
        Utility.bindLet [(sizeVarName, size')] $
          C.Primitive (C.Magic (M.Alloca lt sizeVar))
    M.External domList cod extFunName args varArgAndTypeList -> do
      (xs, args', xsAsVars) <- unzip3 <$> mapM (clarifyPlus h tenv) args
      let (varArgs, varTypes) = unzip varArgAndTypeList
      (ys, varArgs', ysAsVarArgs) <- unzip3 <$> mapM (clarifyPlus h tenv) varArgs
      return $
        Utility.bindLet (zip xs args' ++ zip ys varArgs') $
          C.Primitive (C.Magic (M.External domList cod extFunName xsAsVars (zip ysAsVarArgs varTypes)))
    M.Global name lt -> do
      return $ C.Primitive (C.Magic (M.Global name lt))
    M.OpaqueValue e ->
      clarifyTerm h tenv e
    M.CallType func arg1 arg2 -> do
      (funcVarName, func', funcVar) <- clarifyPlus h tenv func
      (arg1VarName, arg1', arg1Var) <- clarifyPlus h tenv arg1
      (arg2VarName, arg2', arg2Var) <- clarifyPlus h tenv arg2
      return $
        Utility.bindLet [(funcVarName, func'), (arg1VarName, arg1'), (arg2VarName, arg2')] $
          C.Primitive (C.Magic (M.CallType funcVar arg1Var arg2Var))

clarifyLambda ::
  Handle ->
  TM.TypeEnv ->
  AttrL.Attr TM.Term ->
  [BinderF TM.Term] ->
  [BinderF TM.Term] ->
  TM.Term ->
  EIO C.Comp
clarifyLambda h tenv attrL@(AttrL.Attr {lamKind, identity}) fvs mxts e@(m :< _) = do
  case lamKind of
    LK.Fix (_, recFuncName, codType) -> do
      let liftedName = Locator.attachCurrentLocator (locatorHandle h) $ BN.muName recFuncName identity
      let appArgs = fvs ++ mxts
      let appArgs' = map (\(mx, x, _) -> mx :< TM.Var x) appArgs
      let argNum = AN.fromInt $ length appArgs'
      let attr = AttrVG.new argNum
      lamAttr <- do
        c <- liftIO $ Gensym.newCount (gensymHandle h)
        return $ AttrL.normal' (Just (Ident.toText recFuncName)) c codType
      let lamApp = m :< TM.PiIntro lamAttr [] mxts (m :< TM.PiElim False (m :< TM.VarGlobal attr liftedName) [] appArgs')
      isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) liftedName
      unless isAlreadyRegistered $ do
        liftedBody <- liftIO $ Subst.subst (substHandle h) (IntMap.fromList [(Ident.toInt recFuncName, Right lamApp)]) e
        (liftedArgs, liftedBody') <- clarifyBinderBody h IntMap.empty appArgs liftedBody
        liftedBody'' <- liftIO $ Linearize.linearize (linearizeHandle h) liftedArgs liftedBody'
        liftIO $ AuxEnv.insert (auxEnvHandle h) liftedName (O.Opaque, map fst liftedArgs, liftedBody'')
      clarifyTerm h tenv lamApp
    LK.Normal mName _ -> do
      e' <- clarifyTerm h (TM.insTypeEnv (catMaybes [AttrL.fromAttr attrL] ++ mxts) tenv) e
      returnClosure h tenv identity mName O.Clear fvs mxts e'

clarifyPlus :: Handle -> TM.TypeEnv -> TM.Term -> EIO (Ident, C.Comp, C.Value)
clarifyPlus h tenv e = do
  e' <- clarifyTerm h tenv e
  (varName, var) <- liftIO $ Gensym.createVar (gensymHandle h) "var"
  return (varName, e', var)

clarifyBinder :: Handle -> TM.TypeEnv -> [BinderF TM.Term] -> EIO [(Hint, Ident, C.Comp)]
clarifyBinder h tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm h tenv t
      xts' <- clarifyBinder h (IntMap.insert (Ident.toInt x) t tenv) xts
      return $ (m, x, t') : xts'

clarifyPrimOp :: Handle -> TM.TypeEnv -> PrimOp -> Hint -> EIO C.Comp
clarifyPrimOp h tenv op m = do
  let (domList, _) = getTypeInfo op
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- liftIO $ mapAndUnzipM (const (Gensym.createVar (gensymHandle h) "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  lamID <- liftIO $ Gensym.newCount (gensymHandle h)
  returnClosure h tenv lamID (Just "primOp") O.Clear [] mxts $ C.Primitive (C.PrimOp op varList)

returnClosure ::
  Handle ->
  TM.TypeEnv ->
  Int ->
  Maybe T.Text ->
  O.Opacity ->
  [BinderF TM.Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF TM.Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  C.Comp -> -- the `e` in `lam (x1, ..., xn). e`
  EIO C.Comp
returnClosure h tenv lamID mName opacity fvs xts e = do
  fvs'' <- dropFst <$> clarifyBinder h tenv fvs
  xts'' <- dropFst <$> clarifyBinder h tenv xts
  fvEnvSigma <- liftIO $ Sigma.closureEnvS4 (sigmaHandle h) (locatorHandle h) $ map Right fvs''
  let fvEnv = C.SigmaIntro (map (\(x, _) -> C.VarLocal x) fvs'')
  let argNum = AN.fromInt $ length xts'' + 2 -- argNum == count(xts) + env
  let name = Locator.attachCurrentLocator (locatorHandle h) $ BN.lambdaName mName lamID
  isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) name
  unless isAlreadyRegistered $ do
    liftIO $ registerClosure h name opacity xts'' fvs'' e
  return $ C.UpIntro $ C.SigmaIntro [fvEnvSigma, fvEnv, C.VarGlobal name argNum]

registerClosure ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  [(Ident, C.Comp)] ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  IO ()
registerClosure h name opacity xts fvs e = do
  e' <- liftIO $ Linearize.linearize (linearizeHandle h) (fvs ++ xts) e
  (envVarName, envVar) <- Gensym.createVar (gensymHandle h) "env"
  (normalEnvVarName, normalEnvVar) <- Gensym.createVar (gensymHandle h) "env"
  (noeticEnvVarName, noeticEnvVar) <- Gensym.createVar (gensymHandle h) "env"
  (switchVarName, switchVar) <- Gensym.createVar (gensymHandle h) "switch"
  let normalPrefix = lambdaPrefixNormal fvs normalEnvVar
  noeticPrefix <- lambdaPrefixNoetic h fvs noeticEnvVar
  let lambdaBody =
        C.EnumElim
          [(Ident.toInt normalEnvVarName, envVar), (Ident.toInt noeticEnvVarName, envVar)]
          switchVar
          normalPrefix
          [(EC.Int 1, noeticPrefix)]
          (map fst fvs)
          e'
  lambdaBody' <- liftIO $ Reduce.reduce (reduceHandle h) lambdaBody
  let args = map fst xts ++ [envVarName, switchVarName]
  AuxEnv.insert (auxEnvHandle h) name (opacity, args, lambdaBody')

callClosure :: Handle -> N.IsNoetic -> C.Comp -> [(Ident, C.Comp, C.Value)] -> IO C.Comp
callClosure h isNoetic e zexes = do
  let flag = if isNoetic then C.intValue1 else C.intValue0
  let (zs, es', xs) = unzip3 zexes
  ((closureVarName, closureVar), typeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames h
  return $
    Utility.bindLet
      ((closureVarName, e) : zip zs es')
      ( C.SigmaElim
          (not isNoetic)
          [typeVarName, envVarName, lamVarName]
          closureVar
          (C.PiElimDownElim lamVar (xs ++ [envVar, flag]))
      )

newClosureNames :: Handle -> IO ((Ident, C.Value), Ident, (Ident, C.Value), (Ident, C.Value))
newClosureNames h = do
  closureVarInfo <- Gensym.createVar (gensymHandle h) "closure"
  typeVarName <- Gensym.newIdentFromText (gensymHandle h) "exp"
  envVarInfo <- Gensym.createVar (gensymHandle h) "env"
  lamVarInfo <- Gensym.createVar (gensymHandle h) "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

callPrimOp :: PrimOp -> [(Ident, C.Comp, C.Value)] -> C.Comp
callPrimOp op zexes = do
  let (zs, es', xs) = unzip3 zexes
  Utility.bindLet (zip zs es') (C.Primitive (C.PrimOp op xs))

dropFst :: [(a, b, c)] -> [(b, c)]
dropFst xyzs = do
  let (_, ys, zs) = unzip3 xyzs
  zip ys zs

lambdaPrefixNormal ::
  [(Ident, C.Comp)] ->
  C.Value ->
  C.Comp
lambdaPrefixNormal fvs envVar = do
  C.SigmaElim True (map fst fvs) envVar (C.Phi $ map (C.VarLocal . fst) fvs)

lambdaPrefixNoetic ::
  Handle ->
  [(Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
lambdaPrefixNoetic h fvs envVar = do
  -- as == [APP-1, ..., APP-n]
  as <- forM fvs $ \(x, t) -> Utility.toRelevantApp (utilityHandle h) (C.VarLocal x) t
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.createVar (gensymHandle h) "pair") fvs
  body <- Linearize.linearize (linearizeHandle h) fvs $ Utility.bindLet (zip varNameList as) $ C.Phi varList
  return $ C.SigmaElim False (map fst fvs) envVar body
