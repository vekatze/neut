module Move.Scene.Clarify
  ( Handle,
    new,
    clarify,
    clarifyEntryPoint,
    registerFoundationalTypes,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.Maybe
import Move.Context.App
import Move.Context.CompDefinition qualified as CompDefinition
import Move.Context.EIO (EIO, raiseCritical, raiseCritical', toApp)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Clarify.Handle.AuxEnv qualified as AuxEnv
import Move.Scene.Clarify.Linearize qualified as Linearize
import Move.Scene.Clarify.Sigma qualified as Sigma
import Move.Scene.Clarify.Utility qualified as Utility
import Move.Scene.Comp.Reduce qualified as Reduce
import Move.Scene.Term.Subst qualified as Subst
import Rule.ArgNum qualified as AN
import Rule.Attr.DataIntro qualified as AttrDI
import Rule.Attr.Lam qualified as AttrL
import Rule.Attr.VarGlobal qualified as AttrVG
import Rule.BaseLowType qualified as BLT
import Rule.BaseName qualified as BN
import Rule.Binder
import Rule.Comp qualified as C
import Rule.DecisionTree qualified as DT
import Rule.DefiniteDescription qualified as DD
import Rule.Discriminant qualified as D
import Rule.EnumCase qualified as EC
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.LamKind qualified as LK
import Rule.Literal qualified as L
import Rule.Magic qualified as M
import Rule.Noema qualified as N
import Rule.Opacity (isOpaque)
import Rule.Opacity qualified as O
import Rule.OptimizableData qualified as OD
import Rule.Prim qualified as P
import Rule.PrimNumSize qualified as PNS
import Rule.PrimOp
import Rule.PrimType qualified as PT
import Rule.PrimValue qualified as PV
import Rule.Rune qualified as RU
import Rule.Stmt
import Rule.StmtKind
import Rule.Term qualified as TM
import Rule.Term.Chain (nubFreeVariables)
import Rule.Term.Chain qualified as TM
import Rule.Term.FromPrimNum

data Handle
  = Handle
  { gensymHandle :: Gensym.Handle,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle,
    auxEnvHandle :: AuxEnv.Handle,
    sigmaHandle :: Sigma.Handle,
    locatorHandle :: Locator.Handle,
    optDataHandle :: OptimizableData.Handle,
    reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle,
    compDefHandle :: CompDefinition.Handle,
    baseSize :: Int
  }

new :: App Handle
new = do
  gensymHandle <- Gensym.new
  linearizeHandle <- Linearize.new gensymHandle
  utilityHandle <- Utility.new gensymHandle
  auxEnvHandle <- AuxEnv.new
  sigmaHandle <- Sigma.new gensymHandle
  locatorHandle <- Locator.new
  optDataHandle <- OptimizableData.new
  reduceHandle <- Reduce.new gensymHandle
  substHandle <- Subst.new gensymHandle
  compDefHandle <- CompDefinition.new
  baseSize <- toApp Env.getBaseSize'
  return $ Handle {..}

clarify :: Handle -> [Stmt] -> EIO [C.CompStmt]
clarify h stmtList = do
  liftIO $ AuxEnv.clear (auxEnvHandle h)
  baseAuxEnv <- AuxEnv.toCompStmtList <$> liftIO (getBaseAuxEnv h)
  liftIO $ AuxEnv.clear (auxEnvHandle h)
  stmtList' <- do
    stmtList' <- mapM (clarifyStmt h) stmtList
    auxEnv <- liftIO $ AuxEnv.toCompStmtList <$> AuxEnv.get (auxEnvHandle h)
    return $ stmtList' ++ auxEnv
  forM_ (stmtList' ++ baseAuxEnv) $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        liftIO $ CompDefinition.insert (compDefHandle h) x (opacity, args, e)
      C.Foreign {} ->
        return ()
  forM stmtList' $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        e' <- liftIO $ Reduce.reduce (reduceHandle h) e
        -- printNote' "==================="
        -- printNote' $ DD.reify x
        -- printNote' $ T.pack $ show args
        -- printNote' $ T.pack $ show e'
        return $ C.Def x opacity args e'
      C.Foreign {} ->
        return stmt

clarifyEntryPoint :: Handle -> IO [C.CompStmt]
clarifyEntryPoint h = do
  AuxEnv.clear (auxEnvHandle h)
  baseAuxEnv <- getBaseAuxEnv h
  forM (Map.toList baseAuxEnv) $ \(x, (opacity, args, e)) -> do
    e' <- Reduce.reduce (reduceHandle h) e
    return $ C.Def x opacity args e'

registerFoundationalTypes :: Handle -> IO ()
registerFoundationalTypes h = do
  AuxEnv.clear (auxEnvHandle h)
  auxEnv <- getBaseAuxEnv h
  forM_ (Map.toList auxEnv) $ uncurry $ CompDefinition.insert (compDefHandle h)

getBaseAuxEnv :: Handle -> IO CompDefinition.DefMap
getBaseAuxEnv h = do
  Sigma.registerImmediateS4 (sigmaHandle h)
  Sigma.registerClosureS4 (sigmaHandle h)
  AuxEnv.get (auxEnvHandle h)

clarifyStmt :: Handle -> Stmt -> EIO C.CompStmt
clarifyStmt h stmt =
  case stmt of
    StmtDefine _ stmtKind (SavedHint m) f impArgs expArgs _ e -> do
      let xts = impArgs ++ expArgs
      xts' <- dropFst <$> clarifyBinder h IntMap.empty xts
      let tenv = TM.insTypeEnv xts IntMap.empty
      case stmtKind of
        Data name dataArgs consInfoList -> do
          od <- liftIO $ OptimizableData.lookup (optDataHandle h) name
          case od of
            Just OD.Enum -> do
              clarifyStmtDefineBody' h name xts' Sigma.returnImmediateS4
            Just OD.Unary
              | [(_, _, _, [(_, _, t)], _)] <- consInfoList -> do
                  (dataArgs', t') <- clarifyBinderBody h IntMap.empty dataArgs t
                  return $ C.Def f O.Opaque (map fst dataArgs') t'
              | otherwise ->
                  raiseCritical m "Found a broken unary data"
            _ -> do
              let dataInfo = map (\(_, _, _, consArgs, discriminant) -> (discriminant, dataArgs, consArgs)) consInfoList
              dataInfo' <- mapM (clarifyDataClause h) dataInfo
              liftIO (Sigma.returnSigmaDataS4 (sigmaHandle h) name O.Opaque dataInfo')
                >>= clarifyStmtDefineBody' h name xts'
        _ -> do
          e' <- clarifyStmtDefineBody h tenv xts' e
          return $ C.Def f (toLowOpacity stmtKind) (map fst xts') e'
    StmtForeign foreignList ->
      return $ C.Foreign foreignList

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
      return Sigma.returnImmediateS4
    _ :< TM.Var x -> do
      return $ C.UpIntro $ C.VarLocal x
    _ :< TM.VarGlobal (AttrVG.Attr {..}) x -> do
      return $
        C.UpIntro $
          C.SigmaIntro
            [ Sigma.immediateS4,
              C.SigmaIntro [],
              C.VarGlobal x argNum
            ]
    _ :< TM.Pi {} ->
      return Sigma.returnClosureS4
    _ :< TM.PiIntro attr impArgs expArgs e -> do
      clarifyLambda h tenv attr (TM.chainOf tenv [term]) (impArgs ++ expArgs) e
    _ :< TM.PiElim e es -> do
      es' <- mapM (clarifyPlus h tenv) es
      case e of
        _ :< TM.Prim (P.Value (PV.Op op)) ->
          return $ callPrimOp op es'
        _ -> do
          e' <- clarifyTerm h tenv e
          liftIO $ callClosure h e' es'
    _ :< TM.Data _ name dataArgs -> do
      (zs, dataArgs', xs) <- unzip3 <$> mapM (clarifyPlus h tenv) dataArgs
      return $
        Utility.bindLet (zip zs dataArgs') $
          C.PiElimDownElim (C.VarGlobal name (AN.fromInt (length dataArgs))) xs
    m :< TM.DataIntro (AttrDI.Attr {..}) consName dataArgs consArgs -> do
      od <- liftIO $ OptimizableData.lookup (optDataHandle h) consName
      case od of
        Just OD.Enum ->
          return $ C.UpIntro $ C.Int (PNS.IntSize (baseSize h)) (D.reify discriminant)
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
                  C.Int (PNS.IntSize (baseSize h)) (D.reify discriminant) : xs
    m :< TM.DataElim isNoetic xets tree -> do
      let (xs, es, _) = unzip3 xets
      let mxts = map (m,,m :< TM.Tau) xs
      es' <- mapM (clarifyTerm h tenv) es
      (tree', _) <- clarifyDecisionTree h (TM.insTypeEnv mxts tenv) isNoetic IntMap.empty tree
      return $ Utility.irreducibleBindLet (zip xs es') tree'
    _ :< TM.Box t -> do
      clarifyTerm h tenv t
    _ :< TM.BoxNoema {} ->
      return Sigma.returnImmediateS4
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
        P.Type _ ->
          return Sigma.returnImmediateS4
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
              let t = fromPrimNum m (PT.Int PNS.intSize32)
              clarifyTerm h tenv $ m :< TM.Prim (P.Value (PV.Int t PNS.intSize32 (RU.asInt r)))
    _ :< TM.Magic der -> do
      clarifyMagic h tenv der
    m :< TM.Resource _ resourceID _ discarder copier -> do
      liftedName <- liftIO $ Locator.attachCurrentLocator (locatorHandle h) $ BN.resourceName resourceID
      switchValue <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "switchValue"
      value <- liftIO $ Gensym.newIdentFromText (gensymHandle h) "value"
      discarder' <-
        clarifyTerm h IntMap.empty (m :< TM.PiElim discarder [m :< TM.Var value])
          >>= liftIO . Reduce.reduce (reduceHandle h)
      copier' <-
        clarifyTerm h IntMap.empty (m :< TM.PiElim copier [m :< TM.Var value])
          >>= liftIO . Reduce.reduce (reduceHandle h)
      enumElim <-
        liftIO $
          Utility.getEnumElim (utilityHandle h) [value] (C.VarLocal switchValue) copier' [(EC.Int 0, discarder')]
      isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) liftedName
      unless isAlreadyRegistered $ do
        liftIO $ AuxEnv.insert (auxEnvHandle h) liftedName (O.Clear, [switchValue, value], enumElim)
      return $ C.UpIntro $ C.VarGlobal liftedName AN.argNumS4
    _ :< TM.Void ->
      return Sigma.returnImmediateS4

embody :: Handle -> TM.TypeEnv -> [(BinderF TM.Term, TM.Term)] -> TM.Term -> EIO C.Comp
embody h tenv xets cont =
  case xets of
    [] ->
      clarifyTerm h tenv cont
    (mxt@(_, x, t), e) : rest -> do
      (typeExpVarName, typeExp, typeExpVar) <- clarifyPlus h tenv t
      (valueVarName, value, valueVar) <- clarifyPlus h tenv e
      cont' <- embody h (TM.insTypeEnv [mxt] tenv) rest cont
      cont'' <- liftIO $ Linearize.linearize (linearizeHandle h) [(x, typeExp)] cont'
      return $
        Utility.bindLet
          [ (typeExpVarName, typeExp),
            (valueVarName, value),
            (x, C.PiElimDownElim typeExpVar [C.Int (PNS.IntSize (baseSize h)) 1, valueVar])
          ]
          cont''

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
          cont'' <- tidyCursorList h tenv dataArgsMap consumedCursorList cont'
          return (cont'', chain)
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
          (disc, discVar) <- liftIO $ Gensym.newValueVarLocalWith (gensymHandle h) "disc"
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
    _ :< TM.PiElim (_ :< TM.Data _ dataName _) _ -> do
      liftIO $ OptimizableData.lookup (optDataHandle h) dataName
    _ :< TM.PiElim (_ :< TM.VarGlobal _ dataName) _ -> do
      liftIO $ OptimizableData.lookup (optDataHandle h) dataName
    _ :< TM.Prim (P.Type (PT.Int _)) -> do
      return $ Just OD.Enum
    _ :< TM.Prim (P.Type PT.Rune) -> do
      return $ Just OD.Enum
    _ ->
      raiseCritical' "Clarify.isEnumType"

tidyCursorList :: Handle -> TM.TypeEnv -> DataArgsMap -> [Ident] -> C.Comp -> EIO C.Comp
tidyCursorList h tenv dataArgsMap consumedCursorList cont =
  case consumedCursorList of
    [] ->
      return cont
    cursor : rest -> do
      case IntMap.lookup (Ident.toInt cursor) dataArgsMap of
        Nothing ->
          error "tidyCursor"
        Just (dataArgs, cursorSize) -> do
          let (dataArgVars, dataTypes) = unzip dataArgs
          dataTypes' <- mapM (clarifyTerm h tenv) dataTypes
          cont' <- tidyCursorList h tenv dataArgsMap rest cont
          liftIO $ Linearize.linearize (linearizeHandle h) (zip dataArgVars dataTypes') $ do
            C.Free (C.VarLocal cursor) cursorSize cont'

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
      let chain = prefixChain ++ contChain
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
      liftedName <- liftIO $ Locator.attachCurrentLocator (locatorHandle h) $ BN.muName identity
      let appArgs = fvs ++ mxts
      let appArgs' = map (\(mx, x, _) -> mx :< TM.Var x) appArgs
      let argNum = AN.fromInt $ length appArgs'
      let attr = AttrVG.new argNum
      lamAttr <- do
        c <- liftIO $ Gensym.newCount (gensymHandle h)
        return $ AttrL.normal c codType
      let lamApp = m :< TM.PiIntro lamAttr [] mxts (m :< TM.PiElim (m :< TM.VarGlobal attr liftedName) appArgs')
      isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) liftedName
      unless isAlreadyRegistered $ do
        liftedBody <- liftIO $ Subst.subst (substHandle h) (IntMap.fromList [(Ident.toInt recFuncName, Right lamApp)]) e
        (liftedArgs, liftedBody') <- clarifyBinderBody h IntMap.empty appArgs liftedBody
        liftedBody'' <- liftIO $ Linearize.linearize (linearizeHandle h) liftedArgs liftedBody'
        liftIO $ AuxEnv.insert (auxEnvHandle h) liftedName (O.Opaque, map fst liftedArgs, liftedBody'')
      clarifyTerm h tenv lamApp
    LK.Normal _ -> do
      e' <- clarifyTerm h (TM.insTypeEnv (catMaybes [AttrL.fromAttr attrL] ++ mxts) tenv) e
      returnClosure h tenv identity O.Clear fvs mxts e'

clarifyPlus :: Handle -> TM.TypeEnv -> TM.Term -> EIO (Ident, C.Comp, C.Value)
clarifyPlus h tenv e = do
  e' <- clarifyTerm h tenv e
  (varName, var) <- liftIO $ Gensym.newValueVarLocalWith (gensymHandle h) "var"
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
  (xs, varList) <- liftIO $ mapAndUnzipM (const (Gensym.newValueVarLocalWith (gensymHandle h) "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  lamID <- liftIO $ Gensym.newCount (gensymHandle h)
  returnClosure h tenv lamID O.Clear [] mxts $ C.Primitive (C.PrimOp op varList)

returnClosure ::
  Handle ->
  TM.TypeEnv ->
  Int ->
  O.Opacity ->
  [BinderF TM.Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF TM.Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  C.Comp -> -- the `e` in `lam (x1, ..., xn). e`
  EIO C.Comp
returnClosure h tenv lamID opacity fvs xts e = do
  fvs'' <- dropFst <$> clarifyBinder h tenv fvs
  xts'' <- dropFst <$> clarifyBinder h tenv xts
  fvEnvSigma <- liftIO $ Sigma.closureEnvS4 (sigmaHandle h) $ map Right fvs''
  let fvEnv = C.SigmaIntro (map (\(x, _) -> C.VarLocal x) fvs'')
  let argNum = AN.fromInt $ length xts'' + 1 -- argNum == count(xts) + env
  name <- liftIO $ Locator.attachCurrentLocator (locatorHandle h) $ BN.lambdaName lamID
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
registerClosure h name opacity xts1 xts2 e = do
  e' <- liftIO $ Linearize.linearize (linearizeHandle h) (xts2 ++ xts1) e
  (envVarName, envVar) <- Gensym.newValueVarLocalWith (gensymHandle h) "env"
  let args = map fst xts1 ++ [envVarName]
  body <- liftIO $ Reduce.reduce (reduceHandle h) $ C.SigmaElim True (map fst xts2) envVar e'
  AuxEnv.insert (auxEnvHandle h) name (opacity, args, body)

callClosure :: Handle -> C.Comp -> [(Ident, C.Comp, C.Value)] -> IO C.Comp
callClosure h e zexes = do
  let (zs, es', xs) = unzip3 zexes
  ((closureVarName, closureVar), typeVarName, (envVarName, envVar), (lamVarName, lamVar)) <- newClosureNames h
  return $
    Utility.bindLet
      ((closureVarName, e) : zip zs es')
      ( C.SigmaElim
          True
          [typeVarName, envVarName, lamVarName]
          closureVar
          (C.PiElimDownElim lamVar (xs ++ [envVar]))
      )

newClosureNames :: Handle -> IO ((Ident, C.Value), Ident, (Ident, C.Value), (Ident, C.Value))
newClosureNames h = do
  closureVarInfo <- Gensym.newValueVarLocalWith (gensymHandle h) "closure"
  typeVarName <- Gensym.newIdentFromText (gensymHandle h) "exp"
  envVarInfo <- Gensym.newValueVarLocalWith (gensymHandle h) "env"
  lamVarInfo <- Gensym.newValueVarLocalWith (gensymHandle h) "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

callPrimOp :: PrimOp -> [(Ident, C.Comp, C.Value)] -> C.Comp
callPrimOp op zexes = do
  let (zs, es', xs) = unzip3 zexes
  Utility.bindLet (zip zs es') (C.Primitive (C.PrimOp op xs))

dropFst :: [(a, b, c)] -> [(b, c)]
dropFst xyzs = do
  let (_, ys, zs) = unzip3 xyzs
  zip ys zs
