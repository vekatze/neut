module Move.Scene.Clarify
  ( clarify,
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
import Move.Context.Clarify qualified as Clarify
import Move.Context.CompDefinition qualified as CompDefinition
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Gensym qualified as Gensym
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Throw qualified as Throw
import Move.Language.Utility.Gensym qualified as GensymN
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
  { gensymHandle :: GensymN.Handle,
    linearizeHandle :: Linearize.Handle,
    utilityHandle :: Utility.Handle,
    auxEnvHandle :: AuxEnv.Handle,
    sigmaHandle :: Sigma.Handle,
    locatorHandle :: Locator.Handle,
    optDataHandle :: OptimizableData.Handle,
    reduceHandle :: Reduce.Handle,
    substHandle :: Subst.Handle
  }

new :: App Handle
new = do
  gensymHandle <- GensymN.new
  linearizeHandle <- Linearize.new
  utilityHandle <- Utility.new
  auxEnvHandle <- AuxEnv.new
  sigmaHandle <- Sigma.new
  locatorHandle <- Locator.new
  optDataHandle <- OptimizableData.new
  reduceHandle <- Reduce.new
  substHandle <- Subst.new
  return $ Handle {..}

clarify :: [Stmt] -> App [C.CompStmt]
clarify stmtList = do
  Clarify.clearAuxEnv
  baseAuxEnv <- Clarify.toCompStmtList <$> getBaseAuxEnv
  Clarify.clearAuxEnv
  stmtList' <- do
    stmtList' <- mapM clarifyStmt stmtList
    auxEnv <- Clarify.toCompStmtList <$> Clarify.getAuxEnv
    return $ stmtList' ++ auxEnv
  forM_ (stmtList' ++ baseAuxEnv) $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        hc <- CompDefinition.new
        liftIO $ CompDefinition.insert hc x (opacity, args, e)
      C.Foreign {} ->
        return ()
  h <- Reduce.new
  forM stmtList' $ \stmt -> do
    case stmt of
      C.Def x opacity args e -> do
        e' <- liftIO $ Reduce.reduce h e
        -- printNote' "==================="
        -- printNote' $ DD.reify x
        -- printNote' $ T.pack $ show args
        -- printNote' $ T.pack $ show e'
        return $ C.Def x opacity args e'
      C.Foreign {} ->
        return stmt

clarifyEntryPoint :: App [C.CompStmt]
clarifyEntryPoint = do
  Clarify.clearAuxEnv
  baseAuxEnv <- getBaseAuxEnv
  h <- Reduce.new
  forM (Map.toList baseAuxEnv) $ \(x, (opacity, args, e)) -> do
    e' <- liftIO $ Reduce.reduce h e
    return $ C.Def x opacity args e'

registerFoundationalTypes :: App ()
registerFoundationalTypes = do
  Clarify.clearAuxEnv
  auxEnv <- getBaseAuxEnv
  hc <- CompDefinition.new
  liftIO $ forM_ (Map.toList auxEnv) $ uncurry $ CompDefinition.insert hc

getBaseAuxEnv :: App CompDefinition.DefMap
getBaseAuxEnv = do
  h <- Sigma.new
  liftIO $ Sigma.registerImmediateS4 h
  liftIO $ Sigma.registerClosureS4 h
  Clarify.getAuxEnv

clarifyStmt :: Stmt -> App C.CompStmt
clarifyStmt stmt =
  case stmt of
    StmtDefine _ stmtKind (SavedHint m) f impArgs expArgs _ e -> do
      let xts = impArgs ++ expArgs
      xts' <- dropFst <$> clarifyBinder IntMap.empty xts
      let tenv = TM.insTypeEnv xts IntMap.empty
      case stmtKind of
        Data name dataArgs consInfoList -> do
          od <- OptimizableData.lookup name
          case od of
            Just OD.Enum -> do
              clarifyStmtDefineBody' name xts' Sigma.returnImmediateS4
            Just OD.Unary
              | [(_, _, _, [(_, _, t)], _)] <- consInfoList -> do
                  (dataArgs', t') <- clarifyBinderBody IntMap.empty dataArgs t
                  return $ C.Def f O.Opaque (map fst dataArgs') t'
              | otherwise ->
                  Throw.raiseCritical m "Found a broken unary data"
            _ -> do
              let dataInfo = map (\(_, _, _, consArgs, discriminant) -> (discriminant, dataArgs, consArgs)) consInfoList
              dataInfo' <- mapM clarifyDataClause dataInfo
              h <- Sigma.new
              liftIO (Sigma.returnSigmaDataS4 h name O.Opaque dataInfo') >>= clarifyStmtDefineBody' name xts'
        _ -> do
          e' <- clarifyStmtDefineBody tenv xts' e
          return $ C.Def f (toLowOpacity stmtKind) (map fst xts') e'
    StmtForeign foreignList ->
      return $ C.Foreign foreignList

clarifyBinderBody ::
  TM.TypeEnv ->
  [BinderF TM.Term] ->
  TM.Term ->
  App ([(Ident, C.Comp)], C.Comp)
clarifyBinderBody tenv xts e =
  case xts of
    [] -> do
      e' <- clarifyTerm tenv e
      return ([], e')
    (m, x, t) : rest -> do
      t' <- clarifyTerm tenv t
      (binder, e') <- clarifyBinderBody (TM.insTypeEnv [(m, x, t)] tenv) rest e
      return ((x, t') : binder, e')

clarifyStmtDefineBody ::
  TM.TypeEnv ->
  [(Ident, C.Comp)] ->
  TM.Term ->
  App C.Comp
clarifyStmtDefineBody tenv xts e = do
  h <- Linearize.new
  h' <- Reduce.new
  clarifyTerm tenv e >>= liftIO . Linearize.linearize h xts >>= liftIO . Reduce.reduce h'

clarifyStmtDefineBody' ::
  DD.DefiniteDescription ->
  [(Ident, C.Comp)] ->
  C.Comp ->
  App C.CompStmt
clarifyStmtDefineBody' name xts' dataType = do
  h <- Linearize.new
  h' <- Reduce.new
  dataType' <- liftIO (Linearize.linearize h xts' dataType) >>= liftIO . Reduce.reduce h'
  return $ C.Def name O.Clear (map fst xts') dataType'

clarifyTerm :: TM.TypeEnv -> TM.Term -> App C.Comp
clarifyTerm tenv term =
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
      h <- new
      clarifyLambda h tenv attr (TM.chainOf tenv [term]) (impArgs ++ expArgs) e
    _ :< TM.PiElim e es -> do
      h <- new
      es' <- mapM (clarifyPlus h tenv) es
      case e of
        _ :< TM.Prim (P.Value (PV.Op op)) ->
          return $ callPrimOp op es'
        _ -> do
          e' <- clarifyTerm tenv e
          liftIO $ callClosure h e' es'
    _ :< TM.Data _ name dataArgs -> do
      h <- new
      (zs, dataArgs', xs) <- unzip3 <$> mapM (clarifyPlus h tenv) dataArgs
      return $
        Utility.bindLet (zip zs dataArgs') $
          C.PiElimDownElim (C.VarGlobal name (AN.fromInt (length dataArgs))) xs
    m :< TM.DataIntro (AttrDI.Attr {..}) consName dataArgs consArgs -> do
      od <- OptimizableData.lookup consName
      baseSize <- toApp $ Env.getBaseSize m
      case od of
        Just OD.Enum ->
          return $ C.UpIntro $ C.Int (PNS.IntSize baseSize) (D.reify discriminant)
        Just OD.Unary
          | [e] <- consArgs ->
              clarifyTerm tenv e
          | otherwise ->
              Throw.raiseCritical m "Found a malformed unary data in Scene.Clarify.clarifyTerm"
        _ -> do
          h <- new
          (zs, es, xs) <- fmap unzip3 $ mapM (clarifyPlus h tenv) $ dataArgs ++ consArgs
          return $
            Utility.bindLet (zip zs es) $
              C.UpIntro $
                C.SigmaIntro $
                  C.Int (PNS.IntSize baseSize) (D.reify discriminant) : xs
    m :< TM.DataElim isNoetic xets tree -> do
      let (xs, es, _) = unzip3 xets
      let mxts = map (m,,m :< TM.Tau) xs
      es' <- mapM (clarifyTerm tenv) es
      h <- new
      (tree', _) <- clarifyDecisionTree h (TM.insTypeEnv mxts tenv) isNoetic IntMap.empty tree
      return $ Utility.irreducibleBindLet (zip xs es') tree'
    _ :< TM.Box t -> do
      clarifyTerm tenv t
    _ :< TM.BoxNoema {} ->
      return Sigma.returnImmediateS4
    _ :< TM.BoxIntro letSeq e -> do
      embody tenv letSeq e
    _ :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      clarifyTerm tenv $
        TM.fromLetSeqOpaque castSeq $
          TM.fromLetSeq ((mxt, e1) : uncastSeq) e2
    _ :< TM.Let opacity mxt@(_, x, _) e1 e2 -> do
      e2' <- clarifyTerm (TM.insTypeEnv [mxt] tenv) e2
      mxts' <- dropFst <$> clarifyBinder tenv [mxt]
      h <- Linearize.new
      e2'' <- liftIO $ Linearize.linearize h mxts' e2'
      e1' <- clarifyTerm tenv e1
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
              h <- new
              clarifyPrimOp h tenv op m
            PV.StaticText _ text ->
              return $ C.UpIntro $ C.VarStaticText text
            PV.Rune r -> do
              let t = fromPrimNum m (PT.Int PNS.intSize32)
              clarifyTerm tenv $ m :< TM.Prim (P.Value (PV.Int t PNS.intSize32 (RU.asInt r)))
    _ :< TM.Magic der -> do
      clarifyMagic tenv der
    m :< TM.Resource _ resourceID _ discarder copier -> do
      h <- Locator.new
      liftedName <- liftIO $ Locator.attachCurrentLocator h $ BN.resourceName resourceID
      switchValue <- Gensym.newIdentFromText "switchValue"
      value <- Gensym.newIdentFromText "value"
      h' <- Reduce.new
      discarder' <- clarifyTerm IntMap.empty (m :< TM.PiElim discarder [m :< TM.Var value]) >>= liftIO . Reduce.reduce h'
      copier' <- clarifyTerm IntMap.empty (m :< TM.PiElim copier [m :< TM.Var value]) >>= liftIO . Reduce.reduce h'
      hUtil <- Utility.new
      enumElim <- liftIO $ Utility.getEnumElim hUtil [value] (C.VarLocal switchValue) copier' [(EC.Int 0, discarder')]
      isAlreadyRegistered <- Clarify.checkIfAlreadyRegistered liftedName
      unless isAlreadyRegistered $ do
        Clarify.insertToAuxEnv liftedName (O.Clear, [switchValue, value], enumElim)
      return $ C.UpIntro $ C.VarGlobal liftedName AN.argNumS4
    _ :< TM.Void ->
      return Sigma.returnImmediateS4

embody :: TM.TypeEnv -> [(BinderF TM.Term, TM.Term)] -> TM.Term -> App C.Comp
embody tenv xets cont =
  case xets of
    [] ->
      clarifyTerm tenv cont
    (mxt@(m, x, t), e) : rest -> do
      h <- new
      (typeExpVarName, typeExp, typeExpVar) <- clarifyPlus h tenv t
      (valueVarName, value, valueVar) <- clarifyPlus h tenv e
      cont' <- embody (TM.insTypeEnv [mxt] tenv) rest cont
      baseSize <- toApp $ Env.getBaseSize m
      cont'' <- liftIO $ Linearize.linearize (linearizeHandle h) [(x, typeExp)] cont'
      return $
        Utility.bindLet
          [ (typeExpVarName, typeExp),
            (valueVarName, value),
            (x, C.PiElimDownElim typeExpVar [C.Int (PNS.IntSize baseSize) 1, valueVar])
          ]
          cont''

type Size =
  Int

type DataArgsMap = IntMap.IntMap ([(Ident, TM.Term)], Size)

clarifyDataClause ::
  (D.Discriminant, [BinderF TM.Term], [BinderF TM.Term]) ->
  App (D.Discriminant, [(Ident, C.Comp)])
clarifyDataClause (discriminant, dataArgs, consArgs) = do
  let args = dataArgs ++ consArgs
  args' <- dropFst <$> clarifyBinder IntMap.empty args
  return (discriminant, args')

clarifyDecisionTree ::
  Handle ->
  TM.TypeEnv ->
  N.IsNoetic ->
  DataArgsMap ->
  DT.DecisionTree TM.Term ->
  App (C.Comp, [BinderF TM.Term])
clarifyDecisionTree h tenv isNoetic dataArgsMap tree =
  case tree of
    DT.Leaf consumedCursorList letSeq cont@(m :< _) -> do
      let chain = TM.chainOfDecisionTree tenv m tree
      cont' <- clarifyTerm tenv $ TM.fromLetSeq letSeq cont
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
          (disc, discVar) <- liftIO $ GensymN.newValueVarLocalWith (gensymHandle h) "disc"
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

getClauseDataGroup :: Handle -> TM.Term -> App (Maybe OD.OptimizableData)
getClauseDataGroup h term =
  case term of
    _ :< TM.Data _ dataName _ -> do
      liftIO $ OptimizableData.lookupH (optDataHandle h) dataName
    _ :< TM.PiElim (_ :< TM.Data _ dataName _) _ -> do
      liftIO $ OptimizableData.lookupH (optDataHandle h) dataName
    _ :< TM.PiElim (_ :< TM.VarGlobal _ dataName) _ -> do
      liftIO $ OptimizableData.lookupH (optDataHandle h) dataName
    _ :< TM.Prim (P.Type (PT.Int _)) -> do
      return $ Just OD.Enum
    _ :< TM.Prim (P.Type PT.Rune) -> do
      return $ Just OD.Enum
    _ ->
      Throw.raiseCritical' "Clarify.isEnumType"

tidyCursorList :: Handle -> TM.TypeEnv -> DataArgsMap -> [Ident] -> C.Comp -> App C.Comp
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
          dataTypes' <- mapM (clarifyTerm tenv) dataTypes
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
  App (EC.EnumCase, C.Comp, [BinderF TM.Term])
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
      dataArgVars <- liftIO $ mapM (const $ GensymN.newIdentFromText (gensymHandle h) "dataArg") dataTypes
      let cursorSize = 1 + length dataArgVars + length consArgs
      let dataArgsMap' = IntMap.insert (Ident.toInt cursor) (zip dataArgVars dataTypes, cursorSize) dataArgsMap
      let consArgs' = map (\(m, x, _) -> (m, x, m :< TM.Tau)) consArgs
      let prefixChain = TM.chainOfCaseWithoutCont tenv decisionCase
      (body', contChain) <- clarifyDecisionTree h (TM.insTypeEnv consArgs' tenv) isNoetic dataArgsMap' cont
      let chain = prefixChain ++ contChain
      od <- liftIO $ OptimizableData.lookupH (optDataHandle h) consDD
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
              Throw.raiseCritical' "Found a non-unary consArgs for unary ADT"
        _ -> do
          discriminantVar <- liftIO $ GensymN.newIdentFromText (gensymHandle h) "discriminant"
          return
            ( EC.Int (D.reify disc),
              C.SigmaElim
                False
                (discriminantVar : dataArgVars ++ map (\(_, x, _) -> x) consArgs)
                (C.VarLocal cursor)
                body',
              chain
            )

alignFreeVariable :: Handle -> TM.TypeEnv -> [BinderF TM.Term] -> C.Comp -> App C.Comp
alignFreeVariable h tenv fvs e = do
  fvs' <- dropFst <$> clarifyBinder tenv fvs
  liftIO $ Linearize.linearize (linearizeHandle h) fvs' e

clarifyMagic :: TM.TypeEnv -> M.Magic BLT.BaseLowType TM.Term -> App C.Comp
clarifyMagic tenv der = do
  h <- new
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
      clarifyTerm tenv e

clarifyLambda ::
  Handle ->
  TM.TypeEnv ->
  AttrL.Attr TM.Term ->
  [BinderF TM.Term] ->
  [BinderF TM.Term] ->
  TM.Term ->
  App C.Comp
clarifyLambda h tenv attrL@(AttrL.Attr {lamKind, identity}) fvs mxts e@(m :< _) = do
  case lamKind of
    LK.Fix (_, recFuncName, codType) -> do
      liftedName <- liftIO $ Locator.attachCurrentLocator (locatorHandle h) $ BN.muName identity
      let appArgs = fvs ++ mxts
      let appArgs' = map (\(mx, x, _) -> mx :< TM.Var x) appArgs
      let argNum = AN.fromInt $ length appArgs'
      let attr = AttrVG.new argNum
      lamAttr <- do
        c <- liftIO $ GensymN.newCount (gensymHandle h)
        return $ AttrL.normal c codType
      let lamApp = m :< TM.PiIntro lamAttr [] mxts (m :< TM.PiElim (m :< TM.VarGlobal attr liftedName) appArgs')
      isAlreadyRegistered <- liftIO $ AuxEnv.checkIfAlreadyRegistered (auxEnvHandle h) liftedName
      unless isAlreadyRegistered $ do
        -- hsubst <- TM.new
        liftedBody <- liftIO $ Subst.subst (substHandle h) (IntMap.fromList [(Ident.toInt recFuncName, Right lamApp)]) e
        (liftedArgs, liftedBody') <- clarifyBinderBody IntMap.empty appArgs liftedBody
        liftedBody'' <- liftIO $ Linearize.linearize (linearizeHandle h) liftedArgs liftedBody'
        liftIO $ AuxEnv.insert (auxEnvHandle h) liftedName (O.Opaque, map fst liftedArgs, liftedBody'')
      clarifyTerm tenv lamApp
    LK.Normal _ -> do
      e' <- clarifyTerm (TM.insTypeEnv (catMaybes [AttrL.fromAttr attrL] ++ mxts) tenv) e
      returnClosure h tenv identity O.Clear fvs mxts e'

clarifyPlus :: Handle -> TM.TypeEnv -> TM.Term -> App (Ident, C.Comp, C.Value)
clarifyPlus h tenv e = do
  e' <- clarifyTerm tenv e
  (varName, var) <- liftIO $ GensymN.newValueVarLocalWith (gensymHandle h) "var"
  return (varName, e', var)

clarifyBinder :: TM.TypeEnv -> [BinderF TM.Term] -> App [(Hint, Ident, C.Comp)]
clarifyBinder tenv binder =
  case binder of
    [] ->
      return []
    ((m, x, t) : xts) -> do
      t' <- clarifyTerm tenv t
      xts' <- clarifyBinder (IntMap.insert (Ident.toInt x) t tenv) xts
      return $ (m, x, t') : xts'

clarifyPrimOp :: Handle -> TM.TypeEnv -> PrimOp -> Hint -> App C.Comp
clarifyPrimOp h tenv op m = do
  let (domList, _) = getTypeInfo op
  let argTypeList = map (fromPrimNum m) domList
  (xs, varList) <- liftIO $ mapAndUnzipM (const (GensymN.newValueVarLocalWith (gensymHandle h) "prim")) domList
  let mxts = zipWith (\x t -> (m, x, t)) xs argTypeList
  lamID <- liftIO $ GensymN.newCount (gensymHandle h)
  returnClosure h tenv lamID O.Clear [] mxts $ C.Primitive (C.PrimOp op varList)

returnClosure ::
  Handle ->
  TM.TypeEnv ->
  Int ->
  O.Opacity ->
  [BinderF TM.Term] -> -- list of free variables in `lam (x1, ..., xn). e` (this must be a closed chain)
  [BinderF TM.Term] -> -- the `(x1 : A1, ..., xn : An)` in `lam (x1 : A1, ..., xn : An). e`
  C.Comp -> -- the `e` in `lam (x1, ..., xn). e`
  App C.Comp
returnClosure h tenv lamID opacity fvs xts e = do
  fvs'' <- dropFst <$> clarifyBinder tenv fvs
  xts'' <- dropFst <$> clarifyBinder tenv xts
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
  (envVarName, envVar) <- GensymN.newValueVarLocalWith (gensymHandle h) "env"
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
  closureVarInfo <- GensymN.newValueVarLocalWith (gensymHandle h) "closure"
  typeVarName <- GensymN.newIdentFromText (gensymHandle h) "exp"
  envVarInfo <- GensymN.newValueVarLocalWith (gensymHandle h) "env"
  lamVarInfo <- GensymN.newValueVarLocalWith (gensymHandle h) "thunk"
  return (closureVarInfo, typeVarName, envVarInfo, lamVarInfo)

callPrimOp :: PrimOp -> [(Ident, C.Comp, C.Value)] -> C.Comp
callPrimOp op zexes = do
  let (zs, es', xs) = unzip3 zexes
  Utility.bindLet (zip zs es') (C.Primitive (C.PrimOp op xs))

dropFst :: [(a, b, c)] -> [(b, c)]
dropFst xyzs = do
  let (_, ys, zs) = unzip3 xyzs
  zip ys zs
