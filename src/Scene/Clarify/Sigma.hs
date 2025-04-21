module Scene.Clarify.Sigma
  ( registerImmediateS4,
    registerClosureS4,
    immediateS4,
    returnImmediateS4,
    returnClosureS4,
    closureEnvS4,
    returnSigmaDataS4,
  )
where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Locator qualified as Locator
import Control.Monad
import Rule.ArgNum qualified as AN
import Rule.BaseLowType qualified as BLT
import Rule.BaseName qualified as BN
import Rule.Comp qualified as C
import Rule.DefiniteDescription qualified as DD
import Rule.Discriminant qualified as D
import Rule.EnumCase qualified as EC
import Rule.Ident
import Rule.Magic qualified as M
import Rule.Opacity qualified as O
import Scene.Clarify.Linearize
import Scene.Clarify.Utility

registerImmediateS4 :: App ()
registerImmediateS4 = do
  let immediateT _ = return $ C.UpIntro $ C.SigmaIntro []
  let immediate4 arg = return $ C.UpIntro arg
  registerSwitcher O.Clear DD.imm immediateT immediate4

registerClosureS4 :: App ()
registerClosureS4 = do
  (env, envVar) <- Gensym.newValueVarLocalWith "env"
  registerSigmaS4
    DD.cls
    O.Clear
    [Right (env, returnImmediateS4), Left (C.UpIntro envVar), Left returnImmediateS4]

returnImmediateS4 :: C.Comp
returnImmediateS4 = do
  C.UpIntro immediateS4

returnClosureS4 :: C.Comp
returnClosureS4 = do
  C.UpIntro $ C.VarGlobal DD.cls AN.argNumS4

immediateS4 :: C.Value
immediateS4 = do
  C.VarGlobal DD.imm AN.argNumS4

registerSigmaS4 ::
  DD.DefiniteDescription ->
  O.Opacity ->
  [Either C.Comp (Ident, C.Comp)] ->
  App ()
registerSigmaS4 name opacity mxts = do
  registerSwitcher opacity name (sigmaT mxts) (sigma4 mxts)

-- (Assuming `ti` = `return di` for some `di` such that `xi : di`)
-- sigmaT NAME LOC [(x1, t1), ..., (xn, tn)]   ~>
--   update CompEnv with NAME ~> (thunk LAM), where LAM is:
--   lam z.
--     let (x1, ..., xn) := z in
--     <LINEARIZE_HEADER for x1, .., xn> in                     ---
--     bind y1 :=                                    ---        ---
--       bind f1 = t1 in              ---            ---        ---
--       f1 @ (0, x1) in              ---  APP-1     ---        ---
--     ...                                           ---  body  ---  body'
--     bind yn :=                                    ---        ---
--       bind fn = tn in              ---            ---        ---
--       fn @ (0, xn) in              ---  APP-n     ---        ---
--     return ()                                     ---        ---
--
sigmaT ::
  [Either C.Comp (Ident, C.Comp)] ->
  C.Value ->
  App C.Comp
sigmaT mxts argVar = do
  xts <- mapM supplyName mxts
  -- as == [APP-1, ..., APP-n]   (`a` here stands for `app`)
  as <- forM xts $ uncurry toAffineApp
  ys <- mapM (const $ Gensym.newIdentFromText "arg") xts
  body' <- linearize xts $ bindLet (zip ys as) $ C.UpIntro $ C.SigmaIntro []
  return $ C.SigmaElim True (map fst xts) argVar body'

-- (Assuming `ti` = `return di` for some `di` such that `xi : di`)
-- sigma4 NAME LOC [(x1, t1), ..., (xn, tn)]   ~>
--   update CompEnv with NAME ~> (thunk LAM), where LAM is:
--   lam z.
--     let-without-free (x1, ..., xn) := z in
--     <LINEARIZE_HEADER for x1, .., xn> in                                      ---
--     bind x1' :=                                                     ---       ---
--       bind f1 = t1 in              ---                              ---       ---
--       f1 @ (1, x1) in              ---  APP-1                       ---       ---
--     ...                                                             ---       ---
--     bind xn' :=                                                     --- body  --- body'
--       bind fn = tn in              ---                              ---       ---
--       fn @ (1, xn) in              ---  APP-n                       ---       ---
--     return (x1', ..., xn')
sigma4 ::
  [Either C.Comp (Ident, C.Comp)] ->
  C.Value ->
  App C.Comp
sigma4 mxts argVar = do
  xts <- mapM supplyName mxts
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ uncurry toRelevantApp
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.newValueVarLocalWith "pair") xts
  body' <- linearize xts $ bindLet (zip varNameList as) $ C.UpIntro $ C.SigmaIntro varList
  return $ C.SigmaElim False (map fst xts) argVar body'

supplyName :: Either b (Ident, b) -> App (Ident, b)
supplyName mName =
  case mName of
    Right (x, t) ->
      return (x, t)
    Left t -> do
      x <- Gensym.newIdentFromText "unused-sigarg"
      return (x, t)

closureEnvS4 ::
  [Either C.Comp (Ident, C.Comp)] ->
  App C.Value
closureEnvS4 mxts =
  case mxts of
    [] ->
      return immediateS4 -- performance optimization; not necessary for correctness
    _ -> do
      i <- Gensym.newCount
      name <- Locator.attachCurrentLocator $ BN.sigmaName i
      registerSwitcher O.Clear name (sigmaT mxts) (sigma4 mxts)
      return $ C.VarGlobal name AN.argNumS4

returnSigmaDataS4 ::
  DD.DefiniteDescription ->
  O.Opacity ->
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  App C.Comp
returnSigmaDataS4 dataName opacity dataInfo = do
  let aff = sigmaDataT dataInfo
  let rel = sigmaData4 dataInfo
  let dataName' = DD.getFormDD dataName
  registerSwitcher opacity dataName' aff rel
  return $ C.UpIntro $ C.VarGlobal dataName' AN.argNumS4

sigmaData4 :: [(D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> App C.Comp
sigmaData4 = do
  sigmaData sigmaBinder4

sigmaBinder4 :: [(Ident, C.Comp)] -> C.Value -> App C.Comp
sigmaBinder4 xts v = do
  sigma4 (Left returnImmediateS4 : map Right xts) v

sigmaDataT :: [(D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> App C.Comp
sigmaDataT = do
  sigmaData sigmaBinderT

sigmaData ::
  ([(Ident, C.Comp)] -> C.Value -> App C.Comp) ->
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  C.Value ->
  App C.Comp
sigmaData resourceHandler dataInfo arg = do
  case dataInfo of
    [] ->
      return $ C.UpIntro arg
    _ -> do
      let (discList, binderList) = unzip dataInfo
      let discList' = map discriminantToEnumCase discList
      localName <- Gensym.newIdentFromText "local"
      binderList' <- mapM (`resourceHandler` C.VarLocal localName) binderList
      (disc, discVar) <- Gensym.newValueVarLocalWith "disc"
      enumElim <- getEnumElim [localName] discVar (last binderList') (zip discList' (init binderList'))
      return $
        C.UpElim False localName (C.UpIntro arg) $
          C.UpElim True disc (C.Primitive (C.Magic (M.Load BLT.Pointer (C.VarLocal localName)))) enumElim

sigmaBinderT :: [(Ident, C.Comp)] -> C.Value -> App C.Comp
sigmaBinderT xts v = do
  sigmaT (Left returnImmediateS4 : map Right xts) v

discriminantToEnumCase :: D.Discriminant -> EC.EnumCase
discriminantToEnumCase discriminant =
  EC.Int (D.reify discriminant)
