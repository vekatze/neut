module Scene.Clarify.Sigma
  ( registerImmediateS4,
    registerClosureS4,
    immediateS4,
    returnImmediateS4,
    returnClosureS4,
    closureEnvS4,
    returnSigmaDataS4,
    returnEnumS4,
  )
where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Locator qualified as Locator
import Control.Monad
import Entity.Arity qualified as A
import Entity.BaseName qualified as BN
import Entity.Comp qualified as C
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.EnumCase qualified as EC
import Entity.Ident
import Entity.LowType qualified as LT
import Entity.Magic qualified as M
import Entity.Opacity qualified as O
import Scene.Clarify.Linearize
import Scene.Clarify.Utility

registerImmediateS4 :: App ()
registerImmediateS4 = do
  let immediateT _ = return $ C.UpIntro $ C.SigmaIntro []
  let immediate4 arg = return $ C.UpIntro arg
  registerSwitcher O.Transparent DD.imm immediateT immediate4

registerClosureS4 :: App ()
registerClosureS4 = do
  (env, envVar) <- Gensym.newValueVarLocalWith "env"
  registerSigmaS4
    DD.cls
    O.Transparent
    [Right (env, returnImmediateS4), Left (C.UpIntro envVar), Left returnImmediateS4]

returnImmediateS4 :: C.Comp
returnImmediateS4 = do
  C.UpIntro immediateS4

returnClosureS4 :: C.Comp
returnClosureS4 = do
  C.UpIntro $ C.VarGlobal DD.cls A.arityS4

immediateS4 :: C.Value
immediateS4 = do
  C.VarGlobal DD.imm A.arityS4

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
      registerSwitcher O.Transparent name (sigmaT mxts) (sigma4 mxts)
      return $ C.VarGlobal name A.arityS4

returnSigmaDataS4 ::
  DD.DefiniteDescription ->
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  App C.Comp
returnSigmaDataS4 dataName dataInfo = do
  let aff = sigmaDataT dataInfo
  let rel = sigmaData4 dataInfo
  let dataName' = DD.getFormDD dataName
  registerSwitcher O.Opaque dataName' aff rel
  return $ C.UpIntro $ C.VarGlobal dataName' A.arityS4

returnEnumS4 ::
  DD.DefiniteDescription ->
  App C.Comp
returnEnumS4 dataName = do
  let aff _ = return $ C.UpIntro $ C.SigmaIntro []
  let rel arg = return $ C.UpIntro arg
  let dataName' = DD.getFormDD dataName
  registerSwitcher O.Transparent dataName' aff rel
  return $ C.UpIntro $ C.VarGlobal dataName' A.arityS4

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
          C.UpElim True disc (C.Primitive (C.Magic (M.Load LT.Pointer (C.VarLocal localName)))) enumElim

sigmaBinderT :: [(Ident, C.Comp)] -> C.Value -> App C.Comp
sigmaBinderT xts v = do
  sigmaT (Left returnImmediateS4 : map Right xts) v

discriminantToEnumCase :: D.Discriminant -> EC.EnumCase
discriminantToEnumCase discriminant =
  EC.Int (D.reify discriminant)
