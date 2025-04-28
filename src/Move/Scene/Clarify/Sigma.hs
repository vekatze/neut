module Move.Scene.Clarify.Sigma
  ( Handle,
    new,
    registerImmediateS4,
    registerClosureS4,
    immediateS4,
    returnImmediateS4,
    returnClosureS4,
    closureEnvS4,
    returnSigmaDataS4,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.Locator qualified as Locator
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Clarify.Linearize qualified as Linearize
import Move.Scene.Clarify.Utility qualified as Utility
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

data Handle
  = Handle
  { gensymHandle :: Gensym.Handle,
    linearizeHandle :: Linearize.Handle,
    locatorHandle :: Locator.Handle,
    utilityHandle :: Utility.Handle
  }

new :: Gensym.Handle -> App Handle
new gensymHandle = do
  linearizeHandle <- Linearize.new gensymHandle
  locatorHandle <- Locator.new
  utilityHandle <- Utility.new gensymHandle
  return $ Handle {..}

registerImmediateS4 :: Handle -> IO ()
registerImmediateS4 h = do
  let immediateT _ = return $ C.UpIntro $ C.SigmaIntro []
  let immediate4 arg = return $ C.UpIntro arg
  Utility.registerSwitcher (utilityHandle h) O.Clear DD.imm immediateT immediate4

registerClosureS4 :: Handle -> IO ()
registerClosureS4 h = do
  (env, envVar) <- Gensym.newValueVarLocalWith (gensymHandle h) "env"
  registerSigmaS4
    h
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
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  [Either C.Comp (Ident, C.Comp)] ->
  IO ()
registerSigmaS4 h name opacity mxts = do
  Utility.registerSwitcher (utilityHandle h) opacity name (sigmaT h mxts) (sigma4 h mxts)

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
  Handle ->
  [Either C.Comp (Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
sigmaT h mxts argVar = do
  xts <- liftIO $ mapM (supplyName (gensymHandle h)) mxts
  -- as == [APP-1, ..., APP-n]   (`a` here stands for `app`)
  as <- forM xts $ uncurry $ Utility.toAffineApp (utilityHandle h)
  ys <- mapM (const $ Gensym.newIdentFromText (gensymHandle h) "arg") xts
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip ys as) $ C.UpIntro $ C.SigmaIntro []
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
  Handle ->
  [Either C.Comp (Ident, C.Comp)] ->
  C.Value ->
  IO C.Comp
sigma4 h mxts argVar = do
  xts <- liftIO $ mapM (supplyName (gensymHandle h)) mxts
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ uncurry $ Utility.toRelevantApp (utilityHandle h)
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.newValueVarLocalWith (gensymHandle h) "pair") xts
  body' <- Linearize.linearize (linearizeHandle h) xts $ Utility.bindLet (zip varNameList as) $ C.UpIntro $ C.SigmaIntro varList
  return $ C.SigmaElim False (map fst xts) argVar body'

supplyName :: Gensym.Handle -> Either b (Ident, b) -> IO (Ident, b)
supplyName h mName =
  case mName of
    Right (x, t) ->
      return (x, t)
    Left t -> do
      x <- Gensym.newIdentFromText h "unused-sigarg"
      return (x, t)

closureEnvS4 ::
  Handle ->
  [Either C.Comp (Ident, C.Comp)] ->
  IO C.Value
closureEnvS4 h mxts =
  case mxts of
    [] ->
      return immediateS4 -- performance optimization; not necessary for correctness
    _ -> do
      i <- Gensym.newCount (gensymHandle h)
      name <- liftIO $ Locator.attachCurrentLocator (locatorHandle h) $ BN.sigmaName i
      liftIO $ Utility.registerSwitcher (utilityHandle h) O.Clear name (sigmaT h mxts) (sigma4 h mxts)
      return $ C.VarGlobal name AN.argNumS4

returnSigmaDataS4 ::
  Handle ->
  DD.DefiniteDescription ->
  O.Opacity ->
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  IO C.Comp
returnSigmaDataS4 h dataName opacity dataInfo = do
  let aff = sigmaDataT h dataInfo
  let rel = sigmaData4 h dataInfo
  let dataName' = DD.getFormDD dataName
  Utility.registerSwitcher (utilityHandle h) opacity dataName' aff rel
  return $ C.UpIntro $ C.VarGlobal dataName' AN.argNumS4

sigmaData4 :: Handle -> [(D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> IO C.Comp
sigmaData4 h = do
  sigmaData h (sigmaBinder4 h)

sigmaBinder4 :: Handle -> [(Ident, C.Comp)] -> C.Value -> IO C.Comp
sigmaBinder4 h xts v = do
  sigma4 h (Left returnImmediateS4 : map Right xts) v

sigmaDataT :: Handle -> [(D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> IO C.Comp
sigmaDataT h = do
  sigmaData h (sigmaBinderT h)

sigmaData ::
  Handle ->
  ([(Ident, C.Comp)] -> C.Value -> IO C.Comp) ->
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  C.Value ->
  IO C.Comp
sigmaData h resourceHandler dataInfo arg = do
  case dataInfo of
    [] ->
      return $ C.UpIntro arg
    _ -> do
      let (discList, binderList) = unzip dataInfo
      let discList' = map discriminantToEnumCase discList
      localName <- Gensym.newIdentFromText (gensymHandle h) "local"
      binderList' <- mapM (`resourceHandler` C.VarLocal localName) binderList
      (disc, discVar) <- Gensym.newValueVarLocalWith (gensymHandle h) "disc"
      enumElim <- Utility.getEnumElim (utilityHandle h) [localName] discVar (last binderList') (zip discList' (init binderList'))
      return $
        C.UpElim False localName (C.UpIntro arg) $
          C.UpElim True disc (C.Primitive (C.Magic (M.Load BLT.Pointer (C.VarLocal localName)))) enumElim

sigmaBinderT :: Handle -> [(Ident, C.Comp)] -> C.Value -> IO C.Comp
sigmaBinderT h xts v = do
  sigmaT h (Left returnImmediateS4 : map Right xts) v

discriminantToEnumCase :: D.Discriminant -> EC.EnumCase
discriminantToEnumCase discriminant =
  EC.Int (D.reify discriminant)
