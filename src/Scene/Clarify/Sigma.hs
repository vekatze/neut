module Scene.Clarify.Sigma
  ( registerImmediateS4,
    registerClosureS4,
    registerCellS4,
    immediateS4,
    returnImmediateS4,
    returnClosureS4,
    returnCellS4,
    closureEnvS4,
    returnSigmaDataS4,
  )
where

import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import Control.Comonad.Cofree
import Control.Monad
import qualified Entity.Arity as A
import qualified Entity.BaseName as BN
import qualified Entity.Comp as C
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import qualified Entity.EnumCase as EC
import Entity.Ident
import qualified Entity.LowType as LT
import qualified Entity.Magic as M
import Scene.Clarify.Context
import Scene.Clarify.Linearize
import Scene.Clarify.Utility

registerImmediateS4 :: Context m => m ()
registerImmediateS4 = do
  let immediateT _ = return $ C.UpIntro $ C.SigmaIntro []
  let immediate4 arg = return $ C.UpIntro arg
  registerSwitcher DD.imm immediateT immediate4

registerClosureS4 :: Context m => m ()
registerClosureS4 = do
  (env, envVar) <- Gensym.newValueVarLocalWith "env"
  registerSigmaS4
    DD.cls
    [Right (env, returnImmediateS4), Left (C.UpIntro envVar), Left returnImmediateS4]

registerCellS4 :: Context m => m ()
registerCellS4 = do
  (env, envVar) <- Gensym.newValueVarLocalWith "env"
  registerSigmaS4
    DD.cell
    [Right (env, returnImmediateS4), Left (C.UpIntro envVar)] -- Sigma [A: tau, _: A]

returnImmediateS4 :: C.Comp
returnImmediateS4 = do
  C.UpIntro immediateS4

returnClosureS4 :: C.Comp
returnClosureS4 = do
  C.UpIntro $ C.VarGlobal DD.cls A.arityS4

returnCellS4 :: C.Comp
returnCellS4 = do
  C.UpIntro $ C.VarGlobal DD.cell A.arityS4

immediateS4 :: C.Value
immediateS4 = do
  C.VarGlobal DD.imm A.arityS4

registerSigmaS4 ::
  Context m =>
  DD.DefiniteDescription ->
  [Either C.Comp (Ident, C.Comp)] ->
  m ()
registerSigmaS4 name mxts = do
  registerSwitcher name (sigmaT mxts) (sigma4 mxts)

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
  Gensym.Context m =>
  [Either C.Comp (Ident, C.Comp)] ->
  C.Value ->
  m C.Comp
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
  Gensym.Context m =>
  [Either C.Comp (Ident, C.Comp)] ->
  C.Value ->
  m C.Comp
sigma4 mxts argVar = do
  xts <- mapM supplyName mxts
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ uncurry toRelevantApp
  (varNameList, varList) <- mapAndUnzipM (const $ Gensym.newValueVarLocalWith "pair") xts
  body' <- linearize xts $ bindLet (zip varNameList as) $ C.UpIntro $ C.SigmaIntro varList
  return $ C.SigmaElim False (map fst xts) argVar body'

supplyName :: Gensym.Context m => Either b (Ident, b) -> m (Ident, b)
supplyName mName =
  case mName of
    Right (x, t) ->
      return (x, t)
    Left t -> do
      x <- Gensym.newIdentFromText "unused-sigarg"
      return (x, t)

closureEnvS4 ::
  Context m =>
  [Either C.Comp (Ident, C.Comp)] ->
  m C.Value
closureEnvS4 mxts =
  case mxts of
    [] ->
      return immediateS4 -- performance optimization; not necessary for correctness
    _ -> do
      i <- Gensym.newCount
      name <- Locator.attachCurrentLocator $ BN.sigmaName i
      registerSwitcher name (sigmaT mxts) (sigma4 mxts)
      return $ C.VarGlobal name A.arityS4

returnSigmaDataS4 ::
  Context m =>
  DD.DefiniteDescription ->
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  m C.Comp
returnSigmaDataS4 dataName dataInfo = do
  let aff = sigmaDataT dataInfo
  let rel = sigmaData4 dataInfo
  let dataName' = DD.getConsDD dataName
  registerSwitcher dataName' aff rel
  return $ C.UpIntro $ C.VarGlobal dataName' A.arityS4

sigmaData4 :: Context m => [(D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> m C.Comp
sigmaData4 = do
  sigmaData sigmaBinder4

sigmaBinder4 :: Context m => [(Ident, C.Comp)] -> C.Value -> m C.Comp
sigmaBinder4 xts v = do
  sigma4 (Left returnImmediateS4 : map Right xts) v

sigmaDataT :: Context m => [(D.Discriminant, [(Ident, C.Comp)])] -> C.Value -> m C.Comp
sigmaDataT = do
  sigmaData sigmaBinderT

sigmaData ::
  Context m =>
  ([(Ident, C.Comp)] -> C.Value -> m C.Comp) ->
  [(D.Discriminant, [(Ident, C.Comp)])] ->
  C.Value ->
  m C.Comp
sigmaData resourceHandler dataInfo arg = do
  case dataInfo of
    [] ->
      return $ C.UpIntro arg
    _ -> do
      let (discriminantList, binderList) = unzip dataInfo
      let discriminantList' = map discriminantToEnumCase discriminantList
      binderList' <- mapM (`resourceHandler` arg) binderList
      discriminantVar <- Gensym.newIdentFromText "discriminant"
      return $
        C.UpElim discriminantVar (C.Primitive (C.Magic (M.Load LT.voidPtr arg))) $
          C.EnumElim (C.VarLocal discriminantVar) (last binderList') (zip discriminantList' (init binderList'))

sigmaBinderT :: Context m => [(Ident, C.Comp)] -> C.Value -> m C.Comp
sigmaBinderT xts v = do
  sigmaT (Left returnImmediateS4 : map Right xts) v

discriminantToEnumCase :: D.Discriminant -> EC.CompEnumCase
discriminantToEnumCase discriminant =
  () :< EC.Int (D.reify discriminant)
