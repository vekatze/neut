module Scene.Clarify.Sigma
  ( registerImmediateS4,
    registerClosureS4,
    registerCellS4,
    immediateS4,
    returnImmediateS4,
    returnClosureS4,
    returnCellS4,
    closureEnvS4,
  )
where

import qualified Context.App as App
import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import Control.Monad
import qualified Entity.Arity as A
import qualified Entity.BaseName as BN
import Entity.Comp
import qualified Entity.DefiniteDescription as DD
import Entity.Ident
import Scene.Clarify.Context
import Scene.Clarify.Linearize
import Scene.Clarify.Utility

registerImmediateS4 :: Context -> IO ()
registerImmediateS4 ctx = do
  let immediateT _ = return $ CompUpIntro $ ValueSigmaIntro []
  let immediate4 arg = return $ CompUpIntro arg
  registerSwitcher ctx DD.imm immediateT immediate4

registerClosureS4 :: Context -> IO ()
registerClosureS4 ctx = do
  (env, envVar) <- Gensym.newValueVarLocalWith (App.gensym (base ctx)) "env"
  registerSigmaS4
    ctx
    DD.cls
    [Right (env, returnImmediateS4), Left (CompUpIntro envVar), Left returnImmediateS4]

registerCellS4 :: Context -> IO ()
registerCellS4 ctx = do
  (env, envVar) <- Gensym.newValueVarLocalWith (App.gensym (base ctx)) "env"
  registerSigmaS4
    ctx
    DD.cell
    [Right (env, returnImmediateS4), Left (CompUpIntro envVar)] -- Sigma [A: tau, _: A]

returnImmediateS4 :: Comp
returnImmediateS4 = do
  CompUpIntro immediateS4

returnClosureS4 :: Comp
returnClosureS4 = do
  CompUpIntro $ ValueVarGlobal DD.cls A.arityS4

returnCellS4 :: Comp
returnCellS4 = do
  CompUpIntro $ ValueVarGlobal DD.cell A.arityS4

immediateS4 :: Value
immediateS4 = do
  ValueVarGlobal DD.imm A.arityS4

registerSigmaS4 ::
  Context ->
  DD.DefiniteDescription ->
  [Either Comp (Ident, Comp)] ->
  IO ()
registerSigmaS4 ctx name mxts = do
  let gContext = App.gensym (base ctx)
  registerSwitcher ctx name (sigmaT gContext mxts) (sigma4 gContext mxts)

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
  Gensym.Context ->
  [Either Comp (Ident, Comp)] ->
  Value ->
  IO Comp
sigmaT ctx mxts argVar = do
  xts <- mapM (supplyName ctx) mxts
  -- as == [APP-1, ..., APP-n]   (`a` here stands for `app`)
  as <- forM xts $ uncurry (toAffineApp ctx)
  ys <- mapM (const $ Gensym.newIdentFromText ctx "arg") xts
  body' <- linearize ctx xts $ bindLet (zip ys as) $ CompUpIntro $ ValueSigmaIntro []
  return $ CompSigmaElim False (map fst xts) argVar body'

-- (Assuming `ti` = `return di` for some `di` such that `xi : di`)
-- sigma4 NAME LOC [(x1, t1), ..., (xn, tn)]   ~>
--   update CompEnv with NAME ~> (thunk LAM), where LAM is:
--   lam z.
--     let-noetic (x1, ..., xn) := z in
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
  Gensym.Context ->
  [Either Comp (Ident, Comp)] ->
  Value ->
  IO Comp
sigma4 ctx mxts argVar = do
  xts <- mapM (supplyName ctx) mxts
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ uncurry (toRelevantApp ctx)
  (varNameList, varList) <- unzip <$> mapM (const $ Gensym.newValueVarLocalWith ctx "pair") xts
  body' <- linearize ctx xts $ bindLet (zip varNameList as) $ CompUpIntro $ ValueSigmaIntro varList
  return $ CompSigmaElim True (map fst xts) argVar body'

supplyName :: Gensym.Context -> Either b (Ident, b) -> IO (Ident, b)
supplyName ctx mName =
  case mName of
    Right (x, t) ->
      return (x, t)
    Left t -> do
      x <- Gensym.newIdentFromText ctx "unused-sigarg"
      return (x, t)

closureEnvS4 ::
  Context ->
  [Either Comp (Ident, Comp)] ->
  IO Value
closureEnvS4 ctx mxts =
  case mxts of
    [] ->
      return immediateS4 -- performance optimization; not necessary for correctness
    _ -> do
      let gContext = App.gensym (base ctx)
      i <- Gensym.newCount (App.gensym (base ctx))
      name <- Locator.attachCurrentLocator (App.locator (base ctx)) $ BN.sigmaName i
      registerSwitcher ctx name (sigmaT gContext mxts) (sigma4 gContext mxts)
      return $ ValueVarGlobal name A.arityS4
