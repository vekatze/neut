module Scene.Clarify.Sigma
  ( immediateS4,
    returnImmediateS4,
    closureEnvS4,
    returnClosureS4,
    returnCellS4,
    sigmaS4,
  )
where

import Context.App
import qualified Context.Gensym as Gensym
import qualified Context.Locator as Locator
import Control.Monad
import qualified Data.Text as T
import Entity.Comp
import Entity.Global
import Entity.Ident
import Scene.Clarify.Linearize
import Scene.Clarify.Utility

returnImmediateS4 :: Gensym.Context -> IO Comp
returnImmediateS4 ctx = do
  CompUpIntro <$> immediateS4 ctx

immediateS4 :: Gensym.Context -> IO Value
immediateS4 ctx = do
  let immediateT _ = return $ CompUpIntro $ ValueSigmaIntro []
  let immediate4 arg = return $ CompUpIntro arg
  tryCache cartImmName $ registerSwitcher ctx cartImmName immediateT immediate4

sigmaS4 ::
  Context ->
  Maybe T.Text ->
  [Either Comp (Ident, Comp)] ->
  IO Value
sigmaS4 ctx mName mxts = do
  let gContext = gensym ctx
  case mName of
    Nothing -> do
      i <- Gensym.newCount gContext
      -- name <- fmap wrapWithQuote $ attachSectionPrefix $ "sigma;" <> T.pack (show i)
      name <- fmap wrapWithQuote $ Locator.attachCurrentLocator (locator ctx) $ "sigma;" <> T.pack (show i)
      -- h <- wrapWithQuote <$> newText
      registerSwitcher gContext name (sigmaT gContext mxts) (sigma4 gContext mxts)
      return $ ValueVarGlobal name
    Just name ->
      tryCache name $ registerSwitcher gContext name (sigmaT gContext mxts) (sigma4 gContext mxts)

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
      immediateS4 (gensym ctx) -- performance optimization; not necessary for correctness
    _ ->
      sigmaS4 ctx Nothing mxts

returnClosureS4 :: Context -> IO Comp
returnClosureS4 ctx = do
  (env, envVar) <- Gensym.newValueVarLocalWith (gensym ctx) "env"
  retImmS4 <- returnImmediateS4 (gensym ctx)
  t <-
    sigmaS4
      ctx
      (Just cartClsName)
      [Right (env, retImmS4), Left (CompUpIntro envVar), Left retImmS4]
  return $ CompUpIntro t

returnCellS4 :: Context -> IO Comp
returnCellS4 ctx = do
  (env, envVar) <- Gensym.newValueVarLocalWith (gensym ctx) "env"
  retImmS4 <- returnImmediateS4 (gensym ctx)
  t <-
    sigmaS4
      ctx
      (Just cartCellName)
      [Right (env, retImmS4), Left (CompUpIntro envVar)] -- Sigma [A: tau, _: A]
  return $ CompUpIntro t
