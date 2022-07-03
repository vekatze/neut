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

returnImmediateS4 :: Gensym.Axis -> IO Comp
returnImmediateS4 axis = do
  CompUpIntro <$> immediateS4 axis

immediateS4 :: Gensym.Axis -> IO Value
immediateS4 axis = do
  let immediateT _ = return $ CompUpIntro $ ValueSigmaIntro []
  let immediate4 arg = return $ CompUpIntro arg
  tryCache cartImmName $ registerSwitcher axis cartImmName immediateT immediate4

sigmaS4 ::
  Axis ->
  Maybe T.Text ->
  [Either Comp (Ident, Comp)] ->
  IO Value
sigmaS4 axis mName mxts = do
  let gAxis = gensym axis
  case mName of
    Nothing -> do
      i <- Gensym.newCount gAxis
      -- name <- fmap wrapWithQuote $ attachSectionPrefix $ "sigma;" <> T.pack (show i)
      name <- fmap wrapWithQuote $ Locator.attachCurrentLocator (locator axis) $ "sigma;" <> T.pack (show i)
      -- h <- wrapWithQuote <$> newText
      registerSwitcher gAxis name (sigmaT gAxis mxts) (sigma4 gAxis mxts)
      return $ ValueVarGlobal name
    Just name ->
      tryCache name $ registerSwitcher gAxis name (sigmaT gAxis mxts) (sigma4 gAxis mxts)

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
  Gensym.Axis ->
  [Either Comp (Ident, Comp)] ->
  Value ->
  IO Comp
sigmaT axis mxts argVar = do
  xts <- mapM (supplyName axis) mxts
  -- as == [APP-1, ..., APP-n]   (`a` here stands for `app`)
  as <- forM xts $ uncurry (toAffineApp axis)
  ys <- mapM (const $ Gensym.newIdentFromText axis "arg") xts
  body' <- linearize axis xts $ bindLet (zip ys as) $ CompUpIntro $ ValueSigmaIntro []
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
  Gensym.Axis ->
  [Either Comp (Ident, Comp)] ->
  Value ->
  IO Comp
sigma4 axis mxts argVar = do
  xts <- mapM (supplyName axis) mxts
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ uncurry (toRelevantApp axis)
  (varNameList, varList) <- unzip <$> mapM (const $ Gensym.newValueVarLocalWith axis "pair") xts
  body' <- linearize axis xts $ bindLet (zip varNameList as) $ CompUpIntro $ ValueSigmaIntro varList
  return $ CompSigmaElim True (map fst xts) argVar body'

supplyName :: Gensym.Axis -> Either b (Ident, b) -> IO (Ident, b)
supplyName axis mName =
  case mName of
    Right (x, t) ->
      return (x, t)
    Left t -> do
      x <- Gensym.newIdentFromText axis "unused-sigarg"
      return (x, t)

closureEnvS4 ::
  Axis ->
  [Either Comp (Ident, Comp)] ->
  IO Value
closureEnvS4 axis mxts =
  case mxts of
    [] ->
      immediateS4 (gensym axis) -- performance optimization; not necessary for correctness
    _ ->
      sigmaS4 axis Nothing mxts

returnClosureS4 :: Axis -> IO Comp
returnClosureS4 axis = do
  (env, envVar) <- Gensym.newValueVarLocalWith (gensym axis) "env"
  retImmS4 <- returnImmediateS4 (gensym axis)
  t <-
    sigmaS4
      axis
      (Just cartClsName)
      [Right (env, retImmS4), Left (CompUpIntro envVar), Left retImmS4]
  return $ CompUpIntro t

returnCellS4 :: Axis -> IO Comp
returnCellS4 axis = do
  (env, envVar) <- Gensym.newValueVarLocalWith (gensym axis) "env"
  retImmS4 <- returnImmediateS4 (gensym axis)
  t <-
    sigmaS4
      axis
      (Just cartCellName)
      [Right (env, retImmS4), Left (CompUpIntro envVar)] -- Sigma [A: tau, _: A]
  return $ CompUpIntro t
