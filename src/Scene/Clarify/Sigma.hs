module Scene.Clarify.Sigma
  ( immediateS4,
    returnImmediateS4,
    closureEnvS4,
    returnClosureS4,
    returnCellS4,
    sigmaS4,
  )
where

import Control.Monad
import qualified Data.Text as T
import Entity.Basic
import Entity.Comp
import Entity.Global
import Entity.Namespace
import Scene.Clarify.Linearize
import Scene.Clarify.Utility

returnImmediateS4 :: IO Comp
returnImmediateS4 = do
  CompUpIntro <$> immediateS4

immediateS4 :: IO Value
immediateS4 = do
  let immediateT _ = return $ CompUpIntro $ ValueSigmaIntro []
  let immediate4 arg = return $ CompUpIntro arg
  tryCache cartImmName $ registerSwitcher cartImmName immediateT immediate4

sigmaS4 ::
  Maybe T.Text ->
  [Either Comp (Ident, Comp)] ->
  IO Value
sigmaS4 mName mxts =
  case mName of
    Nothing -> do
      i <- newCount
      name <- fmap wrapWithQuote $ attachSectionPrefix $ "sigma;" <> T.pack (show i)
      -- h <- wrapWithQuote <$> newText
      registerSwitcher name (sigmaT mxts) (sigma4 mxts)
      return $ ValueVarGlobal name
    Just name ->
      tryCache name $ registerSwitcher name (sigmaT mxts) (sigma4 mxts)

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
  [Either Comp (Ident, Comp)] ->
  Value ->
  IO Comp
sigmaT mxts argVar = do
  xts <- mapM supplyName mxts
  -- as == [APP-1, ..., APP-n]   (`a` here stands for `app`)
  as <- forM xts $ uncurry toAffineApp
  ys <- mapM (const $ newIdentFromText "arg") xts
  body' <- linearize xts $ bindLet (zip ys as) $ CompUpIntro $ ValueSigmaIntro []
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
  [Either Comp (Ident, Comp)] ->
  Value ->
  IO Comp
sigma4 mxts argVar = do
  xts <- mapM supplyName mxts
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ uncurry toRelevantApp
  (varNameList, varList) <- unzip <$> mapM (const $ newValueVarLocalWith "pair") xts
  body' <- linearize xts $ bindLet (zip varNameList as) $ CompUpIntro $ ValueSigmaIntro varList
  return $ CompSigmaElim True (map fst xts) argVar body'

supplyName :: Either b (Ident, b) -> IO (Ident, b)
supplyName mName =
  case mName of
    Right (x, t) ->
      return (x, t)
    Left t -> do
      x <- newIdentFromText "unused-sigarg"
      return (x, t)

closureEnvS4 ::
  [Either Comp (Ident, Comp)] ->
  IO Value
closureEnvS4 mxts =
  case mxts of
    [] ->
      immediateS4 -- performance optimization; not necessary for correctness
    _ ->
      sigmaS4 Nothing mxts

returnClosureS4 :: IO Comp
returnClosureS4 = do
  (env, envVar) <- newValueVarLocalWith "env"
  retImmS4 <- returnImmediateS4
  t <-
    sigmaS4
      (Just cartClsName)
      [Right (env, retImmS4), Left (CompUpIntro envVar), Left retImmS4]
  return $ CompUpIntro t

returnCellS4 :: IO Comp
returnCellS4 = do
  (env, envVar) <- newValueVarLocalWith "env"
  retImmS4 <- returnImmediateS4
  t <-
    sigmaS4
      (Just cartCellName)
      [Right (env, retImmS4), Left (CompUpIntro envVar)] -- Sigma [A: tau, _: A]
  return $ CompUpIntro t
