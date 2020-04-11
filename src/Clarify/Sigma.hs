{-# LANGUAGE OverloadedStrings #-}

module Clarify.Sigma
  ( cartesianSigma
  , returnArrayType
  , returnClosureType
  ) where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IntMap

import Clarify.Linearize
import Clarify.Utility
import Data.Basic
import Data.Code
import Data.Env

cartesianSigma ::
     Identifier
  -> Meta
  -> ArrayKind
  -> [Either CodePlus (Identifier, CodePlus)]
  -> WithEnv DataPlus
cartesianSigma (I (thetaName, i)) m k mxts = do
  cenv <- gets codeEnv
  let ident = asText' $ I ("cartesian-" <> thetaName, i)
  let theta = (m, DataConst ident)
  case Map.lookup ident cenv of
    Just _ -> return theta
    Nothing -> do
      (switchVarName, switchVar) <- newDataUpsilonWith m "switch"
      (argVarName, argVar) <- newDataUpsilonWith m "argsig"
      aff <- affineSigma argVar m k mxts
      rel <- relevantSigma argVar m k mxts
      insCodeEnv
        ident
        [switchVarName, argVarName]
        ( m
        , CodeEnumElim
            (IntMap.fromList [(asInt argVarName, argVar)])
            switchVar
            (switch aff rel))
      return theta

-- (Assuming `ti` = `return di` for some `di` such that `xi : di`)
-- affineSigma NAME LOC [(x1, t1), ..., (xn, tn)]   ~>
--   update CodeEnv with NAME ~> (thunk LAM), where LAM is:
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
affineSigma ::
     DataPlus
  -> Meta
  -> ArrayKind
  -> [Either CodePlus (Identifier, CodePlus)]
  -> WithEnv CodePlus
affineSigma argVar m k mxts = do
  xts <- mapM supplyName mxts
  -- as == [APP-1, ..., APP-n]   (`a` here stands for `app`)
  as <- forM xts $ \(x, t) -> toAffineApp m x t
  ys <- mapM (const $ newNameWith' "arg") xts
  let body = bindLet (zip ys as) (m, CodeUpIntro (m, sigmaIntro []))
  body' <- linearize xts body
  return (m, CodeSigmaElim k (map fst xts) argVar body')

-- (Assuming `ti` = `return di` for some `di` such that `xi : di`)
-- relevantSigma NAME LOC [(x1, t1), ..., (xn, tn)]   ~>
--   update CodeEnv with NAME ~> (thunk LAM), where LAM is:
--   lam z.
--     let (x1, ..., xn) := z in
--     <LINEARIZE_HEADER for x1, .., xn> in                                      ---
--     bind pair-1 :=                                                  ---       ---
--       bind f1 = t1 in              ---                              ---       ---
--       f1 @ (1, x1) in              ---  APP-1                       ---       ---
--     ...                                                             ---       ---
--     bind pair-n :=                                                  --- body  --- body'
--       bind fn = tn in              ---                              ---       ---
--       fn @ (1, xn) in              ---  APP-n                       ---       ---
--     let (p11, p12) := pair-1 in               ---                   ---       ---
--     ...                                       ---  TRANSPOSED-PAIR  ---       ---
--     let (pn1, pn2) := pair-n in               ---                   ---       ---
--     return ((p11, ..., pn1), (p12, ..., pn2)) ---                   ---       ---
relevantSigma ::
     DataPlus
  -> Meta
  -> ArrayKind
  -> [Either CodePlus (Identifier, CodePlus)]
  -> WithEnv CodePlus
relevantSigma argVar m k mxts = do
  xts <- mapM supplyName mxts
  -- as == [APP-1, ..., APP-n]
  as <- forM xts $ \(x, t) -> toRelevantApp m x t
  -- pairVarNameList == [pair-1, ...,  pair-n]
  (pairVarNameList, pairVarTypeList) <- unzip <$> mapM toPairInfo xts
  transposedPair <- transposeSigma m k pairVarTypeList
  let body = bindLet (zip pairVarNameList as) transposedPair
  body' <- linearize xts body
  return (m, CodeSigmaElim k (map fst xts) argVar body')

toPairInfo ::
     (Identifier, CodePlus) -> WithEnv (Identifier, (DataPlus, CodePlus))
toPairInfo (_, t@(m, _)) = do
  (name, var) <- newDataUpsilonWith m "pair"
  return (name, (var, t))

-- transposeSigma [d1, ..., dn] :=
--   let (x1, y1) := d1 in
--   ...
--   let (xn, yn) := dn in
--   return ((x1, ..., xn), (y1, ..., yn))
transposeSigma ::
     Meta -> ArrayKind -> [(DataPlus, CodePlus)] -> WithEnv CodePlus
transposeSigma m k ds = do
  (xList, xVarList) <- unzip <$> mapM (const $ newDataUpsilonWith m "sig-x") ds
  (yList, yVarList) <- unzip <$> mapM (const $ newDataUpsilonWith m "sig-y") ds
  return $
    bindSigmaElim (zip (zip xList yList) ds) $
    ( m
    , CodeUpIntro
        ( m
        , sigmaIntro
            [(m, DataSigmaIntro k xVarList), (m, DataSigmaIntro k yVarList)]))

bindSigmaElim ::
     [((Identifier, Identifier), (DataPlus, CodePlus))] -> CodePlus -> CodePlus
bindSigmaElim [] cont = cont
bindSigmaElim (((x, y), (d, _)):xyds) cont =
  (fst cont, sigmaElim [x, y] d $ bindSigmaElim xyds cont)

supplyName :: Either b (Identifier, b) -> WithEnv (Identifier, b)
supplyName (Right (x, t)) = return (x, t)
supplyName (Left t) = do
  x <- newNameWith' "unused-sigarg"
  return (x, t)

returnArrayType :: Meta -> WithEnv CodePlus
returnArrayType m = do
  (arr, arrVar) <- newDataUpsilonWith m "arr"
  retImmType <- returnCartesianImmediate m
  t <-
    cartesianSigma
      (I ("array-closure", 0))
      m
      arrVoidPtr
      [Right (arr, retImmType), Left (m, CodeUpIntro arrVar)]
  return (m, CodeUpIntro t)

returnClosureType :: Meta -> WithEnv CodePlus
returnClosureType m = do
  (env, envVar) <- newDataUpsilonWith m "env"
  retImmType <- returnCartesianImmediate m
  t <-
    cartesianSigma
      (I ("closure", 0))
      m
      arrVoidPtr
      [Right (env, retImmType), Left (m, CodeUpIntro envVar), Left retImmType]
  return (m, CodeUpIntro t)
