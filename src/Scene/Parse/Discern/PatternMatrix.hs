{-# LANGUAGE OverloadedRecordDot #-}

module Scene.Parse.Discern.PatternMatrix
  ( compilePatternMatrix,
    ensurePatternMatrixSanity,
  )
where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Data.Text qualified as T
import Data.Vector qualified as V
import Entity.ArgNum qualified as AN
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident
import Entity.Noema qualified as N
import Entity.NominalEnv
import Entity.Pattern qualified as PAT
import Entity.Vector qualified as V
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Fallback qualified as PATF
import Scene.Parse.Discern.Noema
import Scene.Parse.Discern.NominalEnv
import Scene.Parse.Discern.Specialize qualified as PATS

-- This translation is based on:
--   https://dl.acm.org/doi/10.1145/1411304.1411311
compilePatternMatrix ::
  NominalEnv ->
  N.IsNoetic ->
  Hint ->
  V.Vector Ident ->
  PAT.PatternMatrix ([Ident], WT.WeakTerm) ->
  App (DT.DecisionTree WT.WeakTerm)
compilePatternMatrix nenv isNoetic m occurrences mat =
  case PAT.unconsRow mat of
    Nothing ->
      return DT.Unreachable
    Just (row, _) ->
      case PAT.getClauseBody row of
        Right (usedVars, (freedVars, body)) -> do
          let occurrences' = map (\o -> m :< WT.Var o) $ V.toList occurrences
          cursorVars <- mapM (castToNoemaIfNecessary isNoetic) occurrences'
          DT.Leaf freedVars <$> bindLet (zip usedVars cursorVars) body
        Left (mCol, i) -> do
          if i > 0
            then do
              occurrences' <- Throw.liftEither $ V.swap mCol i occurrences
              mat' <- Throw.liftEither $ PAT.swapColumn mCol i mat
              compilePatternMatrix nenv isNoetic mCol occurrences' mat'
            else do
              let headConstructors = PAT.getHeadConstructors mat
              let cursor = V.head occurrences
              clauseList <- forM headConstructors $ \(mPat, consInfo) -> do
                dataHoles <- mapM (const $ Gensym.newHole mPat []) [1 .. AN.reify consInfo.dataArgNum]
                dataTypeHoles <- mapM (const $ Gensym.newHole mPat []) [1 .. AN.reify consInfo.dataArgNum]
                consVars <- mapM (const $ Gensym.newIdentFromText "cvar") [1 .. AN.reify consInfo.consArgNum]
                let ms = map fst consInfo.args
                (consArgs', nenv') <- alignConsArgs nenv $ zip ms consVars
                let occurrences' = V.fromList consVars <> V.tail occurrences
                specialMatrix <- PATS.specialize isNoetic cursor (consInfo.consDD, consInfo.consArgNum) mat
                specialDecisionTree <- compilePatternMatrix nenv' isNoetic mPat occurrences' specialMatrix
                let dataArgs' = zip dataHoles dataTypeHoles
                return $
                  DT.Case
                    { mCons = mPat,
                      consDD = consInfo.consDD,
                      isConstLike = consInfo.isConstLike,
                      disc = consInfo.disc,
                      dataArgs = dataArgs',
                      consArgs = consArgs',
                      cont = specialDecisionTree
                    }
              fallbackMatrix <- PATF.getFallbackMatrix isNoetic cursor mat
              fallbackClause <- compilePatternMatrix nenv isNoetic mCol (V.tail occurrences) fallbackMatrix
              t <- Gensym.newHole mCol []
              return $ DT.Switch (cursor, t) (fallbackClause, clauseList)

alignConsArgs ::
  NominalEnv ->
  [(Hint, Ident)] ->
  App ([BinderF WT.WeakTerm], NominalEnv)
alignConsArgs nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x) : xts -> do
      t <- Gensym.newHole mx []
      let nenv' = extendNominalEnvWithoutInsert mx x nenv
      (xts', nenv'') <- alignConsArgs nenv' xts
      return ((mx, x, t) : xts', nenv'')

bindLet ::
  [(Maybe (Hint, Ident), WT.WeakTerm)] ->
  WT.WeakTerm ->
  App WT.WeakTerm
bindLet binder cont =
  case binder of
    [] ->
      return cont
    (Nothing, _) : xes -> do
      bindLet xes cont
    (Just (m, from), to) : xes -> do
      h <- Gensym.newHole m []
      cont' <- bindLet xes cont
      return $ m :< WT.Let WT.Transparent (m, from, h) to cont'

ensurePatternMatrixSanity :: PAT.PatternMatrix a -> App ()
ensurePatternMatrixSanity mat =
  case PAT.unconsRow mat of
    Nothing ->
      return ()
    Just (row, rest) -> do
      ensurePatternRowSanity row
      ensurePatternMatrixSanity rest

ensurePatternRowSanity :: PAT.PatternRow a -> App ()
ensurePatternRowSanity (patternVector, _) = do
  mapM_ ensurePatternSanity $ V.toList patternVector

ensurePatternSanity :: (Hint, PAT.Pattern) -> App ()
ensurePatternSanity (m, pat) =
  case pat of
    PAT.Var {} ->
      return ()
    PAT.WildcardVar {} ->
      return ()
    PAT.Cons consInfo -> do
      let argNum = length (PAT.args consInfo)
      when (argNum /= AN.reify (PAT.consArgNum consInfo)) $
        Throw.raiseError m $
          "the constructor `"
            <> DD.reify (PAT.consDD consInfo)
            <> "` expects "
            <> T.pack (show (AN.reify (PAT.consArgNum consInfo)))
            <> " arguments, but found "
            <> T.pack (show argNum)
            <> "."
      mapM_ ensurePatternSanity (PAT.args consInfo)
