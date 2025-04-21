module Scene.Parse.Discern.PatternMatrix
  ( compilePatternMatrix,
    ensurePatternMatrixSanity,
  )
where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Locator qualified as Locator
import Context.Tag qualified as Tag
import Context.Throw qualified as Throw
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Data.Text qualified as T
import Data.Vector qualified as V
import Rule.ArgNum qualified as AN
import Rule.Binder
import Rule.DecisionTree qualified as DT
import Rule.Hint
import Rule.Ident
import Rule.Layer
import Rule.Noema qualified as N
import Rule.NominalEnv
import Rule.Pattern qualified as PAT
import Rule.Vector qualified as V
import Rule.WeakTerm qualified as WT
import Scene.Parse.Discern.Fallback qualified as PATF
import Scene.Parse.Discern.Noema
import Scene.Parse.Discern.NominalEnv
import Scene.Parse.Discern.Specialize qualified as PATS

-- This translation is based on:
--   https://dl.acm.org/doi/10.1145/1411304.1411311
compilePatternMatrix ::
  Layer ->
  NominalEnv ->
  N.IsNoetic ->
  V.Vector (Hint, Ident) ->
  PAT.PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  App (DT.DecisionTree WT.WeakTerm)
compilePatternMatrix l nenv isNoetic occurrences mat =
  case PAT.unconsRow mat of
    Nothing ->
      return DT.Unreachable
    Just (row, _) ->
      case PAT.getClauseBody row of
        Right (usedVars, (freedVars, innerLetSeq, body)) -> do
          let occurrences' = map (\(mo, o) -> mo :< WT.Var o) $ V.toList occurrences
          cursorVars <- mapM (castToNoemaIfNecessary isNoetic) occurrences'
          letSeq <- asLetSeq $ zip usedVars cursorVars
          return $ DT.Leaf freedVars (letSeq ++ innerLetSeq) body
        Left (mCol, i) -> do
          if i > 0
            then do
              occurrences' <- Throw.liftEither $ V.swap mCol i occurrences
              mat' <- Throw.liftEither $ PAT.swapColumn mCol i mat
              compilePatternMatrix l nenv isNoetic occurrences' mat'
            else do
              let headConstructors = PAT.getHeadConstructors mat
              let (mCursor, cursor) = V.head occurrences
              clauseList <- forM headConstructors $ \(mPat, specializer) -> do
                case specializer of
                  PAT.LiteralSpecializer literal -> do
                    let occurrences' = V.tail occurrences
                    specialMatrix <- PATS.specialize isNoetic cursor specializer mat
                    cont <- compilePatternMatrix l nenv isNoetic occurrences' specialMatrix
                    return $ DT.LiteralCase mPat literal cont
                  PAT.ConsSpecializer (PAT.ConsInfo {..}) -> do
                    dataHoles <- mapM (const $ Gensym.newHole mPat []) [1 .. AN.reify dataArgNum]
                    dataTypeHoles <- mapM (const $ Gensym.newHole mPat []) [1 .. AN.reify dataArgNum]
                    consVars <- mapM (const $ Gensym.newIdentFromText "cvar") [1 .. AN.reify consArgNum]
                    let ms = map fst args
                    let consVars' = zip ms consVars
                    (consArgs', nenv') <- alignConsArgs l nenv consVars'
                    let occurrences' = V.fromList consVars' <> V.tail occurrences
                    specialMatrix <- PATS.specialize isNoetic cursor specializer mat
                    specialDecisionTree <- compilePatternMatrix l nenv' isNoetic occurrences' specialMatrix
                    let dataArgs' = zip dataHoles dataTypeHoles
                    return $
                      DT.ConsCase $
                        DT.ConsCaseRecord
                          { mCons = mPat,
                            consDD = consDD,
                            isConstLike = isConstLike,
                            disc = disc,
                            dataArgs = dataArgs',
                            consArgs = consArgs',
                            cont = specialDecisionTree
                          }
              fallbackMatrix <- PATF.getFallbackMatrix isNoetic cursor mat
              fallbackClause <- compilePatternMatrix l nenv isNoetic (V.tail occurrences) fallbackMatrix
              t <- Gensym.newHole mCursor []
              return $ DT.Switch (cursor, t) (fallbackClause, clauseList)

alignConsArgs ::
  Layer ->
  NominalEnv ->
  [(Hint, Ident)] ->
  App ([BinderF WT.WeakTerm], NominalEnv)
alignConsArgs l nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x) : xts -> do
      t <- Gensym.newHole mx []
      let nenv' = extendNominalEnvWithoutInsert mx x l nenv
      (xts', nenv'') <- alignConsArgs l nenv' xts
      return ((mx, x, t) : xts', nenv'')

asLetSeq ::
  [(Maybe (Hint, Ident), WT.WeakTerm)] ->
  App [(BinderF WT.WeakTerm, WT.WeakTerm)]
asLetSeq binder =
  case binder of
    [] ->
      return []
    (Nothing, _) : xes -> do
      asLetSeq xes
    (Just (m, from), to) : xes -> do
      h <- Gensym.newHole m []
      cont' <- asLetSeq xes
      return $ ((m, from, h), to) : cont'

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
    PAT.Var v -> do
      Tag.insertBinder (m, v, ())
    PAT.Literal _ -> do
      return ()
    PAT.WildcardVar {} ->
      return ()
    PAT.Cons consInfo -> do
      let argNum = length (PAT.args consInfo)
      when (argNum /= AN.reify (PAT.consArgNum consInfo)) $ do
        consDD' <- Locator.getReadableDD $ PAT.consDD consInfo
        Throw.raiseError m $
          "The constructor `"
            <> consDD'
            <> "` expects "
            <> T.pack (show (AN.reify (PAT.consArgNum consInfo)))
            <> " arguments, but found "
            <> T.pack (show argNum)
            <> "."
      mapM_ ensurePatternSanity (PAT.args consInfo)
