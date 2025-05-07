module Kernel.Parse.Move.Internal.Discern.PatternMatrix
  ( compilePatternMatrix,
    ensurePatternMatrixSanity,
  )
where

import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Vector qualified as V
import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.Locator qualified as Locator
import Kernel.Move.Context.Tag qualified as Tag
import Kernel.Parse.Move.Internal.Discern.Fallback qualified as PATF
import Kernel.Parse.Move.Internal.Discern.Handle qualified as H
import Kernel.Parse.Move.Internal.Discern.Noema
import Kernel.Parse.Move.Internal.Discern.Specialize qualified as PATS
import Kernel.Rule.Pattern qualified as PAT
import Kernel.Rule.Vector qualified as V
import Language.Common.Move.CreateSymbol qualified as Gensym
import Language.Common.Move.Raise (raiseError)
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.Binder
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.Ident
import Language.Common.Rule.Noema qualified as N
import Language.WeakTerm.Move.CreateHole qualified as WT
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Logger.Rule.Hint

-- This translation is based on:
--   https://dl.acm.org/doi/10.1145/1411304.1411311
compilePatternMatrix ::
  H.Handle ->
  N.IsNoetic ->
  V.Vector (Hint, Ident) ->
  PAT.PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  EIO (DT.DecisionTree WT.WeakTerm)
compilePatternMatrix h isNoetic occurrences mat =
  case PAT.unconsRow mat of
    Nothing ->
      return DT.Unreachable
    Just (row, _) ->
      case PAT.getClauseBody row of
        Right (usedVars, (freedVars, innerLetSeq, body)) -> do
          let occurrences' = map (\(mo, o) -> mo :< WT.Var o) $ V.toList occurrences
          cursorVars <- liftIO $ mapM (castToNoemaIfNecessary h isNoetic) occurrences'
          letSeq <- liftIO $ asLetSeq h $ zip usedVars cursorVars
          return $ DT.Leaf freedVars (letSeq ++ innerLetSeq) body
        Left (mCol, i) -> do
          if i > 0
            then do
              occurrences' <- liftEither $ V.swap mCol i occurrences
              mat' <- liftEither $ PAT.swapColumn mCol i mat
              compilePatternMatrix h isNoetic occurrences' mat'
            else do
              let headConstructors = PAT.getHeadConstructors mat
              let (mCursor, cursor) = V.head occurrences
              clauseList <- forM headConstructors $ \(mPat, specializer) -> do
                case specializer of
                  PAT.LiteralSpecializer literal -> do
                    let occurrences' = V.tail occurrences
                    specialMatrix <- PATS.specialize h isNoetic cursor specializer mat
                    cont <- compilePatternMatrix h isNoetic occurrences' specialMatrix
                    return $ DT.LiteralCase mPat literal cont
                  PAT.ConsSpecializer (PAT.ConsInfo {..}) -> do
                    let hGen = H.gensymHandle h
                    dataHoles <- liftIO $ mapM (const $ WT.createHole hGen mPat []) [1 .. AN.reify dataArgNum]
                    dataTypeHoles <- liftIO $ mapM (const $ WT.createHole hGen mPat []) [1 .. AN.reify dataArgNum]
                    consVars <- liftIO $ mapM (const $ Gensym.newIdentFromText hGen "cvar") [1 .. AN.reify consArgNum]
                    let ms = map fst args
                    let consVars' = zip ms consVars
                    (consArgs', h') <- liftIO $ alignConsArgs h consVars'
                    let occurrences' = V.fromList consVars' <> V.tail occurrences
                    specialMatrix <- PATS.specialize h isNoetic cursor specializer mat
                    specialDecisionTree <- compilePatternMatrix h' isNoetic occurrences' specialMatrix
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
              fallbackMatrix <- PATF.getFallbackMatrix h isNoetic cursor mat
              fallbackClause <- compilePatternMatrix h isNoetic (V.tail occurrences) fallbackMatrix
              t <- liftIO $ WT.createHole (H.gensymHandle h) mCursor []
              return $ DT.Switch (cursor, t) (fallbackClause, clauseList)

alignConsArgs ::
  H.Handle ->
  [(Hint, Ident)] ->
  IO ([BinderF WT.WeakTerm], H.Handle)
alignConsArgs h binder =
  case binder of
    [] -> do
      return ([], h)
    (mx, x) : xts -> do
      t <- WT.createHole (H.gensymHandle h) mx []
      let h' = H.extendWithoutInsert h mx x
      (xts', h'') <- alignConsArgs h' xts
      return ((mx, x, t) : xts', h'')

asLetSeq ::
  H.Handle ->
  [(Maybe (Hint, Ident), WT.WeakTerm)] ->
  IO [(BinderF WT.WeakTerm, WT.WeakTerm)]
asLetSeq h binder =
  case binder of
    [] ->
      return []
    (Nothing, _) : xes -> do
      asLetSeq h xes
    (Just (m, from), to) : xes -> do
      hole <- liftIO $ WT.createHole (H.gensymHandle h) m []
      cont' <- asLetSeq h xes
      return $ ((m, from, hole), to) : cont'

ensurePatternMatrixSanity :: H.Handle -> PAT.PatternMatrix a -> EIO ()
ensurePatternMatrixSanity h mat =
  case PAT.unconsRow mat of
    Nothing ->
      return ()
    Just (row, rest) -> do
      ensurePatternRowSanity h row
      ensurePatternMatrixSanity h rest

ensurePatternRowSanity :: H.Handle -> PAT.PatternRow a -> EIO ()
ensurePatternRowSanity h (patternVector, _) = do
  mapM_ (ensurePatternSanity h) $ V.toList patternVector

ensurePatternSanity :: H.Handle -> (Hint, PAT.Pattern) -> EIO ()
ensurePatternSanity h (m, pat) =
  case pat of
    PAT.Var v -> do
      liftIO $ Tag.insertBinder (H.tagHandle h) (m, v, ())
    PAT.Literal _ -> do
      return ()
    PAT.WildcardVar {} ->
      return ()
    PAT.Cons consInfo -> do
      let argNum = length (PAT.args consInfo)
      when (argNum /= AN.reify (PAT.consArgNum consInfo)) $ do
        let mainModule = Env.getMainModule (H.envHandle h)
        let consDD' = Locator.getReadableDD mainModule $ PAT.consDD consInfo
        raiseError m $
          "The constructor `"
            <> consDD'
            <> "` expects "
            <> T.pack (show (AN.reify (PAT.consArgNum consInfo)))
            <> " arguments, but found "
            <> T.pack (show argNum)
            <> "."
      mapM_ (ensurePatternSanity h) (PAT.args consInfo)
