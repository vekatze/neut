module Kernel.Parse.Internal.Discern.PatternMatrix
  ( compilePatternMatrix,
    ensurePatternMatrixSanity,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree hiding (section)
import Control.Monad
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Vector qualified as V
import Kernel.Common.Handle.Global.ModulePath (renderDD)
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Parse.Internal.Discern.Fallback qualified as PATF
import Kernel.Parse.Internal.Discern.Handle qualified as H
import Kernel.Parse.Internal.Discern.Noema
import Kernel.Parse.Internal.Discern.Specialize qualified as PATS
import Kernel.Parse.Pattern qualified as PAT
import Kernel.Parse.Vector qualified as V
import Language.Common.ArgNum qualified as AN
import Language.Common.Binder
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.DecisionTree qualified as DT
import Language.Common.Ident
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.Noema qualified as N
import Language.Common.VarKind qualified as VK
import Language.WeakTerm.CreateHole qualified as WT
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

-- This translation is based on:
--   https://dl.acm.org/doi/10.1145/1411304.1411311
compilePatternMatrix ::
  H.Handle ->
  N.IsNoetic ->
  V.Vector (Hint, Ident) ->
  PAT.PatternMatrix ([Ident], [(BinderF WT.WeakType, WT.WeakTerm)], WT.WeakTerm) ->
  App (DT.DecisionTree WT.WeakType WT.WeakTerm)
compilePatternMatrix h isNoetic occurrences mat = do
  consCaseVarsRef <- liftIO $ newIORef Map.empty
  compilePatternMatrix' h consCaseVarsRef isNoetic occurrences mat

data ConsCaseVars = ConsCaseVars
  { consImpVars :: [Ident],
    consVars :: [Ident]
  }

type ConsCaseVarsRef =
  IORef (Map.HashMap (Int, DD.DefiniteDescription) ConsCaseVars)

getConsCaseVars :: H.Handle -> ConsCaseVarsRef -> Ident -> PAT.ConsInfo -> IO ConsCaseVars
getConsCaseVars h consCaseVarsRef cursor (PAT.ConsInfo {consDD, dataArgNum, consArgNum, consImpArgNames, typeParams}) = do
  let key = (Ident.toInt cursor, consDD)
  known <- readIORef consCaseVarsRef
  case Map.lookup key known of
    Just vars ->
      return vars
    Nothing -> do
      let consImpParams = drop (AN.reify dataArgNum) typeParams
      let consImpNames = zipWith (\declared named -> maybe declared (Ident.toText . snd) named) consImpArgNames consImpParams
      consImpVars <- mapM (Gensym.newIdentFromText (H.gensymHandle h)) consImpNames
      consVars <- mapM (const $ Gensym.newIdentFromText (H.gensymHandle h) "cvar") [1 .. AN.reify consArgNum]
      let vars = ConsCaseVars {consImpVars, consVars}
      modifyIORef' consCaseVarsRef $ Map.insert key vars
      return vars

compilePatternMatrix' ::
  H.Handle ->
  ConsCaseVarsRef ->
  N.IsNoetic ->
  V.Vector (Hint, Ident) ->
  PAT.PatternMatrix ([Ident], [(BinderF WT.WeakType, WT.WeakTerm)], WT.WeakTerm) ->
  App (DT.DecisionTree WT.WeakType WT.WeakTerm)
compilePatternMatrix' h consCaseVarsRef isNoetic occurrences mat =
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
              compilePatternMatrix' h consCaseVarsRef isNoetic occurrences' mat'
            else do
              let headConstructors = PAT.getHeadConstructors mat
              let (mCursor, cursor) = V.head occurrences
              clauseList <- forM headConstructors $ \(mPat, specializer) -> do
                case specializer of
                  PAT.LiteralSpecializer literal -> do
                    let occurrences' = V.tail occurrences
                    specialMatrix <- PATS.specialize h isNoetic cursor [] specializer mat
                    cont <- compilePatternMatrix' h consCaseVarsRef isNoetic occurrences' specialMatrix
                    return $ DT.LiteralCase mPat literal cont
                  PAT.ConsSpecializer consInfo@(PAT.ConsInfo {..}) -> do
                    let hGen = H.gensymHandle h
                    dataHoles <- liftIO $ mapM (const $ WT.createTypeHole hGen mPat []) [1 .. AN.reify dataArgNum]
                    dataTypeHoles <- liftIO $ mapM (const $ WT.createTypeHole hGen mPat []) [1 .. AN.reify dataArgNum]
                    ConsCaseVars {consImpVars, consVars} <- liftIO $ getConsCaseVars h consCaseVarsRef cursor consInfo
                    let consImpArgs = map (\x -> (mPat, VK.normal, x, mPat :< WT.Tau)) consImpVars
                    let typeArgs = dataHoles ++ map (\x -> mPat :< WT.TVar x) consImpVars
                    let ms = map fst args
                    let consVars' = zip ms consVars
                    (consArgs', h') <- liftIO $ alignConsArgs h consVars'
                    let occurrences' = V.fromList consVars' <> V.tail occurrences
                    specialMatrix <- PATS.specialize h isNoetic cursor typeArgs specializer mat
                    specialDecisionTree <- compilePatternMatrix' h' consCaseVarsRef isNoetic occurrences' specialMatrix
                    let dataArgs' = zip dataHoles dataTypeHoles
                    return $
                      DT.ConsCase $
                        DT.ConsCaseRecord
                          { mCons = mPat,
                            consDD = consDD,
                            isConstLike = isConstLike,
                            disc = disc,
                            dataArgs = dataArgs',
                            consImpArgs = consImpArgs,
                            consArgs = consArgs',
                            cont = specialDecisionTree
                          }
              fallbackMatrix <- PATF.getFallbackMatrix h isNoetic cursor mat
              fallbackClause <- compilePatternMatrix' h consCaseVarsRef isNoetic (V.tail occurrences) fallbackMatrix
              t <- liftIO $ WT.createTypeHole (H.gensymHandle h) mCursor []
              return $ DT.Switch (cursor, t) (fallbackClause, clauseList)

alignConsArgs ::
  H.Handle ->
  [(Hint, Ident)] ->
  IO ([BinderF WT.WeakType], H.Handle)
alignConsArgs h binder =
  case binder of
    [] -> do
      return ([], h)
    (mx, x) : xts -> do
      t <- WT.createTypeHole (H.gensymHandle h) mx []
      let h' = H.extendWithoutInsert h mx x
      (xts', h'') <- alignConsArgs h' xts
      return ((mx, VK.normal, x, t) : xts', h'')

asLetSeq ::
  H.Handle ->
  [(Maybe (Hint, VK.VarKind, Ident), WT.WeakTerm)] ->
  IO [(BinderF WT.WeakType, WT.WeakTerm)]
asLetSeq h binder =
  case binder of
    [] ->
      return []
    (Nothing, _) : xes -> do
      asLetSeq h xes
    (Just (m, k, from), to) : xes -> do
      hole <- liftIO $ WT.createTypeHole (H.gensymHandle h) m []
      cont' <- asLetSeq h xes
      return $ ((m, k, from, hole), to) : cont'

ensurePatternMatrixSanity :: H.Handle -> PAT.PatternMatrix a -> App ()
ensurePatternMatrixSanity h mat =
  case PAT.unconsRow mat of
    Nothing ->
      return ()
    Just (row, rest) -> do
      ensurePatternRowSanity h row
      ensurePatternMatrixSanity h rest

ensurePatternRowSanity :: H.Handle -> PAT.PatternRow a -> App ()
ensurePatternRowSanity h (patternVector, _) = do
  mapM_ (ensurePatternSanity h) $ V.toList patternVector

ensurePatternSanity :: H.Handle -> (Hint, PAT.Pattern) -> App ()
ensurePatternSanity h (m, pat) =
  case pat of
    PAT.Var k v -> do
      liftIO $ Tag.insertBinder (H.tagHandle h) (m, k, v, ())
    PAT.Literal _ -> do
      return ()
    PAT.WildcardVar {} ->
      return ()
    PAT.Cons consInfo -> do
      forM_ (catMaybes (PAT.typeParams consInfo)) $ \(mx, x) ->
        liftIO $ Tag.insertBinder (H.tagHandle h) (mx, VK.normal, x, ())
      let argNum = length (PAT.args consInfo)
      when (argNum /= AN.reify (PAT.consArgNum consInfo)) $ do
        let consDD' = renderDD (H.modulePathMap h) $ PAT.consDD consInfo
        raiseError m $
          "The constructor `"
            <> consDD'
            <> "` expects "
            <> T.pack (show (AN.reify (PAT.consArgNum consInfo)))
            <> " arguments, but found "
            <> T.pack (show argNum)
            <> "."
      mapM_ (ensurePatternSanity h) (PAT.args consInfo)
