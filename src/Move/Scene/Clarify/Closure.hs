module Move.Scene.Clarify.Closure
  (
  -- Handle (..),
  -- closureEnvS4,
  )
where

-- import Control.Monad
-- import Control.Monad.IO.Class (MonadIO (liftIO))
-- import Move.Context.Locator qualified as Locator
-- import Move.Language.Utility.Gensym qualified as Gensym
-- import Move.Scene.Clarify.Linearize qualified as Linearize
-- import Move.Scene.Clarify.Sigma qualified as Sigma
-- import Move.Scene.Clarify.Utility qualified as Utility
-- import Rule.ArgNum qualified as AN
-- import Rule.BaseLowType qualified as BLT
-- import Rule.BaseName qualified as BN
-- import Rule.Comp qualified as C
-- import Rule.DefiniteDescription qualified as DD
-- import Rule.Discriminant qualified as D
-- import Rule.EnumCase qualified as EC
-- import Rule.Ident
-- import Rule.Magic qualified as M
-- import Rule.Opacity qualified as O

-- data Handle
--   = Handle
--   { sigmaHandle :: Sigma.Handle,
--     locatorHandle :: Locator.Handle
--   }

-- closureEnvS4 ::
--   Handle ->
--   [Either C.Comp (Ident, C.Comp)] ->
--   IO C.Value
-- closureEnvS4 h mxts =
--   case mxts of
--     [] ->
--       return Sigma.immediateS4 -- performance optimization; not necessary for correctness
--     _ -> do
--       let h' = sigmaHandle h
--       i <- Gensym.newCount (Sigma.gensymHandle h')
--       name <- liftIO $ Locator.attachCurrentLocator (locatorHandle h) $ BN.sigmaName i
--       liftIO $ Utility.registerSwitcher (Sigma.utilityHandle h') O.Clear name (Sigma.sigmaT h' mxts) (Sima.sigma4 h' mxts)
--       return $ C.VarGlobal name AN.argNumS4
