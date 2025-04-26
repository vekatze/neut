module Move.Context.AppM
  ( AppM,
    runAppM,
    liftMaybe,
  )
where

import Control.Monad.Trans.Except
import Move.Context.App
import Move.Context.Throw qualified as Throw
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Rule.Config.Remark qualified as Remark
import Rule.Remark qualified as R

type AppM =
  ExceptT [R.Remark] App

runAppM :: AppM a -> App (Either [R.Remark] a)
runAppM action = do
  hm <- ModuleReflect.new
  unitOrNone <- Throw.runEither (Initialize.initializeCompiler hm Remark.lspConfig)
  case unitOrNone of
    Right _ ->
      runExceptT action
    Left remarks ->
      return $ Left remarks

liftMaybe :: Maybe a -> AppM a
liftMaybe m =
  case m of
    Nothing ->
      except $ Left []
    Just v ->
      return v
