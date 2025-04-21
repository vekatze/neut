module Move.Context.AppM
  ( AppM,
    runAppM,
    liftMaybe,
  )
where

import Move.Context.App
import Move.Context.Throw qualified as Throw
import Control.Monad.Trans.Except
import Rule.Config.Remark qualified as Remark
import Rule.Remark qualified as R
import Move.Scene.Initialize qualified as Initialize

type AppM =
  ExceptT [R.Remark] App

runAppM :: AppM a -> App (Either [R.Remark] a)
runAppM action = do
  unitOrNone <- Throw.runEither (Initialize.initializeCompiler Remark.lspConfig)
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
