module Move.Context.AppM
  ( AppM,
    runAppM,
    liftMaybe,
  )
where

import Control.Monad.Trans.Except
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Throw qualified as Throw
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Remark qualified as Remark
import Rule.Remark qualified as R

type AppM =
  ExceptT [R.Remark] App

runAppM :: AppM a -> App (Either [R.Remark] a)
runAppM action = do
  h <- InitCompiler.new
  unitOrNone <- Throw.runEither (toApp $ InitCompiler.initializeCompiler h Remark.lspConfig)
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
