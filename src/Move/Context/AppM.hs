module Move.Context.AppM
  ( AppM,
    runAppM,
    liftMaybe,
    liftEIO,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except
import Move.Context.App
import Move.Context.EIO (EIO, runEIO, toApp)
import Move.Context.Throw qualified as Throw
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Remark qualified as Remark
import Rule.Error qualified as E
import Rule.Remark qualified as R

type AppM =
  ExceptT [R.Remark] App

runAppM :: Gensym.Handle -> AppM a -> App (Either [R.Remark] a)
runAppM gensymHandle action = do
  h <- InitCompiler.new gensymHandle
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

liftEIO :: EIO a -> AppM a
liftEIO m = do
  val <- liftIO $ runEIO m
  case val of
    Left (E.MakeError vs) ->
      except $ Left vs
    Right v ->
      return v
