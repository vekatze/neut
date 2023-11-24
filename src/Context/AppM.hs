module Context.AppM
  ( AppM,
    runAppM,
    liftMaybe,
  )
where

import Context.App
import Control.Monad.Trans.Maybe

type AppM =
  MaybeT App

runAppM :: AppM a -> App (Maybe a)
runAppM =
  runMaybeT

liftMaybe :: Maybe a -> AppM a
liftMaybe =
  MaybeT . pure
