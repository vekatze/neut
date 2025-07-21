module App.App (App) where

import App.Error qualified as E
import Control.Monad.Except (ExceptT)

type App = ExceptT E.Error IO
