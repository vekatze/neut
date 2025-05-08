module Kernel.Parse.Rule.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Aux.Logger.Rule.Hint
import Data.Text qualified as T
import Kernel.Parse.Rule.Layer
import Language.Common.Rule.Ident

type NominalEnv = [(T.Text, (Hint, Ident, Layer))]

empty :: NominalEnv
empty = []
