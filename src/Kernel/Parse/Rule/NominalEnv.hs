module Kernel.Parse.Rule.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Kernel.Parse.Rule.Layer
import Language.Common.Rule.Ident
import Logger.Rule.Hint

type NominalEnv = [(T.Text, (Hint, Ident, Layer))]

empty :: NominalEnv
empty = []
