module Kernel.Rule.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Kernel.Rule.Layer
import Language.Common.Rule.Ident
import Logger.Rule.Hint

type NominalEnv = [(T.Text, (Hint, Ident, Layer))]

empty :: NominalEnv
empty = []
