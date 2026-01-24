module Kernel.Parse.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Kernel.Parse.Layer
import Kernel.Parse.Stage
import Language.Common.Ident
import Logger.Hint

type NominalEnv = [(T.Text, (Hint, Ident, Layer, Stage))]

empty :: NominalEnv
empty = []
