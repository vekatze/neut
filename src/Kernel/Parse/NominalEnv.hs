module Kernel.Parse.NominalEnv
  ( NominalEnv,
    NameEnv,
    empty,
    emptyNameEnv,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Kernel.Parse.Layer
import Kernel.Parse.Stage
import Language.Common.Ident
import Logger.Hint

type NominalEnv = [(T.Text, (Hint, Ident, Layer, Stage))]

type NameEnv = Map.HashMap T.Text (Hint, Ident, Layer, Stage)

empty :: NominalEnv
empty = []

emptyNameEnv :: NameEnv
emptyNameEnv = Map.empty
