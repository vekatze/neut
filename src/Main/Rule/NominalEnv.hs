module Main.Rule.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Language.Common.Rule.Hint
import Language.Common.Rule.Ident
import Main.Rule.Layer

type NominalEnv = [(T.Text, (Hint, Ident, Layer))]

empty :: NominalEnv
empty = []
