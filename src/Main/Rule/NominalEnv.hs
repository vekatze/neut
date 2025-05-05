module Main.Rule.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Language.Common.Rule.Ident
import Logger.Rule.Hint
import Main.Rule.Layer

type NominalEnv = [(T.Text, (Hint, Ident, Layer))]

empty :: NominalEnv
empty = []
