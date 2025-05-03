module Main.Rule.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Main.Rule.Hint
import Main.Rule.Ident
import Main.Rule.Layer

type NominalEnv = [(T.Text, (Hint, Ident, Layer))]

empty :: NominalEnv
empty = []
