module Rule.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Rule.Hint
import Rule.Ident
import Rule.Layer

type NominalEnv = [(T.Text, (Hint, Ident, Layer))]

empty :: NominalEnv
empty = []
