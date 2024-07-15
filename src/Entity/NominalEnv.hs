module Entity.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Entity.Hint
import Entity.Ident
import Entity.Layer

type NominalEnv = [(T.Text, (Hint, Ident, Layer))]

empty :: NominalEnv
empty = []
