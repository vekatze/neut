module Entity.NominalEnv
  ( NominalEnv,
    empty,
  )
where

import Data.Text qualified as T
import Entity.Hint
import Entity.Ident

type NominalEnv = [(T.Text, (Hint, Ident))]

empty :: NominalEnv
empty = []
